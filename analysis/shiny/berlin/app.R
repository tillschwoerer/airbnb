library(shiny)
library(shinydashboard)
library(tidyverse)
library(sf)
library(tmap)
library(DT)
library(shinyWidgets)

DT_options <- list(pageLength = 5, lengthMenu = c(5, 10, 15, 20), dom="tpl", scrollX = TRUE, autoWidth = TRUE)

load("data/prepared_data.Rdat")
#df <- df %>% st_transform(3035)
vars <- c("listing_url", "price", "size", "neighbourhood_group_cleansed",
          "property_type", "room_type", "accommodates","wifi", "tv",
          "pool", "altbau", "cooking_basics", "bed_linens", "parking", "garden")

variables <- c("n", "price", "size", "accommodates", ".resid")

district_list <- unique(df$name)
amenities <- c("restaurants", "attractions")
max_price <- max(df$price, na.rm=TRUE)
max_accomodates <- max(df$accommodates)

model3 <- lm(price ~  beds  + room_type + accommodates + tv + bed_linens + parking + garden + coffeemaker + pool + bathtub + cooking_basics + gym + altbau + n_restaurants + n_attractions + stop_dist, data = df)
xvars <- all.vars(formula(model3)[[3]])
df_augmented <- broom::augment_columns(model3, data = df)
neighbourhood_list <- unique(df_augmented$neighbourhood_group_cleansed)


### Aggregates
aggregated <- districts %>% inner_join(
  #########
  (df_augmented %>% 
    group_by(name) %>% 
    summarise(n = n(),
              price = round(mean(price, na.rm=TRUE)),
              size = round(mean(size, na.rm=TRUE)),
              accommodates = round(mean(accommodates, na.rm=TRUE)),
              pool = round(mean(pool, na.rm=TRUE)),
              stop_distance = round(mean(stop_dist, na.rm=TRUE)),
              .resid = round(mean(.resid, na.rm=TRUE))
    )
  )
  #########
  , by = "name") 


ui <- dashboardPage(
  dashboardHeader(title = "Berlin Holiday Planner"),
  dashboardSidebar(
    sidebarMenu(
      menuItem(text = "Districts", tabName = "districts"),
      menuItem(text = "Radius Search", tabName = "radius"),
      menuItem(text = 'Recommendation', tabName = 'recommendation', selected = TRUE)
    )  
  ),
    
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "districts",
        fluidRow(
          box(
            width =  12,
            selectInput("var", "Variable", variables)
          ),
          box(
            width = 12,
            tmapOutput('districts')
          )
        )
      ),
      tabItem(
        tabName = "radius",
        fluidRow(
          box(
            width = 12,
            collapsible = TRUE,
            column(width = 6, selectInput("district", "District", district_list)),
            tmapOutput("single_object")
          )
        )
      ),
      tabItem(
        tabName = 'recommendation',
        fluidRow(
          box(
            width = 12,
            collapsible = TRUE,
            column(
              width = 4, 
              sliderInput('price', 'Price range', min = 0, max = max_price, 
                          value = c(0, max_price))
            ),
            column(
              width = 4,
              sliderInput('accommodates', 'Number of people', min = 1, step = 1, 
                          max = max_accomodates, value = 2)
            ),
            column(
              width = 4,
              pickerInput(
                inputId = "rec_districts",
                label = "Districts", 
                choices = neighbourhood_list,
                selected = neighbourhood_list,
                options = list(
                  `actions-box` = TRUE), 
                multiple = TRUE
              )
            ),
            actionButton('refresh', 'Refresh', icon = icon('refresh')),
            DT::dataTableOutput('data'),
            box(
              width = 12,
              collapsible = TRUE,
              column(width = 6, uiOutput('image' )),
              column(width = 6, uiOutput('listing_url'), br(), textOutput('text')),
            )
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  output$districts <- renderTmap({
    req(input$var)
    aggregated %>% 
      tm_shape() +
      tm_fill(input$var, n = 10, convert2density = FALSE) +
      tm_borders() +
      tm_text(text = input$var, size = 0.7)
  })
  
  output$single_object <- renderTmap({
    req(input$district)
    obj <- df %>% 
      filter(name == input$district) %>% 
      slice_sample(n=1) %>% 
      select(vars)
    obj_300 <- obj %>% st_buffer(dist = 300) 
    obj_1000 <- obj_300 %>% st_buffer(dist = 700) 
        
    tmap_mode("view")
    tm_shape(obj, bbox = st_bbox(obj_1000)) +
      tm_dots(size = 0.3) + 
      tm_shape(obj_300) + tm_fill("green", alpha = 0.3, zindex = 401) + 
      tm_shape(obj_1000) + tm_fill("yellow", alpha = 0.3, zindex = 402) +
      tm_shape(restaurants) + tm_dots("red") +
      tm_shape(attractions) + tm_dots("blue") +
      tm_shape(districts) + tm_borders(lwd = 2)
        
  })
    
  filtered <- reactive({
    input$refresh
    isolate({
      input_price <- input$price
      input_accommodates <- input$accommodates
      input_districts <- input$rec_districts
    })
    df_augmented %>% 
      filter(price >= input_price[1], price <= input_price[2]) %>% 
      filter(accommodates >= input_accommodates, accommodates <= (input_accommodates + 2)) %>%
      filter(name %in% input_districts) %>%
      arrange(.resid) %>%
      filter(.resid > -25)
  })

  output$data <- DT::renderDataTable({
    filtered() %>%
    select(listing_name, district = neighbourhood_group_cleansed, price, .fitted, .resid, 
           accommodates, room_type, beds, tv, pool, n_restaurants) %>% 
    head(300) %>%
    DT::datatable(filter='top', 
                  options = DT_options, 
                  selection = list(mode = 'single', selected = 1)) %>%
    formatStyle(columns = colnames(.$x$data), fontSize = '12px', lineHeight='80%') %>%
    formatRound(columns = c(".fitted", ".resid"), 2)
      
  }, server = FALSE)
  
    
  output$image = renderUI({
    req(input$data_rows_selected)
    src <- filtered() %>% slice(input$data_rows_selected) %>% pull(picture_url)
    tags$img(src=src, style="width: 300px")
  })
  
  output$text <- renderText({
    req(input$data_rows_selected)
    obj <- filtered()[input$data_rows_selected,]
    glue::glue('This apartment in Berlin {obj$name} is {round(abs(obj$.resid),1)} $ cheaper than market value, where market value is estimated based on the following apartment features: {paste(xvars, collapse = ", ")}')
  })
  
  output$listing_url <- renderUI({
    req(input$data_rows_selected)
    obj <- filtered()[input$data_rows_selected,]
    url <- a(obj$listing_name, href=obj$listing_url)
    tagList("Link to Airbnb:", url)
  })
        
}

shinyApp(ui = ui, server = server)

