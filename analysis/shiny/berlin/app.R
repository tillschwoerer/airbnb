library(shiny)
library(shinydashboard)
library(tidyverse)
library(sf)
library(osmdata)
library(tmap)
library(DT)

DT_options <- list(pageLength = 5, lengthMenu = c(5, 10, 15, 20), dom="tpl", scrollX = TRUE, autoWidth = TRUE)

load("data/prepared_data.Rdat")
df <- df %>% st_transform(3035)
vars <- c("listing_url", "price", "size", "neighbourhood_group_cleansed", "property_type", 
          "room_type", "accommodates","wifi", "tv", "pool", "altbau", "cooking_basics", 
          "bed_linens", "parking", "garden")

district_list <- unique(df$name)
amenities <- c("restaurants", "attractions")
max_price <- max(df$price, na.rm=TRUE)
max_accomodates <- max(df$accommodates)

model3 <- lm(price ~  beds  + room_type + accommodates + tv + bed_linens + parking + garden + coffeemaker + pool + bathtub + cooking_basics + gym + altbau + n_restaurants + n_attractions + stop_dist, data = df)
# xvars <- all.vars(formula(model3)[[3]])
df_augmented <- broom::augment_columns(model3, data = df)

ui <- dashboardPage(
  dashboardHeader(title = "Berlin Holiday Planner"),
  dashboardSidebar(
    sidebarMenu(
      menuItem(text = "Berlin", tabName = "berlin", icon = icon("flag")),
      menuItem(text = 'Recommendation', tabName = 'recommendation', selected = TRUE)
    )  
  ),
    
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "berlin",
        fluidRow(
          box(
            width = 12,
            collapsible = TRUE,
            selectInput("amenities", "Amenities", choices = amenities ),
            selectInput("district", "District", district_list),
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
              checkboxInput('tv', 'TV', value = FALSE)
            ),
            actionButton('refresh', 'Refresh', icon = icon('refresh')),
            DT::dataTableOutput('data'),
            box(
              width = 12,
              collapsible = TRUE,
              column(width = 6, uiOutput('image' )),
              column(width = 6, textOutput('text'), uiOutput('listing_url')),
            )
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
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
      tm_dots(size = 1) + 
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
      input_tv <- input$tv
    })
    df_augmented %>% 
      filter(price >= input_price[1], price <= input_price[2]) %>% 
      filter(accommodates >= input_accommodates, accommodates <= (input_accommodates + 2)) %>%
      filter(tv == input_tv) %>%
      arrange(.resid) %>%
      filter(.resid > -25)
  })

  output$data <- DT::renderDataTable({
    filtered() %>%
    select(listing_url, picture_url, price, .fitted, .resid, accommodates, room_type, 
           beds, tv, pool, n_restaurants) %>% 
    head(300) %>%
    DT::datatable(filter='top', options = DT_options, selection = 'single') %>%
    formatStyle(columns = colnames(.$x$data), fontSize = '12px', lineHeight='70%')
  }, server = FALSE)
    
    
  output$image = renderUI({
    req(input$data_rows_selected)
    src <- filtered() %>% slice(input$data_rows_selected) %>% pull(picture_url)
    tags$img(src=src, style="width: 300px")
  })
  
  output$text <- renderText({
    req(input$data_rows_selected)
    obj <- filtered()[input$data_rows_selected,]
    glue::glue('This apartment is {round(obj$.resid)} $ cheaper than market value, where market value is estimated based on the following apartment features: {paste(xvars, collapse = ", ")}')
  })
  
  output$listing_url <- renderUI({
    req(input$data_rows_selected)
    obj <- filtered()[input$data_rows_selected,]
    url <- a("bls", href=obj$listing_url)
    tagList("URL link:", url)
  })
        
}

shinyApp(ui = ui, server = server)

