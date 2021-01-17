# library(shiny)
# library(shinydashboard)
# library(tidyverse)
# library(sf)
# library(osmdata)
# library(tmap)
# library(DT)
# 
# load("data/prepared_data.Rdat")
# df <- df %>% st_transform(3035)
# vars <- c("listing_url", "price", "size", "neighbourhood_group_cleansed", "property_type", "room_type",
#            "accommodates","wifi", "tv", "pool", "altbau", "cooking_basics", "bed_linens", "parking",
#           "garden")
# 
# district_list <- unique(df$name)
# amenities <- c("restaurants", "attractions")
# max_price <- max(df$price, na.rm=TRUE)
# max_accomodates <- max(df$accommodates)
# 
# model3 <- lm(price ~  beds  + room_type + accommodates + tv + bed_linens + parking + garden + coffeemaker + pool + bathtub + cooking_basics + gym + altbau + n_restaurants + n_attractions + stop_dist, data = df)
# summary(model3)
# df <- df %>% rownames_to_column(".rownames")
# df_augmented <- broom::augment(model3)  %>%
#     relocate(c(.fitted, .resid), .after = price) %>%
#     left_join(df %>% select(.rownames, listing_url ), by = ".rownames")


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
                    box(width = 12,
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
                    box(width = 12,
                        collapsible = TRUE,
                        column(
                            width = 4,
                            sliderInput('price', 'Price range', min = 0, max = max_price,
                                        value = c(0, max_price))
                        ),
                        column(
                            width = 4,
                            sliderInput('accommodates', 'Max number of people allowed', 
                                        min = 1, max = max_accomodates, value = c(1, max_accomodates))
                        ),
                        column(
                            width = 4,
                            checkboxInput('tv', 'TV', value = FALSE)
                        ),
                        dataTableOutput('data')
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
            # tm_shape(restaurants) + tm_dots("red") +
            # tm_shape(attractions) + tm_dots("blue") +
            tm_shape(districts) + tm_borders(lwd = 2)
        
        # tm %>%
        #     tmap_leaflet() %>%
        #     leaflet::hideGroup(group = "name")
    })
    
    observe({
        var <- input$amenities
        tmapProxy("single_object", session, {
            tm_remove_layer(402) +
                tm_shape(get(input$amenities)) + tm_dots("blue", zindex = 402)
        })
        
    
    output$data <- renderDataTable({
        df_augmented %>% 
            filter(price >= input$price[1], price <= input$price[2]) %>% 
            filter(accommodates >= input$accommodates[1], accommodates <= input$accommodates[2]) %>%
            select(listing_url, price, .fitted, .resid, accommodates, room_type, beds, tv, pool, n_restaurants) %>%
            arrange(.resid)
    })
        
            
    })
    model3$call
    
}

shinyApp(ui = ui, server = server)
