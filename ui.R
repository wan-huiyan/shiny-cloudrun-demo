
library(shiny)
library(shinyWidgets)
library(shinybusy)

shinyUI(fluidPage(
    
    titlePanel("Recommender"),
    
    sidebarLayout(
        sidebarPanel(
            
            uiOutput("store_picker"),
            
            uiOutput("prod_picker"),
            
            textInput("prod_quant", 
                      "Product quantity (pls separate multiple quantities with a space):", 
                      value = ""),
            
            materialSwitch(inputId = "appetisers_toggle",
                           label = "Appetisers only", 
                           value = FALSE,
                           status = "primary"),
            
            hr(),
            
            actionButton("post",
                         "Sent request", icon("check"))
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            
            add_busy_gif(src = "https://media1.giphy.com/media/xUA7aUV0jm1xc8TUS4/giphy.gif", 
                         position = "full-page",
                         height = 200, width = 200),
            
            #htmlOutput("selected_store_text"),
            h3("Items in basket:"),
            htmlOutput("selected_product_text"),
            #htmlOutput("selected_product_img"),
            
            hr(),
            
            fluidRow(
                column(8, h3("Recommendations:")),
                column(4, uiOutput("sort_selection"))
            ),
            
            fluidRow(
                column(4, htmlOutput("recommemded_prod1_img")),
                column(8, htmlOutput("recommemded_prod1"))
            ),
            
            hr(),
            
            fluidRow(
                column(4, htmlOutput("recommemded_prod2_img")),
                column(8, htmlOutput("recommemded_prod2"))
            ),
            
            hr(),
            
            fluidRow(
                column(4, htmlOutput("recommemded_prod3_img")),
                column(8, htmlOutput("recommemded_prod3"))
            ),
            
            hr(),
            
            fluidRow(
                column(4, htmlOutput("recommemded_prod4_img")),
                column(8, htmlOutput("recommemded_prod4"))
            ),
            
            hr(),
            
            fluidRow(
                column(4, htmlOutput("recommemded_prod5_img")),
                column(8, htmlOutput("recommemded_prod5"))
            ),
            
            hr(),
            
            fluidRow(
                column(4, htmlOutput("recommemded_prod6_img")),
                column(8, htmlOutput("recommemded_prod6"))
            ),
            
            hr(),
            
            fluidRow(
                column(4, htmlOutput("recommemded_prod7_img")),
                column(8, htmlOutput("recommemded_prod7"))
            ),
            
            hr(),
            
            fluidRow(
                column(4, htmlOutput("recommemded_prod8_img")),
                column(8, htmlOutput("recommemded_prod8"))
            ),
            
            hr(),
            
            fluidRow(
                column(4, htmlOutput("recommemded_prod9_img")),
                column(8, htmlOutput("recommemded_prod9"))
            ),
            
            hr(),
            
            fluidRow(
                column(4, htmlOutput("recommemded_prod10_img")),
                column(8, htmlOutput("recommemded_prod10"))
            )
        )
    )
))
