#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyWidgets)
library(htmltools)
library(DT)
library(stringr)
library(raster)
library(png)
library(ggplot2)

# Load the once per session stuff here; most efficient outside of server/ui functions
load("shotData.RData")
#basketball court image
courtPlot<-"./stockCourtCropResized800.jpg"
#width of basketball court image in pixels
widthCourt  <- 800
basketX <- 40
basketY <- 213
#distance to hoop from point on court
distance <- NULL

x <- c(0, 50, 100)
y <- c(0, 50, 100)

pointsPlot <- data.frame(x, y)


# Define UI for application
ui <- fluidPage(
    fluidRow(
        titlePanel("NBA Field Goal Analysis"),
        tags$p("Check out our:",
               tags$a(href = "https://github.com/patrick-osborne/CSML1000-Group_10-Final-Project/", "Github")),
        tabsetPanel(type = "tabs",
                    # Data Analysts's Console Code Block ----
                    tabPanel("Data Analyst's Console", 
                             sidebarLayout(
                                 sidebarPanel(
                                     selectInput("model",
                                                 "Association Rule Mining Model:",
                                                 choices=c("Apriori", "ECLAT", "FP-Growth (Not Available)")),
                                     sliderInput("support",
                                                 "Support:",
                                                 min = 0.005,
                                                 max = 0.0401,
                                                 value = 0),
                                     sliderInput("confidence",
                                                 "Confidence:",
                                                 min = 0.5,
                                                 max = 1,
                                                 value = 0),
                                     sliderInput("lift",
                                                 "Lift:",
                                                 min = 4.5,
                                                 max = 104,
                                                 value = 0),
                                     sliderInput("count",
                                                 "Count:",
                                                 min = 0,
                                                 max = 825,
                                                 value = 0)
                                 ),
                                 
                                 # Show beautiful visuals to the right of the sidepanel!
                                 mainPanel(
                                     h3("[Data Table]"),
                                     DTOutput ("console")
                                 )
                             )
                    ),
                    
                    # User's Shopping Cart Code Block ----
                    tabPanel("Coach's Interface",
                             sidebarLayout(
                                 sidebarPanel(
                                     pickerInput("cartSelect",
                                                 label="Select Items:",
                                                 choices="Choice 1",
                                                 multiple=TRUE,
                                                 options=list('live-search' = TRUE
                                                 )
                                     ),
                                     actionButton("addCart",
                                                  "Add Items to Cart"),
                                     actionButton("clearCart",
                                                  "Clear Cart"),
                                     #h5("[Text]"),
                                     verbatimTextOutput("image_hoverinfo"),
                                     verbatimTextOutput("image_clickinfo"),
                                     verbatimTextOutput("distBasket")
                                     # verbatimTextOutput("image_brushinfo")
                                 ),
                                 
                                 # Show beautiful visuals to the right of the sidepanel!
                                 mainPanel(
                                     h3("Court Panel"),
                                     imageOutput("courtPlot", 
                                                 click = "image_click",
                                                 width = widthCourt,
                                                 hover = hoverOpts(
                                                     id = "image_hover",
                                                     delay = 500,
                                                     delayType = "throttle"
                                                 )),
                                     plotOutput("circleTest")
                                     
                                 )
                             )
                    )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(session, input, output) {
    output$emptyMessage <- renderText({ "Add items to your cart to get some recommendations." })
    output$console <- renderDataTable(shotData, rownames=FALSE)
    #shows an empty shopping cart (table)
    output$cart <- renderDataTable(shotData, 
                                   rownames=FALSE, 
                                   options=list(pageLength=9))
    
    observeEvent(input$model, {
        output$console <- renderDataTable(shotData,
                                          rownames=FALSE, 
                                          options=list(pageLength=9))
        
        
    })
    
    observeEvent(input$support, {
        output$console <- renderDataTable(shotData,
                                          rownames=FALSE, 
                                          options=list(pageLength=9))
        
        
    })
    
    observeEvent(input$confidence, {
        output$console <- renderDataTable(shotData,
                                          rownames=FALSE, 
                                          options=list(pageLength=9))
        
        
    })
    
    observeEvent(input$lift, {
        output$console <- renderDataTable(shotData,
                                          rownames=FALSE, 
                                          options=list(pageLength=9))
        
        
    })
    
    observeEvent(input$count, {
        output$console <- renderDataTable(shotData,
                                          rownames=FALSE, 
                                          options=list(pageLength=9))
        
        
    })
    
    observeEvent(input$addCart, {
    })
    
    
    observeEvent(input$clearCart, {
        
    })
    
    output$courtPlot <- renderImage({
        
        
        # Return a list containing information about the image
        list(
            src = courtPlot,
            width = widthCourt,
            contentType = "image/png",
            alt = "This is alternate text"
        )
    })
    
    output$image_hoverinfo <- renderPrint({
        cat("Position:\n")
        cat("x=")
        str(input$image_hover$x)
        cat("y=")
        str(input$image_hover$y)
    })
    
    output$image_clickinfo <- renderPrint({
        cat("Position:\n")
        cat("x=")
        str(input$image_click$x)
        cat("y=")
        str(input$image_click$y)
    })
    
    # output$distBasket <- renderPrint({
    #     cat("Distance to Basket:\n")
    #     pointDistance(c(41.5, 191), c(input$image_click$x, input$image_click$y), type='Euclidean', lonlat=FALSE)
    #     
    # })
    
    
    
    observeEvent(input$image_click, {
        
        #this should output the acutal width of the image in the UI but the number returned is too large.
        #width  <- session$clientData$output_courtPlot_width
        scalingFactor <- 94/widthCourt
        distance <- (pointDistance(c(basketX, basketY), c(input$image_click$x, input$image_click$y), type='Euclidean', lonlat=FALSE)) * scalingFactor
        output$distBasket <- renderPrint({
            cat("Distance to Basket:\n")
            cat(distance)
            cat(" feet")
        })


    })

    
    output$circleTest <- renderPlot(ggplot(pointsPlot, (aes(x=pointsPlot$x, y=pointsPlot$y))) + 
        xlim(0, 800) + ylim(0, 425) +
        geom_point(), width=800, height=425)

    
    
    #     output$image_brushinfo <- renderPrint({
    #     cat("Brush (debounced):\n")
    #     str(input$image_brush)
    # })
    
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
