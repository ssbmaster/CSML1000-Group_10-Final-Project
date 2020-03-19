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
#library(htmltools)
#library(DT)
#library(stringr)
library(raster)
#library(png)
library(caret)
library(gbm)

# Load the once per session stuff here; most efficient outside of server/ui functions

#load the fitted Regression Tree model
load("gbm_v2.RData")

#load a blank data frame for the current shot (to predict with)
load("individualShot.RData")

#basketball court image
courtPlot <- "stockcourtCropResized800.jpg"
#width of basketball court image in pixels
widthCourt  <- 800
heightCourt <- 425
#scaling the values from pixels to feet based on 94 foot wide court
scalingFactor <- 94/widthCourt

basketX <- 40
basketY <- 213
#distance to hoop from point on court
distanceBasket <- NULL
#distance from shooter to closest defender
distanceDefender <- NULL

# Define UI for application
ui <- fluidPage(
    fluidRow(
        titlePanel("NBA Field Goal Analysis"),
        tags$p("Check out our:",
               tags$a(href = "https://github.com/patrick-osborne/CSML1000-Group_10-Final-Project/", "Github")),
        tabsetPanel(type = "tabs",
                    # Coach's Interface ----
                    tabPanel("Coach's Interface",
                             sidebarLayout(
                                 sidebarPanel(
                                     
                                     actionButton("reset",
                                                  "Reset All"),
                                     h4(" "),
                                     verbatimTextOutput("image_hoverinfo"),
                                     verbatimTextOutput("shooterPos"),
                                     verbatimTextOutput("defenderPos"),
                                     verbatimTextOutput("distBasket"),
                                     verbatimTextOutput("defenderShooterDist"),
                                     tags$b(verbatimTextOutput("prediction"))
                                 ),
                                 
                                 # Show beautiful visuals to the right of the sidepanel!
                                 mainPanel(
                                     h4(tags$b("Single-click to place the shooter. Double-click to place the defender.")),
                                     h5(tags$i("You must place both players to get a prediction.")),
                                     imageOutput("courtPlot", 
                                                 click = "image_click",
                                                 dblclick = "dbl_click",
                                                 width = widthCourt,
                                                 height = heightCourt,
                                                 hover = hoverOpts(
                                                     id = "image_hover",
                                                     delay = 500,
                                                     delayType = "throttle"
                                                 )),
                                     tags$a(href = "https://www.123rf.com/photo_24220420_a-realistic-vector-hardwood-textured-basketball-court-.html", "Court image courtesy of 123RF.com"))                               
                             )
                    )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(session, input, output) {
    
    output$courtPlot <- renderImage({
        
        
        # Return a list containing information about the image
        list(
            src = courtPlot,
            width = widthCourt,
            contentType = "image/png",
            alt = "This is alternate text"
        )
    }, deleteFile = FALSE)
    
    output$image_hoverinfo <- renderPrint({
        if(is.null(input$image_hover$x) && is.null(input$image_hover$y))
        {
            cat("")
        }else{
            cat("Mouse Hover Position (from top-left corner):\n")
            cat("x = ")
            cat(round(input$image_hover$x * scalingFactor,digits=2))
            cat(" feet")
            cat("\ny = ")
            cat(round(input$image_hover$y * scalingFactor,digits=2))
            cat(" feet")
        }
    })
    
    output$shooterPos <- renderPrint({
        if(is.null(input$image_click$x) && is.null(input$image_click$y))
        {
            cat("")
        }else{
            cat("Shooter Position (from top-left corner):\n")
            cat("x = ")
            cat(round(input$image_click$x * scalingFactor,digits=2))
            cat(" feet")
            cat("\ny = ")
            cat(round(input$image_click$y * scalingFactor,digits=2))
            cat(" feet")
        }
    })
    
    observeEvent(input$reset, {
        output$shooterPos <- NULL
        output$defenderPos <- NULL
        output$distBasket <- NULL
        output$defenderShooterDist <- NULL
        output$prediction <- NULL
    })
    
    observeEvent(input$dbl_click, {
        #input closest defender distance into our current shot data frame to be used for prediction
        individualShot$CLOSE_DEF_DIST<- distanceDefender
        
        
        distanceDefender <-(pointDistance(c(input$dbl_click$x, input$dbl_click$y), c(input$image_click$x, input$image_click$y), type='Euclidean', lonlat=FALSE)) * scalingFactor
        print(distanceDefender)
        output$defenderPos <- renderPrint({
            cat("Defender Position (from top-left corner):\n")
            cat("x = ")
            cat(round(input$dbl_click$x * scalingFactor,digits=2))
            cat(" feet")
            cat("\ny = ")
            cat(round(input$dbl_click$y * scalingFactor,digits=2))
            cat(" feet")
        })
        
        output$distBasket <- renderPrint({
            cat("Shooter Distance to Basket:\n")
            cat(round(distanceBasket,digits=2))
            cat(" feet")
        })
        
        observeEvent(input$image_click, {
            
            output$shooterPos <- renderPrint({
                if(is.null(input$image_click$x) && is.null(input$image_click$y))
                {
                    cat("")
                }else{
                    cat("Shooter Position (from top-left corner):\n")
                    cat("x = ")
                    cat(round(input$image_click$x * scalingFactor,digits=2))
                    cat(" feet")
                    cat("\ny = ")
                    cat(round(input$image_click$y * scalingFactor,digits=2))
                    cat(" feet")
                }
            })
            
            #this should output the acutal width of the image in the UI but the number returned is too large.
            #width  <- session$clientData$output_courtPlot_width
            distanceBasket <- (pointDistance(c(basketX, basketY), c(input$image_click$x, input$image_click$y), type='Euclidean', lonlat=FALSE)) * scalingFactor
            output$distBasket <- renderPrint({
                cat("Distance to Basket:\n")
                cat(round(distanceBasket,digits=2))
                cat(" feet")
            })
            
            #input the selected distance to basket into our current shot data frame to be used for prediction
            individualShot$SHOT_DIST <- distanceBasket
            
            #input the selected distance to basket into our current shot data frame to be used for prediction
            individualShot$CLOSE_DEF_DIST <- (pointDistance(c(input$dbl_click$x, input$dbl_click$y), c(input$image_click$x, input$image_click$y), type='Euclidean', lonlat=FALSE)) * scalingFactor
            
            #run the prediction with our current shot data frame
            individualPredict <- predict(model_gbm, individualShot, na.action=na.pass, type="prob")
            output$prediction <- renderPrint({
                cat("Predicted Likelihood of Scoring:\n")
                cat(round(individualPredict$yes*100, digits=2))
                cat("%")
            })
        })
        
        output$defenderShooterDist <- renderPrint({
            cat("Distance from Shooter to Closest Defender:\n")
            cat(round(distanceDefender,digits=2))
            cat(" feet")
            
            
        })
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
