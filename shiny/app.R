library(shiny)
library(shinyWidgets)
library(raster)
library(caret)
library(gbm)
#library(htmltools)
#library(DT)
#library(stringr)
#library(png)

# Load the once per session stuff here; most efficient outside of server/ui functions

#load the fitted Regression Tree model
load("gbm_v2.RData")

#load a blank data frame for the current shot (to predict with)
load("individualShot.RData")

#load choices for user selection
load("uniqueChoices.RData")

#basketball court image
courtPlot <- "./www/stockcourtCropResized800.jpg"
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

defenderX <- NULL
defenderY <- NULL

# Define UI for application
ui <- fluidPage(
    fluidRow(
        titlePanel("NBA Shot Prediction"),
        
        # Coach's Interface ----
        sidebarLayout(
            sidebarPanel(
                sliderInput("shotclock",
                            "Shot Clock (seconds remaining):",
                            min = 0,
                            max = 24,
                            value = 0),
                sliderInput("gameclock",
                            "Game Clock (seconds remaining):",
                            min = 0,
                            max = 720,
                            value = 0),
                pickerInput("period",
                            label="Select Period (5-7 are overtime):",
                            choices=na.omit(uniqueChoices$PERIOD),
                            multiple=FALSE,
                            options=list('live-search' = TRUE
                            )
                ),
                actionButton("predict",
                             "Predict"),
                actionButton("reset",
                             "Reset Positions"),
                h4(" "),
                tags$b(verbatimTextOutput("shooterPos")),
                tags$b(verbatimTextOutput("defenderPos")),
                tags$b(verbatimTextOutput("distBasket")),
                tags$b(verbatimTextOutput("defenderShooterDist")),
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
                h4(" "),
                verbatimTextOutput("image_hoverinfo", placeholder=TRUE),
                tags$style(type='text/css', '#prediction {color: firebrick; font-size: medium;}'),
                tags$b(verbatimTextOutput("prediction", placeholder=TRUE)),
                h4(" "),
                tags$p("Court image courtesy of ",
                       tags$a(href = "https://www.123rf.com/photo_24220420_a-realistic-vector-hardwood-textured-basketball-court-.html", "123RF.com")),
                tags$p("Check out our:",
                       tags$a(href = "https://github.com/patrick-osborne/CSML1000-Group_10-Final-Project/", "Github"))
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
        
        defenderX <<- "NA"
        defenderY <<- "NA"

    })
    
    observeEvent(input$dbl_click, {
        #input closest defender distance into our current shot data frame to be used for prediction
        defenderX <<- input$dbl_click$x
        defenderY <<- input$dbl_click$y
            
        individualShot$CLOSE_DEF_DIST<- distanceDefender
        
        distanceDefender <-(pointDistance(c(defenderX, defenderY), c(input$image_click$x, input$image_click$y), type='Euclidean', lonlat=FALSE)) * scalingFactor
        output$defenderPos <- renderPrint({
            cat("Defender Position (from top-left corner):\n")
            cat("x = ")
            cat(round(defenderX * scalingFactor,digits=2))
            cat(" feet")
            cat("\ny = ")
            cat(round(defenderY * scalingFactor,digits=2))
            cat(" feet")
        })
        
        output$distBasket <- renderPrint({
            cat("Shooter Distance to Basket: ")
            cat(round((pointDistance(c(basketX, basketY), c(input$image_click$x, input$image_click$y), type='Euclidean', lonlat=FALSE)) * scalingFactor,digits=2))
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
                cat("Distance to Basket: ")
                cat(round(distanceBasket,digits=2))
                cat(" feet")
            })
            
            #input the selected distance to basket into our current shot data frame to be used for prediction
            individualShot$SHOT_DIST <- distanceBasket
            
            if (!(defenderX == "NA")){
            #input the selected distance to basket into our current shot data frame to be used for prediction
            individualShot$CLOSE_DEF_DIST <- (pointDistance(c(defenderX, defenderY), c(input$image_click$x, input$image_click$y), type='Euclidean', lonlat=FALSE)) * scalingFactor

            
            #input the selected shooter to the current shot data frame to be used for prediction
            individualShot$PERIOD <- input$period
            
            #input the selected XXXX to the current shot data frame to be used for prediction
            individualShot$GAME_CLOCK <- input$gameclock
            
            #input the selected XXXX to the current shot data frame to be used for prediction
            individualShot$SHOT_CLOCK <- input$shotclock
            
            #run the prediction with our current shot data frame
            individualPredict <- predict(model_gbm, individualShot, na.action=na.pass, type="prob")
            
            output$prediction <- renderPrint({
                cat("Predicted Likelihood of Scoring: ")
                cat(round(individualPredict$yes*100, digits=2))
                cat("%")
            })
            }
        })
        
        output$defenderShooterDist <- renderPrint({
            cat("Distance from Shooter to Closest Defender: ")
            cat(round((pointDistance(c(defenderX, defenderY), c(input$image_click$x, input$image_click$y), type='Euclidean', lonlat=FALSE)) * scalingFactor,digits=2))
            cat(" feet")
            
            
        })
        
        
        observeEvent(input$predict, {
            
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
                cat("Distance to Basket: ")
                cat(round(distanceBasket,digits=2))
                cat(" feet")
            })
            
            #input the selected distance to basket into our current shot data frame to be used for prediction
            individualShot$SHOT_DIST <- distanceBasket
            
            if (!(defenderX == "NA")){
                #input the selected distance to basket into our current shot data frame to be used for prediction
            individualShot$CLOSE_DEF_DIST <- (pointDistance(c(defenderX, defenderY), c(input$image_click$x, input$image_click$y), type='Euclidean', lonlat=FALSE)) * scalingFactor

            
            #input the selected shooter to the current shot data frame to be used for prediction
            individualShot$PERIOD <- input$period
            
            #input the selected XXXX to the current shot data frame to be used for prediction
            individualShot$GAME_CLOCK <- input$gameclock
            
            #input the selected XXXX to the current shot data frame to be used for prediction
            individualShot$SHOT_CLOCK <- input$shotclock
            
            #run the prediction with our current shot data frame
            individualPredict <- predict(model_gbm, individualShot, na.action=na.pass, type="prob")
            output$prediction <- renderPrint({
                cat("Predicted Likelihood of Scoring: ")
                cat(round(individualPredict$yes*100, digits=2))
                cat("%")
            })
            }
        })
        
        output$defenderShooterDist <- renderPrint({
            cat("Distance from Shooter to Closest Defender: ")
            cat(round((pointDistance(c(defenderX, defenderY), c(input$image_click$x, input$image_click$y), type='Euclidean', lonlat=FALSE)) * scalingFactor,digits=2))
            cat(" feet")
            
        })
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
