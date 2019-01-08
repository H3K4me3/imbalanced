#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
source("lib.R")

# Options
DEFAULT_SD = 10
DEFAULT_N  = 10

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Imbalanced"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
          shiny::div(id="dynControlUI")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("pointsPlot", click = "pointsPlot_click", dblclick = "pointsPlot_dblclick")
      )
   )
)

loadControlUI <- function(suffix, label) {
    tagList(
        shiny::sliderInput(inputId = paste0("SDInput", suffix), label = paste(label, "SD"), value = DEFAULT_SD, min = 1, max = 50, step = 1),
        shiny::sliderInput(inputId = paste0("NumInput", suffix), label = paste(label, "N"), value = DEFAULT_N, min = 1, max = 100, step = 1)
    )
}



server <- function(input, output) {
    app.states = reactiveValues(points = list())
    
    observeEvent(input$pointsPlot_dblclick, {
        app.states$points <- list()
        removeUI("div:has(> #SDInput1)")
        removeUI("div:has(> #SDInput2)")
        removeUI("div:has(> #SDInput3)")
        removeUI("div:has(> #SDInput4)")
        removeUI("div:has(> #SDInput5)")
        removeUI("div:has(> #NumInput1)")
        removeUI("div:has(> #NumInput2)")
        removeUI("div:has(> #NumInput3)")
        removeUI("div:has(> #NumInput4)")
        removeUI("div:has(> #NumInput5)")
    })
    
    control_observer <- function(id) {
        id <- as.integer(id)
        bquote({
            print(paste("wtf", .(id)))
            sd <- input[[paste0("SDInput", .(id))]]
            n  <- input[[paste0("NumInput", .(id))]]
            
            opts <- attr(app.states$points[[.(id)]], "opts")
            if (n == opts$n && sd == opts$sd)
                return()
            app.states$points[[.(id)]] <- make_points(cx = opts$cx, cy = opts$cy, label = opts$label, n = n, sd = sd)
        })
    }
    
    nullnull <- function(a, b) {
        if (is.null(a) || is.null(b))
            return(NULL)
        TRUE
    }
    observeEvent( nullnull(input$SDInput1, input$NumInput1) , control_observer(1), handler.quoted = TRUE, ignoreInit = TRUE, ignoreNULL = TRUE)
    observeEvent( nullnull(input$SDInput2, input$NumInput2) , control_observer(2), handler.quoted = TRUE, ignoreInit = TRUE, ignoreNULL = TRUE)
    observeEvent( nullnull(input$SDInput3, input$NumInput3) , control_observer(3), handler.quoted = TRUE, ignoreInit = TRUE, ignoreNULL = TRUE)
    observeEvent( nullnull(input$SDInput4, input$NumInput4) , control_observer(4), handler.quoted = TRUE, ignoreInit = TRUE, ignoreNULL = TRUE)
    observeEvent( nullnull(input$SDInput5, input$NumInput5) , control_observer(5), handler.quoted = TRUE, ignoreInit = TRUE, ignoreNULL = TRUE)
    
    observeEvent(input$pointsPlot_click, {
        if (length(app.states$points) >= 5) {
            return()
        }
        
        cen.x1 <- input$pointsPlot_click$x
        cen.x2 <- input$pointsPlot_click$y
        
        new_label <- {
            existing_labels <- sapply(app.states$points, function(p) {
                ans <- unique(p$y)
                stopifnot(length(ans) == 1)
                ans
            })
            LETTERS[!LETTERS %in% existing_labels][1]
        }
        
        app.states$points[[length(app.states$points) + 1]] <- make_points(cx = cen.x1, cy = cen.x2, label = new_label,
                                                                          sd = DEFAULT_SD, n = DEFAULT_N)
        insertUI("#dynControlUI", where = "beforeEnd", ui = {
            loadControlUI(suffix = length(app.states$points), label = new_label)
        })
    })
    
    output$pointsPlot <- renderPlot(width = 500, height = 500, {
        plot_points(app.states$points, make_model(app.states$points))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

