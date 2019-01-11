library(shiny)
source("lib.R")

rm(list=ls())

# For dropdown menu
actionLink <- function(inputId, ...) {
    tags$a(href='javascript:void',
           id=inputId,
           class='action-button',
           ...)
}

ui <- fixedPage(
    titlePanel("Handling the Imbalanced Data"),
    h5("Qing Zhang, CBQG@HSPH"),
    h5(paste0("Real data, compared to the ones we used in ",
              "class, are usually messy and *imbalanced*,",
              "The problem may or may not pose an issue, ", 
              "depending on the specific problem.",
              "Here we present an user interface to simulate imbalanced data, ",
              "together with several techniques to leverage ",
              "this issue, including model selection and resampling ",
              "to see how classification performance will be changed. ")),
    
    br(),
    
    fluidRow(
        column(4,
               wellPanel(
                   
                   # data generation
                   h4("Set your learning data"),
                   sliderInput("bg_ratio", "True label distribution ratio",
                               0.05, 1, 0.5, step = 0.05),
                   sliderInput("c1r", "class 1 sample ratio", 0.05, 1, 0.4, step=0.05),
                   sliderInput("c2r","class 2 sample ratio", 0.05, 1, 0.1, step = 0.05)
               ),
               wellPanel(
                   
                   h4("Choose your classifier"),
                   
                   # classification method
                   selectInput("class_method", "Classifier",
                               choices = c(Random_forest = "rf", 
                                           Generalized_Linear_Models = "glm",
                                           Linear_discriminant_analysis = "lda",
                                           Decision_Tree = "rpart",
                                           K_Nearest_Neighbor = "knn")),
                   
                   h4("Choose your resample scheme"),
                   radioButtons("sample_method", "How do you deal with class imbalance?",
                                choices = c("Oversample", 'Undersample', "As-is"),
                                selected = "As-is"),
                   
                   tags$hr(),
                   
                   tags$small(paste0(
                       "Note: The bold symbols in the decision plot are the ones selected",
                       " (as chosen by the class x sample ratio). You may change the algorithm",
                       " or the resampling scheme and determine whichever gives better prediction."
                   ))
               )
        ),
        column(7,
               wellPanel(h5("Decision Plot"),
                         plotOutput("decision_bounds"),
                         tags$hr(),
                         h5("Performance Measures (When each class is defined as positive)"),
                         plotOutput("performance")
               )
        )
    )
)


server <- function(input, output) {
    
    model <- reactive({
        l_allpoints <- do.call("l_make_points", make_centroid(bg_ratio = input$bg_ratio))
        l_selectedpoints <- l_select_points(l_allpoints, c(input$c1r, input$c2r))
        l_resample <- resample_train(method = input$sample_method, l_selectedpoints)
        make_model(l_resample, method = input$class_method)
    })
    print("a")
    output$decision_bounds <- renderPlot(plot_points(l_allpoints, l_resample, model()))
    
    # plot performance
    output$performance <- renderPlot(plot_perf(measure_perf(model(), l_allpoints)))
}


# Run the application 
shinyApp(ui = ui, server = server)
