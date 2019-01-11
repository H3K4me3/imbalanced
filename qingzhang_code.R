# this is the combined version of app+lib

library(shiny)
library(caret)
library(ggplot2)
library(tibble)
library(tidyr)
###### Data #####


# make evenly spread centroids from the number of class



# ! only two classes
make_centroid <- function(ncen = 2, bg_ratio){
    
    cx = c(1,3)
    cy = c(3,1)
    
    # calculate points number for each class
    bgn = 300
    c1 = round(bgn*bg_ratio)
    c2 = bgn - c1
    
    return(list(cx = cx, cy = cy, label=LETTERS[1:ncen], n = c(c1, c2)))
}


# make background points

make_points <- function(cx, cy, label, n = 10, sd = 2) {
    stopifnot(length(cx) == 1)
    stopifnot(length(cy) == 1)
    stopifnot(length(label) == 1)
    stopifnot(length(n) == 1)
    stopifnot(n >= 1)
    stopifnot(length(sd) == 1)
    
    x1 <- rnorm(n, mean = cx, sd = sd)
    x2 <- rnorm(n, mean = cy, sd = sd)
    
    ans <- data.frame(x1 = x1, x2 = x2, y = label, stringsAsFactors = FALSE)
    attr(ans, "opts") <- list(cx = cx, cy = cy, label = label, n = n, sd = sd)
    ans
}



# select points

select_points <- function(df, select_ratio){
    nrows <- nrow(df)
    df_select <- df[sample(seq(nrows), size  = round(select_ratio*nrows)), ]
    attr(df_select, "opts")  <- c(attr(df, "opts"), list(sratio = select_ratio))
    return (df_select)
}

merge_points <- function(ldf) {
    if (!length(ldf))
        ans <- data.frame(x1 = numeric(), x2 = numeric(), y = character(), stringsAsFactors = FALSE)
    else
        ans <- do.call(rbind, ldf)
    ## print(ans)
    ans[['y']] <- forcats::as_factor(ans[['y']])
    ans
}


# for a single centroid, first generate the background assumptions, the true label distribution,
# and then the select portion between labels.

l_make_points <- Vectorize(make_points, vectorize.args = c("cx", "cy", "label", "n"), SIMPLIFY = FALSE)
l_select_points <- Vectorize(select_points, vectorize.args = c("df", "select_ratio"), SIMPLIFY = FALSE)



if (FALSE) {
    make_points(1, 3, label = "wtf", n = 13)
    a <- l_make_points(1:3, 3:1, label = "wtf")
    a
    b <- l_select_points(a, c(0.2, 0.4, 0.2))
    b
    do.call(rbind, a)
}


# ************************************************************
####### Classifiers #######

# the machine learning steps
make_model <- function(lst.points, method = c("rf","lda","rpart","knn", "glm")) {
    method <- match.arg(method)
    
    stopifnot(is.list(lst.points))
    stopifnot(all(sapply(lst.points, is.data.frame)))
    points <- merge_points(lst.points)
    stopifnot(colnames(points) == c("x1", "x2", "y"))
    
    if (length(lst.points) < 2) {
        return(NULL)
    }
    
    model <- train(y ~ ., data = points, method = method)
    model
}

if (FALSE) {
    data_bg <- l_make_points(c(1:3), c(3:1), label = c("a", "b", "c"), n = c(110, 120, 130), sd = 1)
    data_select <- l_select_points(data_bg, select_ratio = c(0.1, 0.3, 0.6))
    data_bind <- do.call(rbind, data_select)
    system.time( model <- make_model(data_select) )
    model
}


####### Plots #######


plot_points <- function(lst.points_all, lst.points_select, model, resolution = 100) {
    #browser()
    all_points <- merge_points(lst.points_all)
    select_points <- merge_points(lst.points_select)
    #stopifnot(colnames(points) == c("x1", "x2", "y"))
    
    #if (length(lst.points) < 2) {
    #    plot(points[, 1:2], col = as.integer(cl) + 1L, pch = as.integer(cl) + 1L, xlim = c(0, 100), ylim = c(0, 100))
    #    return(NULL)
    #}
    
    decisionplot(model, data_all = all_points, data_select = select_points, resolution = resolution)
}



decisionplot <- function(model, data_all, data_select, resolution = 10000) {
    # Based on http://www.cmap.polytechnique.fr/~lepennec/R/Learning/Learning.html
    cl_all <- data_all[, 3]
    stopifnot(is.factor(cl_all))
    data_all <- data_all[, 1:2]
    k <- length(unique(cl_all))
    
    
    cl_select<- data_select[, 3]
    stopifnot(is.factor(cl_select))
    data_select <- data_select[, 1:2]
    #k_select <- length(unique(cl_select))
    
    xmax = 4
    ymax = 4
    
    plot(data_all, col = as.integer(cl_all) + 1L, pch = as.integer(cl_all) + 1L, xlim = c(0, xmax), ylim = c(0, ymax))
    points(data_select, col = as.integer(cl_select) + 1L, pch = as.integer(cl_select) + 1L, lwd = 4)
    
    if (is.null(model)) {
        return()
    }
    
    # view port
    # FIXME: adjust the range
    r <- data.frame(x1 = c(0, xmax), x2 = c(0, ymax))
    xs <- seq(r[1,1], r[2,1], length.out = resolution)
    ys <- seq(r[1,2], r[2,2], length.out = resolution)
    g <- cbind(rep(xs, each=resolution), rep(ys, time = resolution))
    colnames(g) <- colnames(r)
    g <- as.data.frame(g)
    
    p <- predict(model, g)
    if (is.list(p))
        p <- p$class
    p <- as.factor(p)
    
    # Plot grid
    points(g, col = as.integer(p) + 1L, pch = ".", lwd = 2)
    
    
    # Plot boundary
    z <- matrix(as.integer(p), nrow = resolution, byrow = TRUE)
    contour(xs, ys, z, add = TRUE, drawlabels = FALSE, lwd = 2, levels = (1:(k-1))+.5, xlab="", ylab="")
    
    invisible(z)
}


####### Performance ########

measure_perf <- function(model, lst.all_points){
    
    stopifnot(is.list(lst.all_points))
    stopifnot(all(sapply(lst.all_points, is.data.frame)))
    points <- merge_points(lst.all_points)
    stopifnot(colnames(points) == c("x1", "x2", "y"))
    
    # browser()
    points$y <- as.factor(points$y)
    
    pred <- predict(model, points)
    
    
    get_mat <- function(x, y, reverse = FALSE){
        if (reverse) {
            x = factor(x,levels(x)[c(2,1)])
            y = factor(y,levels(y)[c(2,1)])
        }
        p <- confusionMatrix(x, y)$byClass[c(1,2,5,6,7)]
    }
    
    
    cmat1 <- get_mat(pred, points$y, FALSE)
    cmat2 <- get_mat(pred, points$y, TRUE)
    
    perf <- data.frame(classA = cmat1, classB = cmat2,
                       row.names = c("Sensitivity","Specificity","Precision","Recall","F1"))
    perf <- as.data.frame(t(perf))
    perf$class <- gsub("class", "", rownames(perf))
    
    return(perf)
    
}

plot_perf <- function(perf){
    perf <- tidyr::gather(perf, "measures", "num", -class) 
    ggplot(perf, aes(fill=class, y=num, x=measures)) + 
        geom_bar(position="dodge", stat="identity", alpha = 0.5) +
        scale_fill_manual(values=c("#FF0000", "#33FF33")) +
        theme_light() +
        theme(legend.position="top",
              axis.title.y= element_blank(),
              axis.title.x= element_blank(),
              axis.text.x = element_text(size=14))
}


###### Resample #######


resample_train <- function(method, lst.points){
    
    nobs <- sapply(lst.points, nrow)
    
    if(method == "Undersample"){
        df_resampled <- lapply(lst.points, function(df){
            sids <- sample(seq(nrow(df)), size = min(nobs))
            df[sids, ]
        })
    }
    else if(method == "Oversample"){
        df_resampled <- lapply(lst.points, function(df){
            sids <- sample(seq(nrow(df)), size = max(nobs), replace = TRUE)
            df[sids, ]
        })
    }
    else{df_resampled <- lst.points}
    
    return(df_resampled)
}






##### workflow ####


if (FALSE) {
    
    # set points
    l_allpoints <- l_make_points(1:3, 3:1, n = c(100, 20, 50) , label = letters[1:3], sd=1)
    l_selectedpoints <- l_select_points(l_allpoints, c(0.9, 0.4, 0.2))
    
    # classify
    
    ## do you resample?
    l_resample <- resample_train(method = "Undersample", l_selectedpoints)
    
    ## make model
    model <- make_model(l_resample, method = "glm")
    
    ## plot decisions
    plot_points(l_allpoints, l_resample, model)
    
    # performance
    perf <- measure_perf(model, l_allpoints)
    
    # plot performance
    plot_perf(perf)
    
}






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
