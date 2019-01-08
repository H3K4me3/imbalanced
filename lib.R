library(caret)
library(ggplot2)
library(tibble)

make_points <- function(cx, cy, label, n = 10, sd = 10) {
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

l_make_points <- Vectorize(make_points, vectorize.args = c("cx", "cy", "label", "n"), SIMPLIFY = FALSE)

if (FALSE) {
    make_points(1, 3, label = "wtf", n = 13)
    a <- l_make_points(1:3, 3:1, label = "wtf")
    a
    do.call(rbind, a)
}

merge_points <- function(ldf) {
    if (!length(ldf))
        ans <- data.frame(x1 = numeric(), x2 = numeric(), y = character(), stringsAsFactors = FALSE)
    else
        ans <- do.call(rbind, ldf)
    
    ans[['y']] <- forcats::as_factor(ans[['y']])
    ans
}
make_model <- function(lst.points, method = c("rf")) {
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
    data <- l_make_points(c(1:3), c(3:1), label = c("a", "b", "c"), n = c(110, 120, 130), sd = 1)
    #data <- l_make_points(c(1:3), c(3:1), label = c("a", "b", "c"), n = c(11, 12, 13), sd = 1)
    data <- do.call(rbind, data)
    system.time( model <- make_model(data) )
    model
}


plot_points <- function(lst.points, model, resolution = 100) {
    stopifnot(is.list(lst.points))
    stopifnot(all(sapply(lst.points, is.data.frame)))
    points <- merge_points(lst.points)
    stopifnot(colnames(points) == c("x1", "x2", "y"))
    
    #if (length(lst.points) < 2) {
    #    plot(points[, 1:2], col = as.integer(cl) + 1L, pch = as.integer(cl) + 1L, xlim = c(0, 100), ylim = c(0, 100))
    #    return(NULL)
    #}
    
    decisionplot(model, data = points, resolution = resolution)
}

decisionplot <- function(model, data, resolution = 100) {
    # Based on http://www.cmap.polytechnique.fr/~lepennec/R/Learning/Learning.html
    cl <- data[, 3]
    stopifnot(is.factor(cl))
    data <- data[, 1:2]
    k <- length(unique(cl))
    
    plot(data, col = as.integer(cl) + 1L, pch = as.integer(cl) + 1L, xlim = c(0, 100), ylim = c(0, 100))
    
    if (is.null(model)) {
        return()
    }
    
    # view port
    # FIXME: adjust the range
    r <- data.frame(x1 = c(0, 100), x2 = c(0, 100))
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
    points(g, col = as.integer(p) + 1L, pch = ".")
    
    
    # Plot boundary
    z <- matrix(as.integer(p), nrow = resolution, byrow = TRUE)
    contour(xs, ys, z, add = TRUE, drawlabels = FALSE, lwd = 2, levels = (1:(k-1))+.5)
    
    invisible(z)
}


if (FALSE) {
    points <- l_make_points(c(10,20,30), c(30,20,10), label = c("A", "B", "C"))
    model <- make_model(points)
    points <- do.call(rbind, points)
    points$y <- forcats::as_factor(points$y)
    decisionplot(model, data = points)
}

if (FALSE) {
    points <- l_make_points(c(10,20,30), c(30,20,10), label = c("A", "B", "C"))
    plot_points(points, model = make_model(points))
}


if (FALSE) {
    # Only one click
    points <- l_make_points(c(10), c(30), label = c("A"))
    plot_points(points, model = make_model(points))
    
}


