mice.impute.cart2<-function (y, ry, x, wy = NULL, minbucket = 5, cp = 1e-04, ...) 
{
    if (is.null(wy)) 
        wy <- !ry
    minbucket <- max(1, minbucket)
    xobs <- x[ry, , drop = FALSE]
    xmis <- x[wy, , drop = FALSE]
    yobs <- y[ry]
    if (!is.factor(yobs)) {
        fit <- rpart(yobs ~ ., data = as.data.frame(cbind(yobs, xobs)), method = "anova", 
            control = rpart.control(minbucket = minbucket, cp = cp, 
                ...))
        leafnr <- floor(as.numeric(row.names(fit$frame[fit$where, 
            ])))
        fit$frame$yval <- as.numeric(row.names(fit$frame))
        nodes <- predict(object = fit, newdata = as.data.frame(xmis))
        donor <- lapply(nodes, function(s) yobs[leafnr == s])
        impute <- vapply(seq_along(donor), function(s) sample(donor[[s]], 
            1), numeric(1))
    }
    else {
        fit <- rpart(yobs ~ ., data = as.data.frame(cbind(yobs, xobs)), method = "class", 
            control = rpart.control(minbucket = minbucket, cp = cp, 
                ...))
        nodes <- predict(object = fit, newdata = as.data.frame(xmis))
        impute <- apply(nodes, MARGIN = 1, FUN = function(s) sample(colnames(nodes), 
            size = 1, prob = s))
    }
    return(impute)
}
