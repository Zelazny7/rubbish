
Continuous <- setRefClass("Continuous", contains = "Bin")

Continuous$methods(
  collapse = function(v) {
    d <- unique(pmax(pmin(tail(v, -1), length(tf@tf) - 1), 2))
    tf@tf <<- tf@tf[-d]
    callSuper()
  }
)

Continuous$methods(
  expand = function(v) {

    #browser()
    stopifnot(length(v) == 1)

    f <- !(is.na(x) | x %in% as.numeric(names(tf@exceptions)))

    a <- min(max(1, v), length(tf@tf))  # can't be smaller than 1
    z <- max(min(v + 1, length(tf@tf)), a) # or larger than max els

    vals <- c(x[x > tf@tf[a] & x <= tf@tf[z] & f])

    q <- c(quantile(vals, seq(0.2, 0.8, 0.2))) # quintiles

    tf@tf <<- sort(unique(c(tf@tf, q)))
    callSuper()
  }
)

fmt_numeric_cuts <- function(cuts) {
  l <- format(cuts, trim=TRUE, nsmall=3, digits=3, big.mark=",",
    scientific = FALSE)

  ## get width of largest value
  fmt = sprintf("(%%%1$ds - %%%1$ds]", max(nchar(l)))
  sprintf(fmt, head(l, -1), tail(l, -1))
}

## Note: this should never be called directly by the user,
## it is used by the update performance functions to get the X-var
## summary data
Continuous$methods(factorize = function(newdata=.self$x, transform=.self$tf, ...) {
    f <- callSuper(newdata=newdata, transform=transform, ...)

    lbls <- fmt_numeric_cuts(transform@tf)
    out <- factor(newdata, exclude=NULL,
      levels=c(lbls, names(transform@exceptions), NA))

    levels(out)[is.na(levels(out))] <- "Missing"
    out[f$normal] <- cut(newdata[f$normal], transform@tf, include.lowest = T,
      labels = lbls)

    list(factor=out, types=f)
})

Continuous$methods(predict = function(newdata=.self$x, transform=.self$tf, ...) {
  stopifnot(is.numeric(newdata))
  callSuper(newdata=newdata, transform=transform, ...)
})
