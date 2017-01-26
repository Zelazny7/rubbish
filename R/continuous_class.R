
Continuous <- setRefClass("Continuous", contains = "Bin")

Continuous$methods(
  bin = function(min.iv=0.01, min.cnt=10, min.res=0, max.bin=10, mono=0,
                 exceptions=numeric(0), ...) {

    f <- !is.na(x)
    tf@tf <<- .Call("bin", as.double(x[f]), as.double(perf$y[f]),
                    as.double(perf$w[f]), as.double(min.iv), as.integer(min.cnt),
                    as.integer(min.res), as.integer(max.bin), as.integer(mono),
                    as.double(exceptions))

    tf@exceptions$input <<- exceptions

    callSuper()
  }
)

Continuous$methods(
  collapse = function(v) {
    d <- unique(pmax(pmin(tail(v, -1), length(tf@tf) - 1), 2))
    tf@tf <<- tf@tf[-d]
    callSuper()
  }
)

Continuous$methods(
  expand = function(v) {

    stopifnot(length(v) == 1)

    f <- ! (is.na(x) | x %in% tf@exceptions$input)

    a <- min(max(1, v), length(tf@tf))  # can't be smaller than 1
    z <- max(min(v + 1, length(tf@tf)), a) # or larger than max els

    vals <- x[x > tf@tf[a] & x <= tf@tf[z] & f]

    q <- unique(quantile(vals, seq(0, 1, 0.2))) # quintiles

    tf@tf <<- sort(c(tf@tf[-z], q))
    callSuper()
  }
)

fmt_numeric_cuts <- function(cuts) {
  l = format(round(cuts, 2), trim=TRUE, nsmall=2, digits=2, big.mark=",",
             scientific = FALSE)

  ## get width of largest value
  fmt = sprintf("(%%%1$ds - %%%1$ds]", max(nchar(l)))
  sprintf(fmt, head(l, -1), tail(l, -1))
}

## Note: this should never be called directly by the user,
## it is used by the update performance functions to get the X-var
## summary data
Continuous$methods(
  factorize = function(...) {

  f <- callSuper(...)

  lbls <- fmt_numeric_cuts(tf@tf)
  out <- factor(x, exclude=NULL, levels=c(lbls, tf@exceptions$input, NA))

  levels(out)[is.na(levels(out))] <- "Missing"
  out[f$normal] <- cut(x[f$normal], tf@tf, include.lowest = T, labels = lbls)

  list(factor=out, types=f)
})
