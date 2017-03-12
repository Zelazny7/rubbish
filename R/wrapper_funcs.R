### functions that map to the binnr2 equivalents

#' @export
bin <- function(data, y, w=rep(1, length(y)), min.iv=0.001, min.cnt=25,
                min.res=5, mono=0, max.bin=10, exceptions=numeric(0)) {

  ## only binary performance is supported currently
  perf <- Binary_Performance$new(y=y, w=w)

  sc <- rubbish:::Scorecard$new(data=data, performance=perf)

  sc$bin(min.iv=min.iv, min.cnt=min.cnt, min.res=min.res, mono=mono,
         max.bin=max.bin, exceptions=exceptions)

  return(sc)
}

#' @export
adjust <- function(x, ...) {
  x$adjust(...)
}

#' @export
fit <- function(x, name, ...) {
  x$fit(name=name, ...)
}
