SegmentedScorecard <- setRefClass(
  "SegmentedScorecard", fields = c(segments="list", segvar="factor"))

SegmentedScorecard$methods(bin = function(data, perf, seg, ...) {

  if (!is.factor(seg)) stop("seg must be a factor.", call. = FALSE)
  segvar <<- seg

  ## TODO ## check that x, y, w, and seg are correct dimensions
  #stopifnot(all(sapply(list(y, w, seg), length) == nrow(x)))
  xs <- split(data, seg, drop=T)
  ps <- perf$split(seg=seg)

  #browser()
  segments <<- mapply(Scorecard$new, xs, ps, MoreArgs = list(...), SIMPLIFY = F)
  invisible(lapply(segments, function(x) x$bin(...)))
})


SegmentedScorecard$methods(fit = function(...) {
  invisible(lapply(segments, function(x) x$fit(...)))
})


SegmentedScorecard$methods(predict = function(newdata=NULL, seg=.self$segvar, models=NULL) {

  if (is.null(newdata)) {
    xs <- setNames(rep(list(NULL), length(levels(seg))), levels(seg))
  } else {
    xs <- split(newdata, seg)
  }

  res <- mapply(function(mod, newdata) mod$predict(newdata=newdata),
                segments, xs)

  unsplit(res, seg)

})




