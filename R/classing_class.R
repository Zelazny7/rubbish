#' @include performance_class.R

#' @export Classing
#' @exportClass Classing
Classing <- setRefClass("Classing",
  fields = c(
    variables = "list",
    performance = "Performance",
    step1 = "numeric",
    step2 = "numeric")
  )

setGeneric("create_bin", function(x, ...) callGeneric("create_bin"))

setMethod("create_bin", "numeric", function(x, ...) {
  Continuous$new(x = x, ...)
})

setMethod("create_bin", "factor", function(x, ...) {
  Discrete$new(x = x, ...)
})

Classing$methods(initialize = function(d=NULL,
  performance=Performance$new(), ...) {

  .self$performance <<- performance

  variables <<- lapply(setNames(names(d), names(d)), function(nm) {
    create_bin(x = d[[nm]], perf = performance, name = nm, ...)
  })
})

Classing$methods(bin = function(...) {
  for (i in seq_along(variables)) {
    variables[[i]]$bin(...)
  }
})

Classing$methods(show = function() {
  print("Classing object")
})

Classing$methods(predict = function(newdata=lapply(variables, function(b) b$x),
  ...) {

  ## check that data has var names
  stopifnot(!is.null(names(newdata)))

  ## check that all variables are found in newdata
  dnm <- names(newdata)
  vnm <- names(variables)
  if (!all(vnm %in% dnm)) {
    msg <- paste0(vnm, collapse = ", ")
    stop(sprintf("Vars not found in data: %s", msg), call. = F)
  }

  ## put the newdata in the same order as the variables
  i <- match(vnm, dnm)

  mapply(function(b, v) b$predict(newdata=v), variables[i], newdata[i])

})


Classing$methods(fit = function(newdata=lapply(variables, function(b) b$x),
  y=performance$y, w=performance$w, nfolds=5, upper.limits=3, lower.limits=0,
  alpha=1, ...) {

  # browser()

  ## check for consistent dimensions
  stopifnot(length((newdata[[1]]) == length(y)) &&  (length(y) == length(w)))

  x <- predict(newdata=newdata)

  fit <- cv.glmnet(x = x, y = y, weights = w, nfolds = nfolds,
    alpha = alpha, upper.limits=upper.limits, lower.limits=lower.limits, ...)

  ## return the fit object
  fit

})






