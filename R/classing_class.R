#' @include performance_class.R

#' @export Classing
#' @exportClass Classing
Classing <- setRefClass("Classing",
  fields = c(
    variables = "list",
    performance = "Performance",
    step = "numeric") # step is 1, 2, or 3 depending on model inclusion level
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

  step <<- setNames(rep(2, length(d)), names(d))
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
  transforms=lapply(variables, function(b) b$tf), ...) {

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

  mapply(function(b, v, tf) b$predict(newdata=v, transform=tf),
         variables[i], newdata[i], transforms[i])

})

Classing$methods(drop = function(i, ...) {
  stopifnot(all(i %in% names(step)))
  step[i] <<- 3
})

Classing$methods(cluster = function(...) {
  woe <- predict(model=NULL, type="woe", ...)
  d <- apply(woe, 2, function(x) all(duplicated(x)[-1L]))
  corr <- cor(woe[,-d])

  list(correlations = corr, cluster = hclust(as.dist(1 - abs(corr))))

})
