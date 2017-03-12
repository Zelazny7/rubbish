#' @include performance_class.R

#' @export Classing
#' @exportClass Classing
Classing <- setRefClass("Classing",
  fields = c(
    variables = "list",
    performance = "Performance",
    step = "numeric"))

setGeneric("create_bin", function(x, ...) callGeneric("create_bin"))

setMethod("create_bin", "numeric", function(x, ...) {
  Continuous$new(x = x, ...)
})

setMethod("create_bin", "factor", function(x, ...) {
  Discrete$new(x = x, ...)
})

Classing$methods(initialize = function(data=NULL,
  performance=Performance$new(), ...) {

  .self$performance <<- performance
  vnames <- setNames(names(data), names(data))

  variables <<- lapply(vnames, function(nm) {
    create_bin(x = data[[nm]], perf = performance, name = nm, ...)
  })

  step <<- setNames(rep(2, length(data)), vnames)
})

Classing$methods(bin = function(...) {

  ## add progress barcols <- colnames(x)

  for (i in seq_along(variables)) {
    progress_(i, length(variables), "Binning   ", variables[[i]]$name)
    variables[[i]]$bin(...)
  }
})

Classing$methods(show = function() {
  print("Classing object")
})

Classing$methods(predict = function(newdata=lapply(variables, function(b) b$x),
  transforms=get_transforms(.self), ...) {

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

  mapply(function(idx, b, v, tf) {
    progress_(idx, length(variables), "Predicting", b$name)
    b$predict(newdata=v, transform=tf)
  },
    seq_along(i), variables[i], newdata[i], transforms[i]
  )

})

Classing$methods(drop = function(i, ...) {
  stopifnot(all(i %in% names(step)))
  step[i] <<- 3
})

Classing$methods(cluster = function(drop=TRUE, ...) {
  woe <- predict(model=NULL, type="woe", ...)
  d <- apply(woe, 2, function(x) all(duplicated(x)[-1L]))

  ## drop dropped vars as well
  if (drop) {
    d <- union(d, which(step == 3))
  }

  corr <- cor(woe[,-d])

  list(correlations = corr, cluster = hclust(as.dist(1 - abs(corr))))

})

Classing$methods(sort = function(method=c("perf", "cluster", "alpha")) {
  method <- match.arg(method)

  switch(
    method,
    "perf" = {
      v <- sapply(variables, function(x) x$sort_value())
      i <- order(v, decreasing = TRUE, na.last = TRUE)
    },
    {
      i <- seq_along(variables)
      print("not implemented")
    }
  )

  variables <<- variables[i]
  step <<- step[i]

})

Classing$methods(get_transforms = function(...) {
  lapply(variables, function(x) x$tf)
})

Classing$methods(summary = function(tfs=.self$get_transforms(), step=.self$step...) {
  s <- lapply(variables, function(v) v$summary(tfs[[v$name]]))
  out <- do.call(rbind, s)

  cbind(out, step=step)
})
