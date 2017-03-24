#' @include performance_class.R

#' @export Classing
#' @exportClass Classing
Classing <- setRefClass("Classing",
  fields = c(
    variables = "list",
    performance = "Performance",
    dropped = "logical"))

setGeneric("create_bin", function(x, ...) callGeneric("create_bin"))

setMethod("create_bin", "numeric", function(x, ...) {
  Continuous$new(x = x, ...)
})

setMethod("create_bin", "factor", function(x, ...) {
  Discrete$new(x = x, ...)
})

setMethod("create_bin", "ANY", function(x, ...) {
  NULL
})

Classing$methods(initialize = function(data=NULL,
  performance=Performance$new(), ...) {

  .self$performance <<- performance
  vnames <- setNames(names(data), names(data))

  variables <<- lapply(vnames, function(nm) {
    create_bin(x = data[[nm]], perf = performance, name = nm, ...)
  })

  ## drop variables that aren't numeric or factors
  variables <<- variables[lengths(variables) > 0]
  dropped <<- setNames(logical(length(variables)), names(variables))
})

Classing$methods(bin = function(...) {
  on.exit(cat(sep = "\n"))

  for (i in seq_along(variables)) {
    progress_(i, length(variables), "Binning   ", variables[[i]]$name)
    variables[[i]]$bin(...)
  }

  ## drop vars with zero information value
  zero_value <- sapply(variables, function(b) b$sort_value())
  dropped[zero_value == 0] <<- TRUE

})

Classing$methods(show = function() {
  print("Classing object")
})

Classing$methods(get_variables = function(..., keep=FALSE) {

  k <- which(!dropped[names(variables)])

  if (!keep) {
    lapply(variables[k], function(x) x$x)
  } else {
    lapply(variables, function(x) x$x)
  }
})

Classing$methods(get_transforms = function(..., keep=FALSE) {
  k <- which(!dropped[names(variables)])

  if (!keep) {
    lapply(variables[k], function(x) x$tf)
  } else {
    lapply(variables, function(x) x$tf)
  }
})

Classing$methods(predict = function(newdata=.self$get_variables(),
  transforms=.self$get_transforms(), ...) {
  on.exit(cat(sep = "\n"))

  ## check that data has var names
  stopifnot(!is.null(names(newdata)))

  ## check that all variables are found in newdata
  dnm <- names(newdata)
  vnm <- names(which(!dropped[names(variables)]))

  if (!all(vnm %in% dnm)) {
    msg <- paste0(vnm[!vnm %in% dnm], collapse = ", ")
    stop(sprintf("Vars not found in data: %s", msg), call. = F)
  }

  ## put the newdata in the same order as the variables
  woe <- mapply(function(idx, b, v, tf) {
    progress_(idx, length(vnm), "Predicting", b$name)
    b$predict(newdata=v, transform=tf)
  },
    seq_along(vnm), variables[vnm], newdata[vnm], transforms[vnm])

  colnames(woe) <- vnm
  woe

})

Classing$methods(drop = function(i, ...) {
  stopifnot(all(i %in% names(dropped)))
  dropped[i] <<- TRUE
})

Classing$methods(undrop = function(i, ...) {
  stopifnot(all(i %in% names(dropped)))
  dropped[i] <<- FALSE
})

Classing$methods(cluster = function(keep=FALSE, ...) {
  woe <- predict(newdata=get_variables(keep = keep), type="woe", ...)
  dups <- apply(woe, 2, function(x) all(duplicated(x)[-1L]))

  corr <- cor(woe[,which(!dups)])

  structure(
    list(
      correlations = corr,
      cluster = hclust(as.dist(1 - abs(corr)))),
    class="classing_cluster")

})

Classing$methods(prune_clusters =  function(cc, corr=0.80, n=1) {
  stopifnot(is(cc, "classing_cluster"))

  ## get information values
  p <- sapply(variables[colnames(cc$correlations)], function(x) x$sort_value())

  ## cutree
  grps <- cutree(cc$cluster, h=1-corr)

  # split correlations into groups and return everyone after the first
  splt <- split(data.frame(var=names(grps), val=p, stringsAsFactors = F), grps)

  ## order each group by descending perf value and drop all but the first
  to_drop <- lapply(splt, function(x) x$var[order(-x$val)][-seq.int(n)])
  unlist(to_drop)
})


Classing$methods(summary = function(...) {
  s <- lapply(variables, function(v) v$summary())
  res <- do.call(rbind, s)
  cbind(res, `Dropped`=dropped[row.names(res)])
})

