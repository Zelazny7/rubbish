#' @include performance_class.R

#' @export Classing
#' @exportClass Classing
Classing <- setRefClass("Classing",
  fields = c(
    variables = "list",
    vnames = "character",
    performance = "Performance",
    dropped = "character"))

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
  vnames <<- setNames(names(data), names(data))

  variables <<- lapply(vnames, function(nm) {
    create_bin(x = data[[nm]], perf = performance, name = nm, ...)
  })

  ## drop variables that aren't numeric or factors
  f <- !sapply(variables, is.null)

  variables <<- variables[f]
  vnames <<- vnames[f]
})

Classing$methods(bin = function(...) {
  on.exit(cat(sep = "\n"))

  for (i in seq_along(variables)) {
    progress_(i, length(variables), "Binning   ", variables[[i]]$name)
    variables[[i]]$bin(...)
  }

  ## drop vars with zero information value
  zeros <- sapply(variables, function(b) b$sort_value()) == 0

  dropped <<- names(variables)[zeros]

})

Classing$methods(show = function() {
  print("Classing object")
})

Classing$methods(get_variables = function(..., keep=FALSE) {
  if (!keep) {
    lapply(variables[setdiff(vnames, dropped)], function(x) x$x)
  } else {
    lapply(variables, function(x) x$x)
  }
})

Classing$methods(get_transforms = function(..., keep=FALSE) {
  if (!keep) {
    lapply(variables[setdiff(vnames, dropped)], function(x) x$tf)
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
  vnm <- names(transforms)

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

Classing$methods(drop = function(vars=character(0), all=FALSE, ...) {
  if (all) {
    dropped <<- vnames
  } else {
    stopifnot(all(vars %in% vnames))
    dropped <<- unique(c(dropped, vars))
  }
})

Classing$methods(undrop = function(vars=character(0), all=FALSE, ...) {
  if (all) {
    dropped <<- character(0)
  } else {
    stopifnot(all(vars %in% vnames))
    dropped <<- setdiff(dropped, vars)
  }
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
  res <- cbind(do.call(rbind, s), Dropped=0)
  res[dropped, "Dropped"] <- 1
  res
})

