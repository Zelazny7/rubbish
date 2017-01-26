
Discrete <- setRefClass("Discrete", contains = "Bin")

Discrete$methods(initialize = function(x, ...) {

  if(any(levels(x) %in% "")) stop("Factor levels contain blanks")
  callSuper(x=x, ...)
})

Discrete$methods(bin = function() {

  tf@tf <<- as.list(levels(x))
  names(tf@tf) <<- levels(x)
  callSuper()
})

Discrete$methods(collapse = function(v) {

  f <- which(tf@tf %in% unique(tf@tf)[v]) ## which values were selected for collapse?
  tf@tf[f] <<- paste(names(tf@tf)[f], collapse=',') # collapse them with commas
  callSuper()
})

Discrete$methods(expand = function(v) {
  f <- tf@tf %in% unique(tf@tf)[v]
  tf@tf[f] <<- levels(x)[f]
  callSuper()
})

Discrete$methods(factorize = function(...) {
  f <- callSuper(...)

  out <- x
  levels(out) <- unlist(tf@tf)[levels(out)]
  out <- addNA(out)
  levels(out)[is.na(levels(out))] <- "Missing"

  list(factor=out, types=f)
})
