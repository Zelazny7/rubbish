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

Classing$methods(initialize = function(d, performance, ...) {

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
