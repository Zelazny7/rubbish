

test <- setRefClass(Class="test", fields = c("a" ,"b"))

setGeneric("f", function(.self, a, b) standardGeneric("f"))

setMethod(f, signature = c(.self="test", a="numeric", b="missing"), definition = function(.self, a, b) {
  .self$a <- .self$a + 1
})

setMethod(f, signature = c(.self="test", a="numeric", b="numeric"), definition = function(.self, a, b) {
  print("Calling version two!")
  .self$b <- .self$b - 1
})


test$methods(f=f, g = function() print("yes?"))

x <- test$new(a=1, b=2)
