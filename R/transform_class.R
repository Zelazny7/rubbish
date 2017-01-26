
setClass("Transform", slots = c(
  tf = "ANY",
  subst = "numeric",
  nas = "numeric",
  exceptions = "list")
)

setMethod("initialize", "Transform", function(.Object, ...) {
  .Object@exceptions = list(input=numeric(), output=numeric())
  validObject(.Object)
  .Object
})
