
setClass("Transform", slots = c(
  tf = "ANY",
  value = "numeric",
  subst = "numeric",
  nas = "numeric",
  exceptions = "list")
)

setMethod("initialize", "Transform", function(.Object, ...) {
  .Object@exceptions = list(input=numeric(), output=numeric())
  validObject(.Object)
  .Object
})
