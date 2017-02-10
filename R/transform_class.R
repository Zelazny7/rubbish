#' @include generic_methods.R

setClass("Transform", slots = c(
  tf = "ANY",
  subst = "numeric",
  exceptions = "list",
  nas = "numeric",
  neutralized = "character")
)

setMethod("initialize", "Transform", function(.Object, ...) {
  .Object@exceptions = list(input=numeric(), output=numeric())
  validObject(.Object)
  .Object
})

setMethod("neutralize_", signature = c(tf="Transform", i="numeric"),
  function(tf, i, ...) {
    # browser()
    x <- c(names(tf@subst), tf@exceptions$input, names(tf@nas))
    new_tf <- tf

    ## ones that are already neutralized are UN-neutralized
    nix <- intersect(tf@neutralized, x[i])

    new_tf@neutralized <- setdiff(union(tf@neutralized, x[i]), nix)
    new_tf
  })

## replace inifnite prediction values with zeros
# replace_infinite <- function(tf) {
#   tf@subst[!is.finite(tf@subst)] <- 0
#   tf@nas[!is.finite(tf@nas)] <- 0
#   tf@exceptions$output[!is.finite(tf@exceptions$output)] <- 0
#   tf
# }
