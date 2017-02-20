#' @include generic_methods.R

setClass("Transform", slots = c(
  tf = "ANY",
  subst = "numeric",
  exceptions = "numeric",
  nas = "numeric",
  neutralized = "character",
  repr = "list")
)

# setMethod("initialize", "Transform", function(.Object, ...) {
#   .Object@exceptions = list(input=numeric(), output=numeric())
#   validObject(.Object)
#   .Object
# })

setMethod("neutralize_", signature = c(tf="Transform", i="numeric"),
  function(tf, i, ...) {
    # browser()
    x <- c(names(tf@subst), tf@exceptions, names(tf@nas))
    new_tf <- tf

    ## ones that are already neutralized are UN-neutralized
    nix <- intersect(tf@neutralized, x[i])

    new_tf@neutralized <- setdiff(union(tf@neutralized, x[i]), nix)
    new_tf
  })

update_transform <- function(tf, result) {

  tf@subst <- setNames(result$normal[,"Pred"], row.names(result$normal))
  tf@nas <- c(Missing=result$missing[,"Pred"])

  exception_names <- row.names(result$exception)
  tf@exceptions[exception_names] <- result$exception[,"Pred"]

  tf@repr <- result
  tf
}


