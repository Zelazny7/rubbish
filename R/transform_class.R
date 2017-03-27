#' @include generic_methods.R

setClass("Transform", slots = c(
    tf = "ANY",
    subst = "numeric",
    exceptions = "numeric",
    nas = "numeric",
    overrides = "numeric",
    repr = "list"),
  prototype = list(nas = c(Missing=0))
)

neutralize_ <- function(tf, i) {
  x <- c(names(tf@subst), tf@exceptions, names(tf@nas))
  new_tf <- tf

  ## ones that are already neutralized are UN-neutralized
  neutral <- names(which(tf@overrides == 0))
  nix <- intersect(neutral, x[i])

  overrides <- setdiff(x[i], nix)

  new_tf@overrides[overrides] <- 0

  new_tf
}

set_equal_ <- function(tf, v1, v2) {

  x <- c(tf@subst, tf@exceptions, tf@nas)
  new_tf <- tf

  overrides <- setNames(x[v2], names(x)[v1])

  new_tf@overrides[names(overrides)] <- overrides

  new_tf

}

update_transform <- function(tf, result) {

  tf@subst <- setNames(result$normal[,"Pred"], row.names(result$normal))
  tf@nas <- c(Missing=result$missing[,"Pred"])

  exception_names <- row.names(result$exception)
  tf@exceptions[exception_names] <- result$exception[,"Pred"]

  tf@repr <- result

  tf
}

