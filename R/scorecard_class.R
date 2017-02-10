#' @include classing_class.R

#' @export Scorecard
#' @exportClass Scorecard
Scorecard <- setRefClass("Scorecard",
 fields = c(
   classing = "Classing",
   model = "ANY")
)


