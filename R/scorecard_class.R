#' @include classing_class.R

#' @export Scorecard
#' @exportClass Scorecard
Scorecard <- setRefClass("Scorecard",
 fields = c(
   seed = "numeric",
   models = "list"),
 contains = "Classing"
)

Scorecard$methods(initialize = function(..., seed=as.numeric(strftime(Sys.time(), format="%H%M%S"))) {
  seed <<- seed
  callSuper(...)
})

add_model_ <- function(.self, mod) {
  .self$models[[mod@name]] <- mod
}

Scorecard$methods(fit = function(name, newdata=lapply(variables, function(b) b$x),
  y=performance$y, w=performance$w, nfolds=5, upper.limits=3, lower.limits=0,
  alpha=1, ...) {

  ## check names of newdata here... TODO

  ## check for consistent dimensions
  stopifnot(length((newdata[[1]]) == length(y)) &&  (length(y) == length(w)))

  if (name %in% names(models)) {
    ans <- menu(c("Yes", "No"), title = "Model name already exists. Overwrite?")
    if (ans == "No") break
  }

  ## penalty factor for step 1 vars is 0
  pf <- rep(0, length(variables))

  pf[step == 1] <- 0
  pf[step == 2] <- 1

  x <- predict(newdata=newdata, type="woe")

  set.seed(sc$seed)
  this_fit <- cv.glmnet(x = x, y = y, weights = w, nfolds = nfolds,
                        alpha = alpha, upper.limits=upper.limits,
                        lower.limits=lower.limits, penalty.factor = pf,
                        exclude=which(step == 3), ...)

  ## store the last transforms
  m <- new("Model", name=name, fit=this_fit, step=step,
           transforms=lapply(variables, function(x) x$tf))

  ## get the lengths of the variables

  add_model_(.self, m)
  ## return the fit object
  #fits[[length(fits) + 1]] <<- this_fit

})

Scorecard$methods(show = function(...) {
  ## show the models / coefs?
  cat(sprintf("%d models", length(models)), sep="\n")
  cat(sprintf(" |-- %s", sapply(models, slot, "name")), sep="\n")
})

Scorecard$methods(predict = function(model=NULL, newdata=lapply(variables, function(b) b$x), type="score", ...) {

  if (is.null(model) & type == "woe") {
    return(callSuper(newdata=newdata, transforms=lapply(variables, function(b) b$tf), ...))
  } else if (is.null(model)) {
    model <- names(models)[[length(models)]]
  }

  stopifnot(model %in% names(models))
  mod <- models[[model]]
  woe <- callSuper(newdata=newdata, transforms=mod@transforms, ...)

  glmnet::predict.cv.glmnet(object=mod@fit, newx=woe, ...)

})

## show summary for selected model
## NULL shows the most recently fit model
Scorecard$methods(summary = function(model=NULL, ...) {
  if (is.null(model)) model <- models[[length(models)]]@name
  cat(sprintf("Showing summary for %s:", model), sep="\n")
})
