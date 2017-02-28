#' @include classing_class.R

#' @export Scorecard
#' @exportClass Scorecard
Scorecard <- setRefClass("Scorecard",
 fields = c(
   seed = "numeric",
   models = "list",
   HEAD = "list",
   selected_model = "character",
   current = "logical"),
 contains = "Classing"
)

Scorecard$methods(initialize = function(..., seed=as.numeric(strftime(Sys.time(), format="%H%M%S"))) {
  seed <<- seed
  current <<- TRUE
  callSuper(...)
})

Scorecard$methods(has_model = function(model) {
  if (!model %in% names(models)) {
    stop("Requested model not found: ", model, call. = FALSE)
  }
})

Scorecard$methods(select = function(model, ...) {
  "select a model and load the transforms associated with it"

  has_model(model)

  ## save the current state in Head
  if (current) {
    HEAD <<- get_transforms(.self)
    current <<- FALSE
  }

  mod <- models[[model]]
  selected_model <<- model

  for (v in names(variables)) {
    variables[[v]]$tf <<- mod@transforms[[v]]
  }
})

Scorecard$methods(add_model = function(mod, ...) {
  models[[mod@name]] <<- mod
  select(mod@name)
})

Scorecard$methods(fit = function(name, description="", newdata=.self$get_variables(),
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

  set.seed(seed)
  this_fit <- cv.glmnet(x = x, y = y, weights = w, nfolds = nfolds,
                        alpha = alpha, upper.limits=upper.limits,
                        lower.limits=lower.limits, penalty.factor = pf,
                        exclude=which(step == 3), ...)

  ## store the last transforms
  m <- new("Model", name=name, description=description, fit=this_fit, step=step,
           transforms=get_transforms())

  add_model(m)

})

Scorecard$methods(show = function(...) {
  ## show the models / coefs?
  cat(sprintf("%d models", length(models)), sep="\n")
  i <- rep("", length(models))
  i[names(models) == selected_model] <- "*"
  cat(sprintf(" |-- %s %-2s  > %s", sapply(models, slot, "name"), i,
              sapply(models, slot, "description")), sep="\n")
})

Scorecard$methods(get_variables = function(...) {
  lapply(variables, function(b) b$x)
})

Scorecard$methods(predict = function(newdata=.self$get_variables(), type="score", ...) {

  woe <- callSuper(newdata=newdata, transforms=.self$get_transforms(), ...)

  if (type == "woe") {
    return(woe)
  }

  glmnet::predict.cv.glmnet(object=mod@fit, newx=woe, type="link")
})

## show summary for selected model
## NULL shows the most recently fit model
Scorecard$methods(summary = function(...) {
  if (length(models) == 0) {
    callSuper()
  } else {
    mod <- models[[selected_model]]
    cat("Model Summary: ", selected_model, "\n")
    callSuper(tfs=mod@transforms, step=mod@step)
  }
})

Scorecard$methods(resume = function(...) {
  "load the HEAD set of transforms and resume"

  for (v in names(variables)) {
    variables[[v]]$tf <<- HEAD[[v]]
  }
  current <<- TRUE
  selected_model <<- character(0)
})



