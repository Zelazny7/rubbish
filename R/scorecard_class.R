#' @include classing_class.R
#' @include classing_adjust_method.R

#' @export Scorecard
#' @exportClass Scorecard
Scorecard <- setRefClass("Scorecard",
 fields = c(
   seed = "numeric",
   models = "list",
   selected_model = "character",
   inmodel = "logical"),
 contains = "Classing")

Scorecard$methods(initialize = function(..., seed=as.numeric(strftime(Sys.time(), format="%H%M%S"))) {
  seed <<- seed
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
  mod <- models[[model]]
  selected_model <<- model
  dropped <<- mod@dropped
  inmodel <<- mod@inmodel

  for (v in names(mod@transforms)) {
    variables[[v]]$tf <<- mod@transforms[[v]]
  }
})

Scorecard$methods(add_model = function(mod, ...) {
  models[[mod@name]] <<- mod
  select(mod@name)
})

Scorecard$methods(bin = function(...) {
  callSuper(...)
  HEAD <- new("Model", name="HEAD", description="", fit=NULL, dropped=dropped,
              transforms=get_transforms())
  add_model(HEAD)
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

  x <- predict(newdata=newdata, type="woe")

  set.seed(seed)
  this_fit <- cv.glmnet(x = x, y = y, weights = w, nfolds = nfolds,
                        alpha = alpha, upper.limits=upper.limits,
                        lower.limits=lower.limits, ...)

  ## get the coeficients
  coefs <- coef(this_fit, s="lambda.min")[,1]

  ## set the inmodel vector
  inmodel <<- setNames(logical(length(variables)), names(variables))
  inmodel[names(which(coefs[-1] != 0))] <<- TRUE

  ## store the last transforms
  m <- new("Model", name=name, description=description, fit=this_fit,
           dropped=dropped, transforms=get_transforms(), coefs=coefs,
           inmodel=inmodel)

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

Scorecard$methods(predict = function(newdata=.self$get_variables(), type="score", ...) {

  woe <- callSuper(newdata=newdata, transforms=.self$get_transforms(), ...)

  if (type == "woe") {
    return(woe)
  }

  mod <- models[[selected_model]]
  glmnet::predict.cv.glmnet(object=mod@fit, newx=woe, type="link")
})

## show summary for selected model
Scorecard$methods(summary = function(...) {

  cat("Model Summary: ", selected_model, "\n")
  res <- callSuper(tfs = get_transforms(keep=TRUE))
  cbind(res, `In Model` = inmodel, `Coefs` = models[[selected_model]]@coefs[row.names(res)])
})

Scorecard$methods(adjust = function(...) {
  callSuper(...)
})

Scorecard$methods(sort = function(method=c("perf", "cluster", "alpha"), ...) {
  method <- match.arg(method)

  switch(
    method,
    "perf" = {
      v <- sapply(variables, function(x) x$sort_value())
      i <- order(inmodel, -dropped, v,
        decreasing = TRUE, na.last = TRUE)
    },
    {
      i <- seq_along(variables)
      print("not implemented")
    }
  )

  variables <<- variables[i]
  dropped <<- dropped[i]
  inmodel <<- inmodel[i]

})

Scorecard$methods(psuedo_pvalues = function(times=20, bag.fraction = 0.50,
  nfolds=5, upper.limits=3, lower.limits=0, alpha=1, ...) {

  x <- predict(newdata=get_variables(), type="woe")

  coefs <- list()
  for (i in seq.int(times)) {
    progress_(i, times, "Fitting")

    s <- sample.int(nrow(x), nrow(x)*bag.fraction)

    fit <- glmnet::cv.glmnet(x = x[s,], y = performance$y[s],
      weights = performance$w[s], nfolds = 10, alpha = alpha,
      upper.limits=upper.limits, lower.limits=lower.limits, keep=TRUE)

    coefs[[i]] <- coef(fit, s="lambda.min")
  }

  res <- as.matrix(do.call(cbind, coefs))

  ## what is the probability of the coefficient being zero?
  pvals <- sapply(apply(res, 1, ecdf), function(x) x(0))

  structure(
    list(
      pvalues = pvals,
      coefs = res),
    class = "psuedo_pvalues")
})
