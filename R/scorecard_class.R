#' @include classing_class.R
#' @include classing_adjust_method.R

#' @export Scorecard
#' @exportClass Scorecard
Scorecard <- setRefClass("Scorecard",
 fields = c(
   seed = "numeric",
   models = "list",
   selected_model = "character",
   inmodel = "character"),
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
  HEAD <- new("Model", name="scratch", description="", fit=NULL, ks=0,
              dropped=dropped, transforms=get_transforms())
  add_model(HEAD)
})


Scorecard$methods(fit = function(name, description="", newdata=.self$get_variables(),
  y=performance$y, w=performance$w, nfolds=5, upper.limits=3, lower.limits=0,
  alpha=1, family="binomial", ...) {

  ## check names of newdata here... TODO

  ## check for consistent dimensions
  stopifnot(length((newdata[[1]]) == length(y)) &&  (length(y) == length(w)))

  if (name %in% names(models)) {
    ans <- menu(c("Yes", "No"), title = "Model name already exists. Overwrite?")
    if (ans == "No") break
  }

  v <- setdiff(vnames, dropped)
  x <- predict(newdata=newdata[v], type="woe")

  set.seed(seed)
  this_fit <- cv.glmnet(x = x, y = y, weights = w, nfolds = nfolds,
    family=family, alpha=alpha, upper.limits=upper.limits,
    lower.limits=lower.limits, keep=TRUE, ...)

  ## get the coeficients
  coefs <- coef(this_fit, s="lambda.min")[,1]
  coefs <- coefs[which(coefs != 0)]

  ## set the inmodel vector
  inmodel <<- names(coefs)[-1]

  ## performance metrics
  contr <- contributions_(x[,names(coefs)[-1]], coefs, y, w)
  ks <- ks_(this_fit$fit.preval[,which.min(this_fit$cvm)], y, w) # kfold

  ## store the last transforms
  m <- new("Model", name=name, description=description, fit=this_fit,
           dropped=dropped, transforms=get_transforms(), coefs=coefs,
           inmodel=inmodel, contribution=contr, ks=ks)

  add_model(m)

})

Scorecard$methods(show = function(...) {
  ## show the models / coefs?
  cat(sprintf("%d models", length(models)), sep="\n")
  i <- rep("", length(models))
  i[names(models) == selected_model] <- "*"
  cat(sprintf(" |-- %-2s %-20s | %04.1f ks | %s", i,
              sapply(models, slot, "name"),
              sapply(models, slot, "ks") * 100,
              sapply(models, slot, "description")), sep="\n")
})

Scorecard$methods(predict = function(newdata=NULL, keep=FALSE, type="score", ...) {

  woe <- callSuper(newdata=newdata, keep=keep)

  if (type == "woe") return(woe)

  mod <- models[[selected_model]]
  v <- names(mod@coefs[-1])

  woe[,v] %*% mod@coefs[v] + mod@coefs[1]
})

## show summary for selected model
Scorecard$methods(summary = function(...) {

  mod <- models[[selected_model]]

  cat(mod@name, "\nOut-of-Fold KS: ", mod@ks, "\n")

  res <- callSuper(tfs = get_transforms(keep=TRUE))
  vars <- row.names(res)


  out <- cbind(res, `In Model` = 0, `Coefs` = mod@coefs[vars],
    `Contribution` = mod@contribution[vars])

  out[inmodel,"In Model"] <- 1
  out
})

Scorecard$methods(adjust = function(...) {
  callSuper(...)
})

Scorecard$methods(sort = function(...) {

  v <- setNames(sapply(variables, function(x) x$sort_value()), names(variables))

  im <- setNames(rep(0, length(v)), names(v))
  dr <- setNames(rep(0, length(v)), names(v))

  im[inmodel] <- 1
  dr[dropped] <- 1

  i <- order(im, -dr, v, decreasing = TRUE, na.last = TRUE)

  variables <<- variables[i]

})

Scorecard$methods(pseudo_pvalues = function(times=20, bag.fraction = 0.50,
  replace=FALSE,  nfolds=5, upper.limits=3, lower.limits=0, alpha=1, ...) {

  x <- predict(newdata=get_variables(), type="woe")

  coefs <- list()
  for (i in seq.int(times)) {
    progress_(i, times, "Fitting")

    s <- sample.int(nrow(x), nrow(x)*bag.fraction, replace = replace)

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

Scorecard$methods(compare = function(...) {
  mods <- unlist(list(...))

  ## check that requested models are in the scorecard
  stopifnot(all(mods %in% names(models)))

  on.exit(select(selected_model))

  ## select each and get the summary
  summaries <- lapply(mods, function(x) {

    select(x)
    res <- summary()
    res[,c("IV","Dropped","In Model","Coefs","Contribution")]

  })

  ## merge them all
  contribution <- lapply(summaries, function(x) x[,"Contribution"])
  coefficients <- lapply(summaries, function(x) x[,"Coefs"])

  # merge helper for use with Reduce
  merge_ <- function(a, b) {
    tmp <- merge(a,b, by=0, all=T)
    row.names(tmp) <- tmp$Row.names
    subset(tmp, select = -Row.names)
  }

  res <- merge(
    Reduce(merge_, contribution),
    Reduce(merge_, coefficients), by=0, all=T)

  cols <- c("Contribution", "Coefficients")
  colnames(res) <- c("Variable", paste(rep(cols, each=length(mods)), mods))

  res[order(-res[2], na.last = TRUE),]

})


Scorecard$methods(gen_code_sas = function(pfx="", method="min", ...) {

  mod <- models[[selected_model]]

  v <- names(which(mod@inmodel))
  coefs <- mod@coefs[-1][v]

  ## Print the reason code mappings
  out <- "/** Adverse Action Code Mappings **/"
  out <- c(out, lapply(seq_along(v), function(i) {
    sprintf("%%let %s_AA_%02d = \"\"; /** %s **/", pfx, i, v[i])
  }))

  ### Print the variables
  out <- c(out, lapply(seq_along(v), function(i) {
    variables[[v[i]]]$gen_code_sas(method=method, pfx=pfx, coef=coefs[i])
  }))

  out <- c(out,
    sprintf("\n/*** Final Score Calculation ***/"),
    sprintf("%s_lgt = %s", pfx, mod@coefs[1]),
    sprintf("  + %s_V%02d_w", pfx, seq_along(v)),
    ";")

  unlist(out)

})


Scorecard$methods(get_dropped = function(invert=FALSE) {
  if (invert) setdiff(vnames, dropped) else dropped
})

Scorecard$methods(get_inmodel = function(invert=FALSE) {
  if (invert) setdiff(vnames, inmodel) else inmodel
})



