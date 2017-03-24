
Discrete <- setRefClass("Discrete", contains = "Bin")

Discrete$methods(initialize = function(x, ...) {
  if(any(levels(x) %in% "")) stop("Factor variable contains blanks")
  callSuper(x=x, ...)
})

Discrete$methods(collapse = function(v) {
  f <- which(tf@tf %in% unique(tf@tf)[v]) ## which values were selected for collapse?
  tf@tf[f] <<- paste(names(tf@tf)[f], collapse=',') # collapse them with commas
  callSuper()
})

Discrete$methods(expand = function(v) {
  f <- tf@tf %in% unique(tf@tf)[v]
  tf@tf[f] <<- levels(x)[f]
  callSuper()
})

Discrete$methods(factorize = function(newdata=.self$x, transform=.self$tf, ...) {
  f <- callSuper(newdata, ...)

  out <- newdata
  levels(out) <- unlist(transform@tf)[levels(out)]
  out <- addNA(out)
  levels(out)[is.na(levels(out))] <- "Missing"

  list(factor=out, types=f)
})

Discrete$methods(predict = function(newdata=.self$x, transform=.self$tf, ...) {
  stopifnot(is.factor(newdata))
  callSuper(newdata=newdata, transform=transform, ...)
})


Discrete$methods(gen_code_sas = function(pfx="", coef=1, method="min", i=1, ...) {

  val <- gsub(",", "','", names(tf@subst))
  p <- tf@subst * coef
  ref <- switch(method,"min" = min(p), "max" = max(p), "neutral" = 0)
  m <- if (length(tf@nas) == 0) 0 else tf@nas * coef

  ## WoE Substitution
  c(sprintf("\n/*** %s ***/", name),
    sprintf("if missing(%s)\n  then %s_V%02d_w = %s;", name, pfx, i, m),
    sprintf("else if %s in ('%s')\n  then %s_V%02d_w = %s;",
      name, val, pfx, i, p),
    sprintf("else %s_V%02d_w = 0;", pfx, i),

    ## AA Code
    sprintf("\nif missing(%s)\n  then %s_AA_code_%02d = \"&%s_AA_%02d\";",
      name, pfx, i, pfx, i),
    sprintf("else if %s in ('%s')\n  then %s_AA_code_%02d = \"&%s_AA_%02d\";",
      name, val, pfx, i, pfx, i),
    sprintf("else %s_AA_code_%02d = \"&%s_AA_%02d\";", pfx, i, pfx, i),

    ## AA Dist
    sprintf("\n%s_AA_dist_%02d = %s - %s_V%02d_w;", pfx, i, ref, pfx, i))
})





