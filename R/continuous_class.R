#' @include bin_class.R

#' @export Continuous
#' @exportClass Continuous
Continuous <- setRefClass("Continuous", fields=c(uniq="numeric"),
  contains = "Bin")

Continuous$methods(initialize = function(...) {
  callSuper(...)
  uniq <<- sort(unique(x))
})

Continuous$methods(collapse = function(v) {
    d <- unique(pmax(pmin(tail(v, -1), length(tf@tf) - 1), 2))
    tf@tf <<- tf@tf[-d]
    callSuper()
  }
)

Continuous$methods(expand = function(v) {

    #browser()
    stopifnot(length(v) == 1)

    f <- !(is.na(x) | x %in% as.numeric(names(tf@exceptions)))

    a <- min(max(1, v), length(tf@tf))  # can't be smaller than 1
    z <- max(min(v + 1, length(tf@tf)), a) # or larger than max els

    vals <- c(x[x > tf@tf[a] & x <= tf@tf[z] & f])

    q <- c(quantile(vals, seq(0.2, 0.8, 0.2))) # quintiles

    tf@tf <<- sort(unique(c(tf@tf, q)))
    callSuper()
  }
)


Continuous$methods(fmt_numeric_cuts = function(...) {
  l <- uniq[pmax(1, findInterval(tf@tf, uniq, all.inside = TRUE))]
  l <- round(l, 3)
  fmt <- sprintf("(%%%1$ds - %%%1$ds]", max(nchar(l)))
  sprintf(fmt, head(l, -1), tail(l, -1))
})


## Note: this should never be called directly by the user,
## it is used by the update performance functions to get the X-var
## summary data
Continuous$methods(factorize = function(newdata=.self$x, transform=.self$tf, ...) {

  f <- callSuper(newdata=newdata, transform=transform, ...)

  lbls <- fmt_numeric_cuts(transform@tf)
  out <- factor(newdata, exclude=NULL,
    levels=c(lbls, names(transform@exceptions), NA))

  levels(out)[is.na(levels(out))] <- "Missing"
  out[f$normal] <- cut(newdata[f$normal], transform@tf, include.lowest = T,
    labels = lbls)

  list(factor=out, types=f)
})

Continuous$methods(predict = function(newdata=.self$x, transform=.self$tf, ...) {
  stopifnot(is.numeric(newdata))
  callSuper(newdata=newdata, transform=transform, ...)
})


Continuous$methods(gen_code_sas = function(pfx="", coef=1, method="min", i=1, ...) {

  val <- tail(head(tf@tf, -1), -1)
  p <- tf@subst * coef
  ref <- switch(method,"min" = min(p), "max" = max(p), "neutral" = 0)

  E <- names(tf@exceptions)
  m <- if (length(tf@nas) == 0) 0 else tf@nas * coef
  e <- if (length(tf@exceptions) == 0) 0 else tf@exceptions * coef

  ## WoE Substitution
  c(sprintf("\n/*** %s ***/", name),
    sprintf("if missing(%s)\n  then %s_V%02d_w = %s;", name, pfx, i, m),
    sprintf("else if %s = %s\n  then %s_V%02d_w = %s;", name, E, pfx, i, e),
    sprintf("else if %s <= %s\n  then %s_V%02d_w = %s;", name, val, pfx, i,
      head(p, -1)),
    sprintf("else %s_V%02d_w = %s;" , pfx, i, tail(p, 1)),

    ## Reason Codes
    sprintf("\nif missing(%s)\n  then %s_AA_code_%02d = \"&%s_AA_%02d\";",
      name, pfx, i, pfx, i),
    sprintf("else if %s = %s\n  then %s_AA_code_%02d = \"&%s_AA_%02d\";",
      name, E, pfx, i, pfx, i),
    sprintf("else if %s <= %s\n  then %s_AA_code_%02d = \"&%s_AA_%02d\";",
      name, val, pfx, i, pfx, i),
    sprintf("else %s_AA_code_%02d = \"&%s_AA_%02d\";", pfx, i, pfx, i),

    ## Distance Calculations
    sprintf("\n%s_AA_dist_%02d = %s - %s_V%02d_w;", pfx, i, ref, pfx, i))
})
