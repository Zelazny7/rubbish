setClass("Model", slots = c(
  name = "character",
  settings = "list",
  fit = "ANY",
  transforms = "list",
  step = "numeric",
  coefs = "numeric"))
