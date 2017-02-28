setClass("Model", slots = c(
  name = "character",
  description = "character",
  settings = "list",
  fit = "ANY",
  transforms = "list",
  step = "numeric",
  coefs = "numeric"))
