## print progress update bar to console
progress_ <- function(i, max, text = "Progress", extra="") {
  progress <- paste(rep("=", (10*i/max)), collapse="")
  cat(sprintf("\r%s : %-10s| %-50s", text, progress, extra))
}

