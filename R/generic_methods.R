
setGeneric("update_", def = function(.self, b, ...) callGeneric("update_"))

setGeneric("bin_", def = function(.self, b, ...) callGeneric("bin_"))

setGeneric("neutralize_", function(tf, i, ...) callGeneric("neutralize_"))

setGeneric("plot_", function(.self, b, ...) callGeneric("plot_"))

# setGeneric("predict_", function(.self, newdata, ...) callGeneric("predict_"))
