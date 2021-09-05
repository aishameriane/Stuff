# Reproducible example for shap.plot.summary()

library(SHAPforxgboost)
library(data.table)
library(ggplot2)


# Download the file from here: https://github.com/aishameriane/Stuff/blob/master/shap_long_complete.Rds
shap_long_complete <- readRDS(file = "shap_long_complete.Rds")

# Original plot
shap.plot.summary(shap_long_complete)


# Modifies the plot function from the package
shap.plot.summary.alt <- function (data_long, x_bound = NULL, dilute = FALSE, scientific = FALSE, 
                                   my_format = NULL) 
{
  if (scientific) {
    label_format = "%.1e"
  }
  else {
    label_format = "%.3f"
  }
  if (!is.null(my_format)) 
    label_format <- my_format
  N_features <- setDT(data_long)[, uniqueN(variable)]
  if (is.null(dilute)) 
    dilute = FALSE
  nrow_X <- nrow(data_long)/N_features
  if (dilute != 0) {
    dilute <- ceiling(min(nrow_X/10, abs(as.numeric(dilute))))
    set.seed(1234)
    data_long <- data_long[sample(nrow(data_long), min(nrow(data_long)/dilute, 
                                                       nrow(data_long)/2))]
  }
  x_bound <- if (is.null(x_bound)) 
    max(abs(data_long$value)) * 1.1
  else as.numeric(abs(x_bound))
  plot1 <- ggplot(data = data_long) + 
    coord_flip(ylim = c(-x_bound, x_bound)) + 
    geom_hline(yintercept = 0) + 
    ggforce::geom_sina(aes(x = variable, y = value, color = stdfvalue), method = "counts", maxwidth = 0.7, alpha = 0.7) + 
    geom_text(data = unique(data_long[, c("variable", "mean_value")]), aes(x = variable, y = -Inf, label = sprintf(label_format, mean_value)), size = 3, alpha = 0.7, hjust = -0.2, fontface = "bold", check_overlap = TRUE) + 
    scale_color_gradient(low = "#FFCC33", high = "#6600CC", 
                         breaks = c(0, 1), labels = c(" Low", "High "), guide = guide_colorbar(barwidth = 12, 
                                                                                               barheight = 0.3)) + theme_bw() + theme(axis.line.y = element_blank(), 
                                                                                                                                      axis.ticks.y = element_blank(), legend.position = "bottom", 
                                                                                                                                      legend.title = element_text(size = 10), legend.text = element_text(size = 8), 
                                                                                                                                      axis.title.x = element_text(size = 10)) + scale_x_discrete(limits = rev(levels(data_long$variable)), 
                                                                                                                                                                                                 labels = label.feature.custom(rev(levels(data_long$variable)))) + 
    labs(y = "SHAP value (impact on model output)", x = "", 
         color = "Feature value  ")
  return(plot1)
}

# Only could make it work by copying this and giving a different name to change in shap.plot.summary.alt()

label.feature.custom <- function(x){
  # a saved list of some feature names that I am using
  labs <- SHAPforxgboost::labels_within_package
  # but if you supply your own `new_labels`, it will print your feature names
  # must provide a list.
  if (!is.null(new_labels)) {
    if(!is.list(new_labels)) {
      message("new_labels should be a list, for example,`list(var0 = 'VariableA')`.\n")
    }  else {
      message("Plot will use your user-defined labels.\n")
      labs = new_labels
    }
  }
  out <- rep(NA, length(x))
  for (i in 1:length(x)){
    if (is.null(labs[[ x[i] ]])){
      out[i] <- x[i]
    }else{
      out[i] <- labs[[ x[i] ]]
    }
  }
  return(out)
}

# Makes the modified plot
shap.plot.summary.alt(shap_long_complete)


