render_html <- function(scriptname, htmlname) { 
  rmarkdown::render(input = str_c("./07-mercedes-benz/", scriptname), 
                    output_file = str_c("./07-mercedes-benz/output/", htmlname), 
                    knit_root_dir = ".")
}
render_html("01-load-data.R", "01-load-data.html")
render_html("02-plot-data.R", "02-plot-data.html")
render_html("03-engineer-features.R", "03-engineer-features.html")
render_html("04-train-random-forest.R", "04-train-random-forest.html")
render_html("05-train-gbm.R", "05-train-gbm.html")
render_html("06-train-xgboost.R", "06-train-xgboost.html")
render_html("07-average-predictions.R", "07-average-predictions.html")
