render_html <- function(scriptname, htmlname) { 
  rmarkdown::render(input = str_c("./Mercedes Benz/", scriptname), 
                    output_file = str_c("./Mercedes Benz/Output/", htmlname), 
                    knit_root_dir = ".")
}
render_html("1.001 Load Data.R", "1.001 Load Data.html")
render_html("1.002 Plot Data.R", "1.002 Plot Data.html")
render_html("1.003 Engineer Features.R", "1.003 Engineer Features.html")
render_html("1.004 Train Random Forest.R", "1.004 Train Random Forest 01.html")
render_html("1.005 Train XGBoost.R", "1.005 Train XGBoost 01.html")
