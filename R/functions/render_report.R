render_report <- function() {
  log_info("Render report")
  rmarkdown::render("C:/Users/patos/Documents/ST4092_120442916/ProjectReport.Rmd", encoding = "UTF-8")
  log_info("Done")
}
