render_report <- function() {
  log_info("Render report")
  path <- file.path("C:/Users/patos/Documents/ST4092_120442916/")
  rmarkdown::render(file.path(path, "ProjectReport.Rmd"), encoding = "UTF-8")
  log_info("Done")
}
