cran_repo <- "https://cloud.r-project.org"
core_pkgs <- c(
  "tidyverse",
  "sf",
  "tigris",
  "yaml",
  "jsonlite",
  "janitor",
  "readxl",
  "leaflet",
  "htmlwidgets",
  "htmltools",
  "tidytransit",
  "lubridate",
  "progress",
  "fs"
)
java_pkgs <- c("rJava", "r5r")
helper_pkgs <- c("rJavaEnv")
all_pkgs <- c(core_pkgs, helper_pkgs)

installed <- rownames(installed.packages())
need <- setdiff(all_pkgs, installed)

if (length(need) > 0) {
  install.packages(need, repos = cran_repo)
}

sysname <- Sys.info()[["sysname"]]
java_ok <- FALSE
java_path <- ""

if (sysname == "Darwin") {
  probe <- tryCatch(system('/usr/libexec/java_home -v 21', intern = TRUE, ignore.stderr = TRUE), error = function(e) character())
  if (length(probe) > 0) {
    java_ok <- TRUE
    java_path <- probe[[1]]
    message('Detected macOS JDK 21 at: ', java_path)
  }
} else {
  java_path <- Sys.getenv('JAVA_HOME', unset = '')
  java_ok <- nzchar(java_path)
}

if (!java_ok) {
  message('Java Development Kit 21 was not detected.')
  message('r5r requires JDK 21. Install Java first, restart R, then run this script again.')
  message('Optional helper package rJavaEnv is installed. You can also try:')
  message('  install.packages("rJavaEnv")')
  message('  rJavaEnv::java_quick_install(version = 21)')
  quit(save = 'no', status = 1)
}

need_java_pkgs <- setdiff(java_pkgs, rownames(installed.packages()))
if ('rJava' %in% need_java_pkgs) {
  install.packages('rJava', repos = cran_repo)
}
if ('r5r' %in% need_java_pkgs) {
  install.packages('r5r', repos = cran_repo)
}

message('Package installation check complete.')
