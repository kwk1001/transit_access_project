required_packages <- c(
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
  "r5r",
  "lubridate",
  "progress",
  "fs"
)

java_home_detect <- function() {
  if (.Platform$OS.type == "windows") {
    return(Sys.getenv("JAVA_HOME", unset = ""))
  }
  if (Sys.info()[["sysname"]] == "Darwin") {
    out <- tryCatch(system('/usr/libexec/java_home -v 21', intern = TRUE, ignore.stderr = TRUE), error = function(e) character())
    if (length(out) > 0) return(out[[1]])
  }
  Sys.getenv("JAVA_HOME", unset = "")
}

peek_java_config <- function(config_path, default_memory = "12G", default_active_processors = 2L) {
  out <- list(java_memory = default_memory, java_active_processors = default_active_processors)

  if (is.null(config_path) || !file.exists(config_path)) {
    return(out)
  }

  get_nested <- function(x, path, default = NULL) {
    cur <- x
    for (nm in path) {
      if (!is.list(cur) || is.null(cur[[nm]])) {
        return(default)
      }
      cur <- cur[[nm]]
    }
    cur
  }

  apply_values <- function(cfg_obj) {
    mem <- get_nested(cfg_obj, c("r5r_network", "java_memory"))
    ap <- get_nested(cfg_obj, c("r5r_network", "java_active_processors"))

    if (is.null(mem)) mem <- get_nested(cfg_obj, c("routing", "java_memory"))
    if (is.null(ap)) ap <- get_nested(cfg_obj, c("routing", "java_active_processors"))

    if (!is.null(mem) && nzchar(as.character(mem))) out$java_memory <<- as.character(mem)
    if (!is.null(ap) && is.finite(suppressWarnings(as.numeric(ap)))) out$java_active_processors <<- as.integer(ap)
  }

  ext <- tolower(tools::file_ext(config_path))

  if (ext == "r") {
    env <- new.env(parent = baseenv())
    try(sys.source(config_path, envir = env), silent = TRUE)
    cfg <- NULL
    if (exists("project_config", envir = env, inherits = FALSE)) cfg <- env$project_config
    if (is.null(cfg) && exists("cfg", envir = env, inherits = FALSE)) cfg <- env$cfg
    if (is.list(cfg)) {
      apply_values(cfg)
    }
    return(out)
  }

  if (ext %in% c("yml", "yaml")) {
    cfg_yaml <- NULL
    if (requireNamespace("yaml", quietly = TRUE)) {
      cfg_yaml <- tryCatch(yaml::read_yaml(config_path), error = function(e) NULL)
    }
    if (is.list(cfg_yaml)) {
      apply_values(cfg_yaml)
      return(out)
    }

    lines <- readLines(config_path, warn = FALSE, encoding = "UTF-8")
    active_section <- NULL
    for (ln in lines) {
      if (grepl("^[A-Za-z0-9_]+\\s*:\\s*$", ln)) {
        active_section <- sub("\\s*:.*$", "", trimws(ln))
      }
      if (identical(active_section, "r5r_network") && grepl("^\\s+java_memory\\s*:", ln)) {
        mem <- trimws(gsub("[\"']", "", sub("^\\s*java_memory\\s*:\\s*", "", ln)))
        if (nzchar(mem)) out$java_memory <- mem
      }
      if (identical(active_section, "r5r_network") && grepl("^\\s+java_active_processors\\s*:", ln)) {
        ap <- suppressWarnings(as.integer(trimws(sub("^\\s*java_active_processors\\s*:\\s*", "", ln))))
        if (!is.na(ap) && ap > 0) out$java_active_processors <- ap
      }
    }
  }

  out
}

configure_java_for_r5r <- function(java_memory = "12G", java_active_processors = 2L, force = FALSE) {
  if ("rJava" %in% loadedNamespaces()) {
    return(invisible(FALSE))
  }

  if (is.null(java_memory) || !nzchar(as.character(java_memory))) java_memory <- "12G"
  java_memory <- as.character(java_memory)
  if (is.null(java_active_processors)) java_active_processors <- 2L
  java_active_processors <- suppressWarnings(as.integer(java_active_processors))
  if (is.na(java_active_processors) || java_active_processors < 1) {
    java_active_processors <- 2L
  }

  desired <- c(
    paste0("-Xmx", java_memory),
    "-Djava.awt.headless=true",
    paste0("-XX:ActiveProcessorCount=", java_active_processors)
  )
  current <- getOption("java.parameters")
  current_chr <- if (length(current) > 0) as.character(current) else character()

  if (!force && length(current_chr) > 0 && identical(current_chr, desired)) {
    return(invisible(FALSE))
  }

  options(java.parameters = desired)
  invisible(TRUE)
}

build_r5r_help_message <- function() {
  jh <- java_home_detect()
  java_hint <- if (nzchar(jh)) {
    paste0('Detected JAVA_HOME or JDK path: ', jh)
  } else {
    paste(
      'No usable Java Development Kit 21 was detected.',
      'r5r requires JDK 21.',
      'On macOS, install a JDK first, restart R, then run install.packages(c("rJava","r5r")).'
    )
  }

  paste(
    'Package r5r is unavailable in this R session.',
    java_hint,
    'Official r5r docs require Java Development Kit 21.',
    'The r5r docs also note that Java memory must be set before loading r5r, using options(java.parameters = "-Xmx...").',
    'If install.packages("r5r") fails, install rJava first after Java is available.',
    sep = ' '
  )
}

load_required_packages <- function() {
  missing_pkgs <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]

  if (length(missing_pkgs) > 0) {
    msg <- paste0(
      'Missing required packages: ',
      paste(missing_pkgs, collapse = ', '),
      '. Install them before running the pipeline.'
    )
    if ('r5r' %in% missing_pkgs) {
      msg <- paste(msg, build_r5r_help_message())
    }
    stop(msg, call. = FALSE)
  }

  suppressPackageStartupMessages({
    library(tidyverse)
    library(sf)
    library(tigris)
    library(yaml)
    library(jsonlite)
    library(janitor)
    library(readxl)
    library(leaflet)
    library(htmlwidgets)
    library(htmltools)
    library(tidytransit)
    library(r5r)
    library(lubridate)
    library(progress)
    library(fs)
  })
}
