# Dependency management setup.

here::i_am("common-setup.R")

# Make sure at least one version of ggrrr is installed
# if (!"ggrrr" %in% rownames(installed.packages())) { devtools::install_github("terminological/ggrrr") }
# # make sure ggrrr is using local development version if it exists.
# ggrrr::unstable()

# install specific release for stability purposes
devtools::install_github("terminological/html2pdfr@0.4.1", build_opts = c("--no-multiarch"),quiet = TRUE,upgrade = FALSE)
devtools::install_github("terminological/ggrrr@0.0.0.9009", build_opts = c("--no-multiarch"),quiet = TRUE,upgrade = FALSE)

library(ggrrr)

## manage dependencies ----
# cran packages

cran(c("here","tidyverse","devtools","lubridate","huxtable","patchwork","openxlsx","rstatix"))
library(tidyverse)
library(lubridate)
library(patchwork)

# non cran packages
# this will check to see if it is checked out for local development or load it from github.
# non_cran("dtrackr","terminological/dtrackr@0.2.1",force=TRUE)

# devtools::install_github("terminological/dtrackr@0.2.1")
# devtools::install_local("~/Git/dtrackr")
cran("dtrackr")
library(dtrackr)

## standard functions ----
source(here::here("data-wrangling.R"))
# source(here::here("qcovid2.R"))
source(here::here("qcovid2.R"))
source(here::here("descriptive-tables.R"))

# Sets ggplot defaults
ggrrr::gg_pedantic(font="Arial")

# These override base message and warning and direct them to rlang::inform and rlang::warn respectively.
# This may be a terrible idea.
# message = function(...) {
#   l = rlang::list2(...)
#   msg = paste0(lapply(l,as.character),collapse = "")
#   msg = paste0(unname(msg),"\n")
#   rlang::inform(msg,.frequency = "regularly", .frequency_id = msg)
# }

# warning = function(...) {
#   l = rlang::list2(...)
#   msg = paste0(lapply(l,as.character),collapse = "")
#   msg = paste0(unname(msg),"\n")
#   rlang::warn(msg,.frequency = "regularly", .frequency_id = msg)
# }

## Glue formatting functions

fdmy = function(date) format(date,"%d %b %Y")
