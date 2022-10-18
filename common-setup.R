# Dependency management setup.

here::i_am("common-setup.R")

options("avoncap.input" = here::here("input"))

# Make sure at least one version of ggrrr is installed
# if (!"ggrrr" %in% rownames(installed.packages())) { devtools::install_github("terminological/ggrrr") }
# # make sure ggrrr is using local development version if it exists.
# ggrrr::unstable()

options(repos = c(
  terminological = 'https://terminological.r-universe.dev',
  CRAN = 'https://cloud.r-project.org'))
# Download and install ggrrr in R

if (!rlang::is_installed("ggrrr")) install.packages('ggrrr')
if (!rlang::is_installed("html2pdfr")) install.packages('html2pdfr')

# install specific release for stability purposes
# devtools::install_github("terminological/html2pdfr@0.4.2", build_opts = c("--no-multiarch"),quiet = TRUE,upgrade = FALSE)
# devtools::install_github("terminological/ggrrr@0.0.0.9009", build_opts = c("--no-multiarch"),quiet = TRUE,upgrade = FALSE)
# devtools::install_github("bristol-vaccine-centre/avoncap", build_opts = c("--no-multiarch"),quiet = TRUE,upgrade = FALSE)

ggrrr::unstable()
ggrrr::unstable(pkg = "avoncap", org="bristol-vaccine-centre")

library(ggrrr)
library(avoncap)

## manage dependencies ----
# cran packages

cran(c("here","tidyverse","devtools","lubridate","huxtable","patchwork","openxlsx","rstatix"))
library(tidyverse)
library(lubridate)
library(patchwork)
library(dtrackr)

# Sets ggplot defaults
ggrrr::gg_pedantic(font="Roboto")

## Glue formatting functions

.fdmy = function(date) format(date,"%d %b %Y")
