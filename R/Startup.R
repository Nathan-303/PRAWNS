#' Load in all ther packages as a jerry rigged fix to figuring out namespace
#'
#' @export
#'
#' @examples
#' Startup()

Startup <- function(){
library(sf)
library(tidyverse)
library(terra)
library(exactextractr)
library(rlang)
library(broom)
library(ggpubr)
library(viridis)
library(stars)
library(ragg)
library(raster)
library(scales)
}
