library(tidyverse)
library(here)
library(zoo)
library(fuzzyjoin)
library(here)
library(spaMM)
library(MASS)
library(lme4)
library(lmerTest)
library(sf)
sf_use_s2(FALSE)
library(raster)
library(caret)

rm(list = ls())

source(here("scripts", 'pars.R'))
source(here("scripts", "fns_ensemble.R"))
source(here("scripts", "fns_cluster_forecast.R"))

source(here("scripts", '1-dataprep.R'))
source(here("scripts", '2-candidate_models.R'))
source(here("scripts", '3-ensemble.R'))

## figs.Rmd creates all the tables and figures for the manuscript 