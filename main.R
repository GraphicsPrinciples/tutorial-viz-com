###################################
## Libraries 
###################################
library(tidyverse)
library(RxODE)
library(caTools)
library(rstanarm)
theme_set(theme_gray()) # undo rstan theme setting

library(ggplot2)
library(dplyr)
library(gridExtra)
library(data.table)
library(grid)
library(RColorBrewer)
library(lme4)
library(emmeans)


###################################
## Path to figure directory
###################################
fig_path = 'figures/'

##################################
## Set default theme             
##################################
source('utils/paper_theme.R')

##################################
## Image size file defaults
##################################
d_width <- 116.9
d_height <- 82.7
d_dpi <- 1000

##################################
## Set seed 
##################################
set.seed(12)

##################################
##  Run section 2
##################################
source('Section2/201_selection.R')
source('Section2/202_preattent.R')
source('Section2/203_preattent.R')
source('Section2/204_group.R')


##################################
##  Run section 4
##################################
source('Section4/401_case1.R')
source('Section4/402_case2.R')
source('Section4/403_case3.R')
source('Section4/404_case4.R')
