# Set up library computer for this project

# Need to install git each time :(
# https://git-scm.com/download/win
# Seems ok though? 

# Install rtools :( 
# https://cran.r-project.org/bin/windows/Rtools/rtools40.html

# Update installed packages
# options(pkgType = "binary") 

update.packages(ask = FALSE)

install.packages("rlang")
install.packages("tidyverse")

# Install packages
p = c(
  "broom", 
  "dbplyr", 
  "dtplyr", 
  "forcats", 
  "googledrive", 
  "googlesheets4", 
  "hms",
  "modelr",
  "pillar",
  "reprex", 
  "rvest",
  "stringr",
  "tidyr", 
  "dplyr", 
  "purrr", 
  "readr", 
  "ggplot2",
  "gitcreds")
install.packages(p)

# Set up gitcreds
gitcreds_set(url = "https://github.com/vapisharody")

# Check out this repo
# https://github.com/vapisharody/eoa
# to THIS location
# C:/Users/vpishar/Desktop


##
             