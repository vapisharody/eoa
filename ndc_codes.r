# R code to pull all the NDC codes for a list of antibiotics
# Vivek Pisharody
# 12/19/2023

rm(list = ls())

# Import Packages
library(tidyr)
library(dplyr)
library(purrr)
library(httr)
library(jsonlite)

# Note: generic_name vs generic_name.exact pitfalls
abx <- c("Amoxicillin",
         "amoxicillin",
         "Amoxicillin+and+Clavulanate+Potassium",
         "amoxicillin+and+clavulanate+potassium",
         "Azithromycin",
         "azithromycin",
         "Cefadroxil",
         # "cefadroxil", #nothing
         "Cefdinir",
         "cefdinir",
         "Cephalexin",
         "cephalexin",
         "Ciprofloxacin",
         "ciprofloxacin",
         # "Ciproxin",  #No ciprofloxacin product listed under this in the FDA database
         "Clindamycin+hydrochloride",
         "clindamycin+hydrochloride",
         # "Clindamycin+phosphate", #Found only in non-oral forms
         # "clindamycin+phosphate", #Found only in non-oral forms
         #"Clindamycin+nicotinamide", #None found
         #"Cotrimoxazole", #No TMP-SMX product listed under this in the FDA database
         "Sulfamethoxazole+and+Trimethoprim",
         "sulfamethoxazole+and+trimethoprim",
         "Doxycycline",
         "doxycycline",
         "Doxycycline+hyclate",
         "doxycycline+hyclate",
         # "Doxycycline+monohydrate", #nothing found
         # "doxycycline+monohydrate",
         "Minocycline",
         #"minocycline",
         "Minocycline+hydrochloride",
         "minocycline+hydrochloride")

# abx_long <- purrr::reduce(abx, paste, sep = "+OR+")
# abx_long

make_query <- function(t){
  paste0("https://api.fda.gov/drug/ndc.json?search=route.exact:\"ORAL\"+AND+generic_name.exact:(",
         t,
         ")&limit=1000") 
}

get_ndcs <- function(q){
  a <- GET(q)
  b <- fromJSON(rawToChar(a$content)) 
  c <- b$results
  return(c$product_ndc)
}

ndcs_from_text <- function(abx_name){ abx_name %>% make_query() %>% get_ndcs }

# Pull NDC codes
ndcs_list <- purrr::map(abx, ndcs_from_text)
names(ndcs_list) <- abx

# Write to a combined file with one row per antibiotic
a <- purrr::map(ndcs_list, function(x){paste(x, collapse = ", ")}) %>% unlist()
write.csv(a, "ndc_codes.csv")

# -------
# COMMENTED OUT -- option to write to separate files

# Write to separate files for each antibiotic w one row for each NDC code
# temp <- function(x){paste0(x, ".csv")}
# filenames <- lapply(abx, temp) %>% unlist()
# 
# pwalk(list(ndcs_list, filenames), write.csv)

# -------

# Resources
# https://community.rstudio.com/t/is-there-a-way-to-get-data-from-openfda-via-their-api/80725/2
# https://r4ds.had.co.nz/iteration.html?q=walk%20#walk

# Check that a given NDC is what you expect: 
# https://api.fda.gov/drug/ndc.json?search=product_ndc:0143-9285&limit=1000