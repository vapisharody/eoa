

rm(list = ls())

# Import Packages
library(tidyr)
library(dplyr)
library(purrr)
library(haven)

# f1 <- "EOAMainCohort.xlsx"
f1 <- "EOAMainCohort_head.xlsx"

coltypes = c("numeric", "numeric", "numeric", 
              "numeric", "numeric", "date", "numeric", 
              "numeric", "numeric", "numeric", 
              "numeric", "numeric", "numeric", 
              "numeric", "numeric", "numeric", 
              "numeric", "text", "numeric", "numeric", 
              "text", "numeric", "numeric", "text", 
              "text", "numeric", "numeric", "numeric", 
              "numeric", "numeric", "text", "text", 
              "numeric", "numeric", "text", "numeric", 
              "text", "numeric", "text", "numeric", 
              "numeric", "date", "numeric", "date", 
              "text", "date", "date", "numeric", 
              "numeric", "numeric", "numeric", 
              "numeric", "numeric", "numeric", 
              "numeric", "numeric", "numeric", 
              "numeric", "numeric", "numeric", 
              "numeric", "numeric", "numeric", 
              "numeric", "numeric", "numeric", 
              "numeric", "numeric", "numeric", 
              "numeric", "numeric", "numeric", 
              "numeric", "numeric", "numeric", 
              "numeric", "numeric", "numeric", 
              "numeric", "numeric", "numeric", 
              "numeric", "numeric", "numeric", 
              "numeric", "numeric", "numeric", 
              "numeric", "numeric", "numeric", 
              "numeric", "numeric", "numeric", 
              "numeric", "numeric", "numeric", 
              "numeric", "numeric", "numeric", 
              "numeric", "numeric", "numeric", 
              "numeric", "numeric", "numeric", 
              "numeric", "numeric", "numeric", 
              "numeric", "numeric", "numeric", 
              "numeric", "numeric", "numeric", 
              "numeric", "numeric", "numeric", 
              "numeric", "numeric", "numeric", 
              "text", "text", "numeric", "numeric", 
              "numeric", "numeric", "numeric", 
              "numeric", "numeric", "numeric", 
              "numeric", "numeric", "numeric", 
              "numeric", "numeric", "numeric", 
              "numeric", "numeric", "numeric", 
              "numeric", "numeric", "numeric", 
              "numeric", "numeric", "numeric", 
              "numeric", "numeric", "numeric", 
              "numeric", "numeric", "numeric", 
              "numeric", "numeric", "numeric", 
              "numeric", "numeric", "numeric", 
              "numeric", "numeric", "numeric", 
              "numeric", "numeric", "numeric", 
              "numeric", "numeric", "numeric", 
              "numeric", "numeric", "numeric", 
              "numeric", "numeric", "date", "numeric", 
              "numeric", "numeric", "numeric", 
              "numeric", "numeric", "numeric", 
              "numeric", "numeric", "numeric", 
              "numeric", "numeric", "numeric", 
              "numeric", "numeric", "numeric", 
              "numeric", "numeric", "numeric", 
              "numeric", "numeric", "numeric", 
              "numeric", "numeric", "numeric", 
              "text", "text", "numeric", "numeric", 
              "numeric", "numeric", "numeric",
              "numeric", "numeric", "numeric", 
              "numeric", "numeric", "numeric", 
              "numeric", "numeric", "numeric", 
              "numeric", "numeric", "numeric", 
              "numeric", "numeric", "numeric", 
              "numeric", "numeric", "numeric", 
              "date", "date", "numeric", "numeric", 
              "numeric", "numeric", "date", "numeric", 
              "numeric", "numeric")


library(readxl)
EOAMainCohort_head <- read_excel("Latest Data/EOAMainCohort_head.xlsx", col_types = coltypes)

View(EOAMainCohort_head)

f2 <- "ccdrugs2016.sas7bdat"

d2 <- read_sas(f2)
d2 <- d2 %>% mutate(ndc_numeric = as.numeric(NDCNUM))
View(d2)

d2$NDCNUM %>% n_distinct()
d2$ndc_numeric %>% n_distinct()

s <- d2 %>% slice_sample(n = 1000)
View(s)

# Procedure year variable
s <- s %>% mutate(procedure_year = format.Date(SVCDATEProcedure, "%Y"))

# Group by procedure_year and extract stats -- for example, age
s %>% group_by(procedure_year) %>% select(procedure_year, AGE) %>% summarise(mean(AGE))

# Group by procedure_year and extract stats -- for example, age
s %>% group_by(procedure_year) %>% select(procedure_year, AGE) %>% summarise(mean(AGE))

