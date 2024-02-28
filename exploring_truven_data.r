

rm(list = ls())

# Import Packages
library(tidyr)
library(dplyr)
library(purrr)
library(haven)

# f <- "mdservices2016in.sas7bdat"
# 
# d <- read_sas(f)
# View(d)

f2 <- "ccdrugs2016.sas7bdat"

d2 <- read_sas(f2)
d2 <- d2 %>% mutate(ndc_numeric = as.numeric(NDCNUM))
View(d2)

d2$NDCNUM %>% n_distinct()
d2$ndc_numeric %>% n_distinct()

s <- d2 %>% slice_sample(n = 1000)
View(s)

s %>% mutate(procedure_year = format.Date(SVCDATEProcedure, "Y")) %>% select(procedure_year)

s %>% group_by(SVCDATEProcedure) %>% select(SVCDATEProcedure, AGE) %>% summarise(AGE)


