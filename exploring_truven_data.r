

# rm(list = ls())

# Import Packages
library(tidyr)
library(dplyr)
library(purrr)
library(haven)
library(readxl)

f1 <- "EOAMainCohort.xlsx"

EOAMainCohort <- read_excel("Latest Data/EOAMainCohort.xlsx")


drugfilecombined <- read_sas("Latest Data/drugfilecombined.sas7bdat", NULL)
drugfilecombined_head <- head(drugfilecombined)

colnames(drugfilecombined) %>% View()

drugfilecombined_head %>% View()


EOAMainCohort$PROCTYP %>% as.numeric() %>% summary()


ndc_codes <- read.csv()


####
s <- d2 %>% slice_sample(n = 1000)
View(s)

# Procedure year variable
s <- s %>% mutate(procedure_year = format.Date(SVCDATEProcedure, "%Y"))

# Group by procedure_year and extract stats -- for example, age
s %>% group_by(procedure_year) %>% select(procedure_year, AGE) %>% summarise(mean(AGE))

# Group by procedure_year and extract stats -- for example, age
s %>% group_by(procedure_year) %>% select(procedure_year, AGE) %>% summarise(mean(AGE))

