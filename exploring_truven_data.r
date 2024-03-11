

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
# drugfilecombined_head <- head(drugfilecombined)

ndc_codes <- read.csv("ndc_codes_list.csv")

# Convert to 11-digit NDC codes.
# Need to pad with a zero in the right location
ndc_codes_split <- ndc_codes_split %>%
  mutate(a_n = nchar(ndc_code_a)) %>%
  mutate(b_n = nchar(ndc_code_b)) %>%
  mutate(c_n = nchar(ndc_code_c)) %>%
  mutate(a_pasted = paste0("0", ndc_code_a)) %>%
  mutate(b_pasted = paste0("0", ndc_code_b)) %>%
  mutate(c_pasted = paste0("0", ndc_code_c)) %>%
  mutate(a_converted = case_when(a_n == 4 ~ a_pasted,
                                 a_n == 5 ~ ndc_code_a)) %>%
  mutate(b_converted = case_when(b_n == 3 ~ b_pasted,
                                 b_n == 4 ~ ndc_code_b)) %>%
  mutate(c_converted = case_when(c_n == 1 ~ c_pasted,
                                 c_n == 2 ~ ndc_code_c)) %>% 
  mutate(ndc_code_final_dashed = paste0(a_converted, "-", b_converted, "-", c_converted)) %>% 
  mutate(ndc_code_final = paste0(a_converted, b_converted, c_converted))

ndc_codes_final <- ndc_codes_split %>% select(ndc_code_final)

## Select records from the drug file with with NDC codes of interest.
EOadrug1 <- drugfilecombined %>% filter(NDCNUM %in% ndc_codes_final$ndc_code_final)


#Filter antibiotics given within 7 days before surgery 
EOApreopfills<-EOadrug1%>%
  mutate(SVCDATEDrug = as.Date(SVCDATEDrug, format = "%m/%d/%Y"),
         SVCDATEProcedure = as.Date(SVCDATEProcedure, format = "%m/%d/%Y"))%>%
  mutate(daysbefore = as.numeric(difftime(SVCDATEProcedure, SVCDATEDrug, units = "days")))%>%
  filter(daysbefore > 0 & daysbefore < 8)


#Include antibiotics filled on day of surgery 

#First link Length of stay to postop fills 
Lengthofstay<- EOAMainCohort %>%
  select(ENROLID, SVCDATE, LOS) %>%
  replace_na(list (LOS = 0))

#Rename SVCDATEProcedure
EOadrug1 <- EOadrug1 %>% mutate(SVCDATE = SVCDATEProcedure)

#### WARNING: Many-to-many matches.
### Example
### (EOAMainCohort %>% filter(ENROLID == 1556115402))[,124:127] %>% View()
#Join LOS to drug file 
#If Na will use 0 so day of discharge is day of surgery 
EOAdrug2<-left_join(EOadrug1, Lengthofstay, by = c("ENROLID", "SVCDATE"))%>%
  replace_na(list(LOS = 0))

#Include postopfills on day of discharge to make sure we include patients discharged not on day of surgery
EOApostopfills<-EOAdrug2%>%
  mutate(SVCDATEDrug = as.Date(SVCDATEDrug, format = "%m/%d/%Y"),
         SVCDATE = as.Date(SVCDATE, format = "%m/%d/%Y"))%>%
  mutate(SVCDATEDischarge = SVCDATE + LOS)%>%
  mutate(daysafter = as.numeric(difftime(SVCDATEDrug, SVCDATEDischarge, units = "days")))%>%
  filter(daysafter > -1 & daysafter< 1)


#Filter drug type and days supply
EOApreopfills1<-EOApreopfills %>%
  # filter(drug == "cefalexin" | drug == "cefadroxil" | drug == "clindamycin")%>%
  filter(DAYSUPP == 7)

EOApostopfills1<-EOApostopfills %>%
  # filter(drug == "cefalexin" | drug == "cefadroxil" | drug == "clindamycin")%>%
  filter(DAYSUPP == 7)

#Bind preop and postop fills together
EOAdrugscombined<-bind_rows(EOApreopfills1,EOApostopfills1)

#Join EOA to main data 
EOAFinal<-left_join(EOAMainCohort, EOAdrugscombined, by=c("ENROLID","SVCDATE", "AGE", "YEAR"))#%>%
  # replace_na(list(drug = "no EOA")) #no "drug" columns

#Assign final EOA status if RXs are found for these drugs
# EOAFinal<-EOAFinal%>%
#   mutate(EOAPrescribed = if_else(drug == "cefalexin" | drug == "cefadroxil" | drug == "clindamycin", 1, 0))
EOAFinal<-EOAFinal%>%
  mutate(EOAPrescribed = if_else(is.na(NDCNUM), 0, 1)) %>%
  mutate(EOAPrescribed.factor = factor(EOAPrescribed))

# EOAFinal$EOAPrescribed.factor %>% summary()

# Strange:
# table(EOAFinal$YEAR.x, EOAFinal$YEAR.y, useNA = "always")

# Procedure year variable
EOAFinal <- EOAFinal %>% mutate(procedure_year = format.Date(SVCDATEProcedure, "%Y"))

# Group by procedure_year and extract stats -- for example, age
EOAFinal %>% group_by(YEAR) %>% select(procedure_year, AGE) %>% summarise(mean(AGE)) 

# Group by procedure_year
EOAFinal %>% 
  group_by(YEAR) %>% 
  # select(procedure_year, AGE) %>% 
  summarise(mean(EOAPrescribed)) %>% View()



# Group by procedure_year and extract stats -- for example, age
EOAFinal %>% 
  group_by(YEAR, EOAPrescribed.factor) %>% 
  # select(procedure_year, AGE) %>% 
  summarise(mean(AGE)) 



####
s <- d2 %>% slice_sample(n = 1000)
View(s)

# Procedure year variable
s <- s %>% mutate(procedure_year = format.Date(SVCDATEProcedure, "%Y"))

# Group by procedure_year and extract stats -- for example, age
s %>% group_by(procedure_year) %>% select(procedure_year, AGE) %>% summarise(mean(AGE))

# Group by procedure_year and extract stats -- for example, age
s %>% group_by(procedure_year) %>% select(procedure_year, AGE) %>% summarise(mean(AGE))

