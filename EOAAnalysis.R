library(dplyr)
library(haven)

#Load Procedure Files
ProceduresEOA<-read_sas("E:/Andrew/EOAkneeData/procedurescombinedeoa.sas7bdat", 
                        NULL)
#Load Enrollment Files 
CCenrollmentfile<-read_sas("E:/Andrew/EOAkneeData/ccenrollmentok.sas7bdat", 
           NULL)
MDenrollmentfile<-read_sas("E:/Andrew/EOAkneeData/mdenrollmentok.sas7bdat", 
                           NULL)
EnrollmentcombinedEOA<-rbind(CCenrollmentfile,MDenrollmentfile)

#Load Comorbidities
ComorbiditiesEOA<-read_sas("E:/Andrew/EOAkneeData/comorbiditiescombined3.sas7bdat", 
                        NULL)
#Delete dups 
ComorbiditiesEOA<-ComorbiditiesEOA%>%
  distinct(ENROLID, SVCDATE,.keep_all = TRUE)

#Load 90 day complications 
ComplicationsEOA<-read_sas("E:/Andrew/EOAkneeData/complications90combined.sas7bdat", 
                           NULL)

#Load Admissions 
CCAdmissionsEOA<-read_sas("E:/Andrew/EOAkneeData/ccadmissionsfile.sas7bdat", 
                           NULL)
MDadmissionsEOA<-read_sas("E:/Andrew/EOAkneeData/mdadmissionsfile.sas7bdat", 
                          NULL)

AdmissionsEOA<-rbind(CCAdmissionsEOA,MDadmissionsEOA)


AdmissionsEOA2<-AdmissionsEOA%>%
  dplyr::select(DAYS, ADMDATE, ENROLID,SVCDATEProcedure, DX1)

#Changing column names
AdmissionsEOA2$SVCDATE<-AdmissionsEOA2$SVCDATEProcedure
AdmissionsEOA2$LOS<-AdmissionsEOA2$DAYS 

#Keep only admissions that correspond to surgery date
AdmissionsEOA2<-AdmissionsEOA2%>%
  group_by(ENROLID,SVCDATE)%>%
  filter(ADMDATE == SVCDATE)%>%
  dplyr::select(ENROLID,SVCDATE,LOS, ADMDATE)%>%
  ungroup()

#Classify Re-Admissions
ReadmissionEOA<-AdmissionsEOA%>%
  mutate(Days_After=ADMDATE-SVCDATEProcedure)%>%
  filter(Days_After > 0 & Days_After <91)

#Rename all SVCATEProcedures as SVCDATE for joining
ReadmissionEOA$SVCDATE<-ReadmissionEOA$SVCDATEProcedure

#Rename top 2 dx codes for readmission 
ReadmissionEOA$DXReadmission1<-ReadmissionEOA$DX1
ReadmissionEOA$DXReadmission2<-ReadmissionEOA$DX2

#Keep only the earliest readmission date for each ENROLID and SVCDATE
ReadmissionEOA<-ReadmissionEOA%>%
  group_by(ENROLID,SVCDATE)%>%
  slice(which.min(Days_After))%>%
  ungroup()

ReadmissionEOA$ADMDATEreadmission<-ReadmissionEOA$ADMDATE

ReadmissionEOA2<-ReadmissionEOA%>%
  dplyr::select(SVCDATE, ENROLID, ADMDATEreadmission, Days_After,DXReadmission1,DXReadmission2)



#Join Comorbidities to Procedure file
#Remove duplicate column that forms from left_join
ProceduresComorbsEOA <- left_join(ProceduresEOA, ComorbiditiesEOA,
                                  by = c("ENROLID", "SVCDATE"), suffix = c("", ".y")) %>%
  select(-ends_with(".y"))

#Join 90 day complications
TKAEOA1<- left_join(ProceduresComorbsEOA, ComplicationsEOA,
                                  by = c("ENROLID", "SVCDATE"), suffix = c("", ".y")) 

#Remove duplicated combination of ENROLIDs and SVCDATE (same patient and same surgery date) 
TKAEOA2<-TKAEOA1%>%
  distinct(ENROLID, SVCDATE, .keep_all = TRUE)

#Join Length of Stay
TKAEOA3<-left_join(TKAEOA2,AdmissionsEOA2, by = c("ENROLID", "SVCDATE"), suffix = c("", ".y"))
  

#Join Readmissions
TKAEOA4<-left_join(TKAEOA3,ReadmissionEOA2, by = c("ENROLID", "SVCDATE"), suffix = c("", ".y"))


#Rename NA for patients without a readmission as 0 and readmission as 1 for those with readmission
TKAEOA5<-TKAEOA4%>%
  mutate(readmission=ifelse(is.na(ADMDATEreadmission),0, 1))

#Filter Enrollment 
EnrollmentEOA2<-EnrollmentcombinedEOA%>%
  filter(Enrollment6m_3m==1)

TKAEOA6<-TKAEOA5[(TKAEOA5$ENROLID) %in% EnrollmentEOA2$ENROLID,]

TKAEOA7<-TKAEOA6%>%
  distinct(ENROLID, .keep_all = TRUE)
  
TKAEOA7<-TKAEOA7%>%
  filter(AGE > 18 & AGE < 90)

# Need to Filter Patients with Prior UKA
UKAprocedures <- read_sas("E:/Andrew/procedurescombined_uka.sas7bdat", 
                          NULL)
UKAprocedures<-UKAprocedures%>%
  distinct(ENROLID, SVCDATE,.keep_all = TRUE)%>%
  mutate(UKA = 1)

UKAprocedures$SVCDATEUKA<-UKAprocedures$SVCDATE

UKAprocedures<-UKAprocedures%>%
  dplyr::select(ENROLID, SVCDATEUKA, UKA)

#Join UKAs to main data 
library(tidyr)
TKAEOA8<-left_join(TKAEOA7, UKAprocedures, by=c("ENROLID"))%>%
  replace_na(list(UKA = 0))

TKAEOA8<-TKAEOA8%>%
  mutate(UKA_days= SVCDATEUKA-SVCDATE)

TKAEOA8<-TKAEOA8%>%         
  mutate(priorUKA= case_when(UKA_days > 0 ~ 0,
                             TRUE ~ UKA))

#Filter out Prior UKA
TKAEOA8<-TKAEOA8%>%
  dplyr::filter(priorUKA == 0)


#Filter out prior infected joint or hardware
TKAEOA8<-TKAEOA8%>%
  filter(HxInfectedJointHW == 0)

#Filter out surgeries before 2014 and after 2020 
TKAEOA8<-TKAEOA8%>%
  filter(YEAR > 2013)

#Filter out active infection (abscess, cellulitis, and septic joint) at TOS 

TKAEOA8<-TKAEOA8%>%
  filter(!(stringr::str_detect((DX1), "L03") | stringr::str_detect((DX2), "L03")
           |    stringr::str_detect((DX3), "L03") |stringr::str_detect((DX4), "L03")
           |  stringr::str_detect((DX1), "M00") | stringr::str_detect((DX2), "M00")
           |    stringr::str_detect((DX3), "M00") |stringr::str_detect((DX4), "M00")
           |  stringr::str_detect((DX1), "M7106") | stringr::str_detect((DX2), "M7106")
           |    stringr::str_detect((DX3), "M7106") |stringr::str_detect((DX4), "M7106")
           |  stringr::str_detect((DX1), "L04") | stringr::str_detect((DX2), "L04")
           |    stringr::str_detect((DX3), "L04") |stringr::str_detect((DX4), "L04")))

#Filter out post traumatic osteoarthritis 

TKAEOA9<-TKAEOA8%>%
  filter(!(stringr::str_detect((DX1), "M173") | stringr::str_detect((DX2), "M173")
           |    stringr::str_detect((DX3), "M173") |stringr::str_detect((DX4), "M173")))

#Filter out TKA for tibial or femoral fracture 
TKAEOA9<-TKAEOA8%>%
  filter(!(stringr::str_detect((DX1), "S821") | stringr::str_detect((DX2), "S821")
           |    stringr::str_detect((DX3), "S821") |stringr::str_detect((DX4), "S821") 
           |    stringr::str_detect((DX1), "S724") | stringr::str_detect((DX2), "S724")
           |    stringr::str_detect((DX3), "S724") |stringr::str_detect((DX4), "S724")))

library(writexl)
write_xlsx(TKAEOA8,"E:/Andrew/EOAMainCohort.xlsx")


#DRUG ANALYSIS 
redbook <- read_sas("Z:/Redbook/redbook21.sas7bdat", NULL)

redbook<-redbook%>%
  dplyr::select(NDCNUM, GENNME, PKSIZE, ROACD,STRNGTH)


ndc_numbers <- bind_rows(
  redbook %>%
    filter(stringr::str_detect(tolower(GENNME), "cefadroxil")) %>%
    mutate(drug = "cefadroxil"),
  redbook %>%
    filter(stringr::str_detect(tolower(GENNME), "cephalexin")) %>%
    mutate(drug = "cefalexin"),
  redbook %>%
    filter(stringr::str_detect(tolower(GENNME), "clindamycin")) %>%
    mutate(drug = "clindamycin"),
  redbook %>%
    filter(stringr::str_detect(tolower(GENNME), "doxycycline")) %>%
    mutate(drug = "doxycycline"),
  redbook%>%
    filter(stringr::str_detect(tolower(GENNME), "sulfamethoxazole/trimethoprim")) %>%
    mutate(drug = "TMP_SMX")
)%>%
  dplyr::select(drug, NDCNUM,GENNME,PKSIZE, ROACD,STRNGTH)

ndc_numbers<-ndc_numbers%>%
  filter(ROACD == "PO")


#Load drugs files 
EOAdrugs<-read_sas("E:/Andrew/EOakneeData/drugfilecombined.sas7bdat", NULL)

#Use ndc_numbers to keep only drug fills of interest
EOadrug1=inner_join(EOAdrugs, ndc_numbers,  by="NDCNUM")


#Filter antibiotics given within 7 days before surgery 
EOApreopfills<-EOadrug1%>%
  mutate(SVCDATEDrug = as.Date(SVCDATEDrug, format = "%m/%d/%Y"),
    SVCDATEProcedure = as.Date(SVCDATEProcedure, format = "%m/%d/%Y"))%>%
     mutate(daysbefore = as.numeric(difftime(SVCDATEProcedure, SVCDATEDrug, units = "days")))%>%
      filter(daysbefore > 0 & daysbefore < 8)
    
#Include antibiotics filled on day of surgery 
#First link Length of stay to postop fills 
library(dplyr)
library(tidyr)
Lengthofstay<-TKAEOA9%>%
  select(ENROLID, SVCDATE, LOS)%>%
  replace_na(list (LOS = 0))

#Rename SVCDATEProcedure
EOadrug1$SVCDATE<-EOadrug1$SVCDATEProcedure

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
EOApreopfills1<-EOApreopfills%>%
  filter(drug == "cefalexin" | drug == "cefadroxil" | drug == "clindamycin")%>%
  filter(DAYSUPP == 7)

EOApostopfills1<-EOApostopfills%>%
  filter(drug == "cefalexin" | drug == "cefadroxil" | drug == "clindamycin")%>%
  filter(DAYSUPP == 7)

#Join preop and postop fills together, use bind_rows instead of
#bind_rows to join when columns are different (preop has daysbefore and postop has days after column) 

EOAdrugscombined<-bind_rows(EOApreopfills1,EOApostopfills1)


#Join EOA to main data 
EOAFinal<-left_join(TKAEOA9,EOAdrugscombined, by=c("ENROLID","SVCDATE"))%>%
  replace_na(list(drug="no EOA"))

#Assign final EOA status if RXs are found for these drugs
EOAFinal<-EOAFinal%>%
  mutate(EOAPrescribed = if_else(drug == "cefalexin" | drug == "cefadroxil" | drug == "clindamycin", 1, 0))
  

#Add Elixhauser Comorbidity Index 
EOAFinal<-EOAFinal%>%
  mutate(ElixhauserComorbidityIndex = rowSums(select(., Elix_CHF:Elix_Depression), na.rm = TRUE))

#Match Overall Cohort 

library(MatchIt)
MatchedEOAall<-matchit(formula = EOAPrescribed ~ AGE.x + SEX.x + Elix_CHF:Elix_Depression + 
                         BMI35 + CKD + AutoimmuneDz + ActiveSmoking + 
                         MRSA_MSSAColonization + HepC + ChronicCystitis + StasisDermatitis + 
                         HxSepsis + HxSAInfection + CM_CAD + CM_HLD + 
                         PriorIschemicStroke + PriorMI, 
                       data = EOAFinal, method = "nearest", distance = "glm", replace = FALSE, ratio = 1, caliper = 0.2)

summary(MatchedEOAall)
MatchedEOAall<-match.data(MatchedEOAall)


#Load in PJI cases
PJICCCKnee<-read_sas("E:/Andrew/PJIKneeData/cccpprocedurescombined_pji.sas7bdat", 
                     NULL)
PJIMDCKnee<-read_sas("E:/Andrew/PJIKneeData/mdcpprocedurescombined_pji.sas7bdat", 
                     NULL)
PJIv2knee<-rbind(PJICCCKnee,PJIMDCKnee)

library(dplyr)
PJIv2knee<-PJIv2knee%>%
  dplyr::select(ENROLID,DX1,PROC1, SVCDATE)%>%
  mutate(PJIcombined=1)
PJIv2knee$PROCPJI<-PJIv2knee$PROC1
PJIv2knee$SVCDATE_PJI<-PJIv2knee$SVCDATE

PJIv2knee<-PJIv2knee%>%
  distinct(ENROLID,SVCDATE_PJI,.keep_all = TRUE)

PJIv2knee<-PJIv2knee%>%
  dplyr::select(ENROLID,PJIcombined, PROCPJI, SVCDATE_PJI)

MatchedEOAall1<-left_join(MatchedEOAall,PJIv2knee, by="ENROLID")

#Keep only the earliest PJI date for those who have one
MatchedEOAall1 <- MatchedEOAall1 %>%
  group_by(ENROLID) %>%
  filter(SVCDATE_PJI == min(SVCDATE_PJI, na.rm = TRUE) | is.na(SVCDATE_PJI)) %>%
  ungroup()


library(tidyr)
MatchedEOAall1<-MatchedEOAall1%>%
  mutate(DaystoPJI = SVCDATE_PJI - SVCDATE)%>%
  mutate(PJI90days = case_when(DaystoPJI > 90 | DaystoPJI <0 ~ 0,
                               TRUE ~ PJIcombined))%>%
  mutate(PJI2year = case_when(DaystoPJI > 760 | DaystoPJI <0 ~ 0,
                              TRUE ~ PJIcombined))%>%
  replace_na(list(PJI90days = 0))%>%
  replace_na(list(PJI2year = 0))

library(gtsummary)
library(dplyr)
MatchedEOAall1%>%
  dplyr::select(ENROLID,SVCDATE,EOAPrescribed,PJI90days,PJI_90,PJI2year)%>%
  tbl_summary(
    by=EOAPrescribed,
    missing = "no",
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_dichotomous() ~ "{n} / {N} ({p}%)"), 
  )%>%
  add_p(list(all_continuous() ~ "t.test")
  )

#Filter High Risk patients only 
EOAallhighrisk<-EOAFinal%>%
  filter(BMI35 == 1 | DiabetesMellitus == 1 | CKD == 1 | AutoimmuneDz == 1 |ActiveSmoking == 1 | MRSA_MSSAColonization == 1 | HepC == 1 | 
           ChronicCystitis == 1 | StasisDermatitis == 1 | HxSepsis == 1)

MatchedEOAHR<-matchit(formula = EOAPrescribed ~ AGE.x + SEX.x + Elix_CHF:Elix_Depression + 
                         BMI35 + CKD + AutoimmuneDz + ActiveSmoking + 
                         MRSA_MSSAColonization + HepC + ChronicCystitis + StasisDermatitis + 
                         HxSepsis + HxSAInfection + CM_CAD + CM_HLD + 
                         PriorIschemicStroke + PriorMI, 
                       data = EOAallhighrisk, method = "nearest", distance = "glm", replace = FALSE, ratio = 1, caliper = 0.2)

summary(MatchedEOAHR)
MatchedEOAHR<-match.data(MatchedEOAHR)

#Join PJI cases 

MatchedEOAHR1<-left_join(MatchedEOAHR,PJIv2knee, by="ENROLID")
#Keep only the earliest PJI date for those who have one
MatchedEOAHR1 <- MatchedEOAHR1 %>%
  group_by(ENROLID) %>%
  filter(SVCDATE_PJI == min(SVCDATE_PJI, na.rm = TRUE) | is.na(SVCDATE_PJI)) %>%
  ungroup()


library(tidyr)
MatchedEOAHR1<-MatchedEOAHR1%>%
  mutate(DaystoPJI = SVCDATE_PJI - SVCDATE)%>%
  mutate(PJI90days = case_when(DaystoPJI > 90 | DaystoPJI <0 ~ 0,
                               TRUE ~ PJIcombined))%>%
  mutate(PJI2year = case_when(DaystoPJI > 760 | DaystoPJI <0 ~ 0,
                              TRUE ~ PJIcombined))%>%
  replace_na(list(PJI90days = 0))%>%
  replace_na(list(PJI2year = 0))


library(gtsummary)
library(dplyr)
MatchedEOAHR1%>%
  dplyr::select(ENROLID,SVCDATE,EOAPrescribed,PJI90days,PJI2year)%>%
  tbl_summary(
    by=EOAPrescribed,
    missing = "no",
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_dichotomous() ~ "{n} / {N} ({p}%)"), 
  )%>%
  add_p(list(all_continuous() ~ "t.test")
  )
