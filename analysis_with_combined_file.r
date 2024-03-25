# Analysis using the combined file Andrew sent on 3/15/2024

# rm(list = ls())

# Import Packages
library(tidyr)
library(dplyr)
library(purrr)
library(haven)
library(readxl)
library(ggplot2)

f1 <- "EOAMainCohort.xlsx"

# NOTE: Commenting out protected lines -- uncomment as needed.
EOAMainCohort <- read_excel("Latest Data/EOAMainCohort.xlsx")
# EOAMainCohort_untouched <- EOAMainCohort
d  <- EOAMainCohort

# drugfilecombined <- read_sas("Latest Data/drugfilecombined.sas7bdat", NULL)
# drugfilecombined_untouched <- drugfilecombined
# drugfilecombined <- drugfilecombined_untouched

# Seems that the main cohort excel file from 3/15/2024 had a join that didn't quite go right
# Clearing out the issues w the join

# EOAMainCohort %>% select(ends_with(".x"), ends_with(".y")) %>% colnames() %>% View()

d <- d %>% 
  rename_with( ~ unlist(strsplit(.x, ".x")), ends_with(".x")) %>% 
  select(-ends_with(".y")) # %>% colnames() %>% View()

#NOTE: Need to clarify with Andrew which drugs are included. 
# Drugs prescribed between 5 days before and 3 days after are included
# Course of 7-14 days
# d$daysbefore %>% hist()
# d$daysafter %>% hist()
# d$DAYSUPP %>% hist()
# d$EOAPrescribed %>% factor() %>% summary()

#Elix 
d <- d %>% 
  rowwise() %>%
  mutate(Elix = sum(c_across(starts_with("Elix_")), na.rm = FALSE)) 


# Make factor variables
d <- d %>% mutate(EOAPrescribed.factor = factor(EOAPrescribed, levels = c(0, 1), labels = c("No EOA", "EOA")))
d <- d %>% mutate(AGEGRP.factor = factor(AGEGRP))
d <- d %>% mutate(female = (SEX == "2")) %>% mutate(female = factor(female))
d <- d %>% mutate(UKA.factor = factor(UKA))
d <- d %>% mutate(priorUKA.factor = factor(priorUKA))
d <- d %>% mutate(DiabetesMellitus.factor = factor(DiabetesMellitus))
d <- d %>% mutate(BMI35.factor = factor(BMI35))
d <- d %>% mutate(Obesity.factor = factor(Elix_Obesity))
d <- d %>% mutate(DAYSUPP.factor = factor(DAYSUPP))
d <- d %>% mutate(UKA.factor = factor(UKA))
d <- d %>% mutate(priorUKA.factor = factor(priorUKA))
d <- d %>% mutate(AutoimmuneDz.factor = factor(AutoimmuneDz))
d <- d %>% mutate(CKD.factor = factor(CKD))
d <- d %>% mutate(RenalFailure.factor = factor(Elix_RenalFailure))
d <- d %>% mutate(ActiveSmoking.factor = factor(ActiveSmoking))
d <- d %>% mutate(DiabetesMellitus.factor = factor(DiabetesMellitus))
d <- d %>% mutate(DiabetesUncomp.factor = factor(Elix_DiabetesUncomp))
d <- d %>% mutate(DiabetesComp.factor = factor(Elix_DiabetesComp))
d <- d %>% mutate(StasisDermatitis.factor = factor(StasisDermatitis))
d <- d %>% mutate(ChronicCystitis.factor = factor(ChronicCystitis))
d <- d %>% mutate(Obesity.factor = factor(Elix_Obesity))
d <- d %>% mutate(HxSepsis.factor = factor(HxSepsis))
d <- d %>% mutate(MRSA_MSSAColonization.factor = factor(MRSA_MSSAColonization))
d <- d %>% mutate(HepC.factor = factor(HepC))
d <- d %>% mutate(HTN = case_when(Elix_CompHTN > 0 | Elix_UncompHTN > 0 ~ 1, .default = 0))
d <- d %>% mutate(HTN.factor = factor(HTN))
d <- d %>% mutate(Elix_Hypothyroid.factor = factor(Elix_Hypothyroid))
d <- d %>% mutate(Elix_Arrythmia.factor = factor(Elix_Arrythmia))
d <- d %>% mutate(Elix_ChronicPulm.factor = factor(Elix_ChronicPulm))
d <- d %>% mutate(Elix_Depression.factor = factor(Elix_Depression))
d <- d %>% mutate(Elix_FluidElectrolyte.factor = factor(Elix_FluidElectrolyte))
d <- d %>% mutate(Elix_Valvular.factor = factor(Elix_Valvular))
d <- d %>% mutate(Elix_RA.factor = factor(Elix_RA))
d <- d %>% mutate(Elix_PVD.factor = factor(Elix_PVD))
d <- d %>% mutate(Elix_SolidTumor.factor = factor(Elix_SolidTumor))
d <- d %>% mutate(Elix_DeficiencyAnemia.factor = factor(Elix_DeficiencyAnemia))

# High-risk cohort
d <- d %>% 
  rowwise() %>%
  mutate(risk_sum = sum(AutoimmuneDz, 
                        CKD,  
                        Elix_RenalFailure, 
                        ActiveSmoking,  
                        Elix_DiabetesUncomp, 
                        Elix_DiabetesComp,  
                        StasisDermatitis,  
                        ChronicCystitis, 
                        BMI35,
                        HxSepsis, 
                        MRSA_MSSAColonization,
                        HepC, na.rm = FALSE)) %>%
  mutate(high_risk = case_when(risk_sum >= 1 ~ 1, risk_sum == 0 ~ 0, .default = NA)) %>% 
  mutate(high_risk.factor = factor(high_risk, levels = c(0, 1), labels = c("Standard Risk", "High Risk"))) %>% 
  mutate(very_high_risk = case_when(risk_sum >= 2 ~ 2, risk_sum == 1 ~ 1, risk_sum == 0 ~ 0, .default = NA)) %>% 
  mutate(very_high_risk.factor = factor(very_high_risk, levels = c(0, 1, 2), labels = c("Standard Risk", "High Risk", "Very High Risk")))
# 
# d <- d %>% 
# mutate(very_high_risk = case_when(risk_sum >= 2 ~ 2, risk_sum == 1 ~ 1, risk_sum == 0 ~ 0, .default = NA)) %>% 
#   mutate(very_high_risk.factor = factor(very_high_risk, levels = c(0, 1, 2), labels = c("Standard Risk", "High Risk", "Very High Risk")))

#Check: Can we get this? 
#   Intra-articular cortisone injection < 3 months prior to surgery 
#   IBD/Ulcerative colitis/Crohn's disease 
#   MS (may be included under autoimmune)
#   Vasculitis (may be included under autoimmune)

#Check: What's included in this var? 
#   AutoimmuneDz -- Which Autoimmune diseases
#   CKD -- does it double count w Elix_RenalFailure?

#Check: Should we use this as well? 
#   MRSA_MSSAColonization


# TABLE: EOA vs high-risk
table(d$high_risk.factor, d$EOAPrescribed.factor)

# Group by procedure_year
EOA_stats_by_year <-
  d %>% 
  group_by(YEAR) %>% 
  summarise(n_cases = n(), rate = mean(EOAPrescribed), sd = sd(EOAPrescribed)) 

# TABLE: Rate of EOA by year, overall cohort ("all patients")
EOA_stats_by_year <- EOA_stats_by_year %>% 
  mutate(Rate = round(100*rate, digits = 2)) %>% 
  mutate(Year = YEAR) %>% 
  mutate(Cases = n_cases)

EOA_stats_by_year %>% select(Year, Cases, Rate) %>% View()


# Group by procedure_year AND risk cohort
EOA_stats_by_year_cohort <-
  d %>% 
  group_by(YEAR, high_risk.factor) %>% 
  summarise(n_cases = n(), rate = mean(EOAPrescribed), sd = sd(EOAPrescribed)) %>% 
  mutate(Rate = round(100*rate, digits = 2)) %>% 
  mutate(Year = YEAR) %>% 
  mutate(Cases = n_cases) %>%
  mutate(Risk = high_risk.factor)

# View(EOA_stats_by_year_cohort)

# TABLE: EOA rates by year and cohort
EOA_stats_by_year_cohort %>% ungroup() %>% 
  select(Year, Cases, Rate, Risk) %>% group_by(Year) %>%
  pivot_wider(names_from = Risk, values_from = c(Rate, Cases)) %>% 
  View()


# Group by procedure_year AND very high risk cohort
EOA_stats_by_year_cohort_veryhigh <-
  d %>% 
  group_by(YEAR, very_high_risk.factor) %>% 
  summarise(n_cases = n(), rate = mean(EOAPrescribed), sd = sd(EOAPrescribed)) %>% 
  mutate(Rate = round(100*rate, digits = 2)) %>% 
  mutate(Year = YEAR) %>% 
  mutate(Cases = n_cases) %>%
  mutate(Risk = very_high_risk.factor)

# View(EOA_stats_by_year_cohort_veryhigh)


# PLOT: Rate of EOA vs year for overall cohort
# ggplot(EOA_stats_by_year, aes(y = rate, x = YEAR)) + geom_point() + geom_smooth()
ggplot(EOA_stats_by_year, aes(y = Rate, x = Year)) + 
  geom_bar(stat = "identity") + 
  xlab("Year") + ylab("Rate (%)") + #ggtitle("Rate of EOA in overall cohort") +
  scale_x_continuous(breaks = seq(min(EOA_stats_by_year$Year), max(EOA_stats_by_year$Year), by = 1)) + geom_vline(xintercept = 2018, linetype = "dashed")

# PLOT: Rate of EOA vs year by risk cohort
ggplot(EOA_stats_by_year_cohort, aes(y = Rate, x = Year, color = Risk, fill = Risk)) + 
  geom_bar(stat = "identity", position = position_dodge()) +
  xlab("Year") + ylab("Rate (%)") + #ggtitle("Rate of EOA by risk cohort") +
  scale_x_continuous(breaks = seq(min(EOA_stats_by_year$Year), max(EOA_stats_by_year$Year), by = 1)) + geom_vline(xintercept = 2018, linetype = "dashed")


# PLOT: Rate of EOA vs year by risk cohort (w very high risk)
ggplot(EOA_stats_by_year_cohort_veryhigh, aes(y = Rate, x = Year, color = Risk, fill = Risk)) + 
  geom_bar(stat = "identity", position = position_dodge()) +
  xlab("Year") + ylab("Rate (%)") + #ggtitle("Rate of EOA in overall cohort") +
  scale_x_continuous(breaks = seq(min(EOA_stats_by_year$Year), max(EOA_stats_by_year$Year), by = 1)) + geom_vline(xintercept = 2018, linetype = "dashed")


# PLOT: Rate of EOA vs year by risk cohort
# ggplot(EOA_stats_by_year_cohort, aes(y = Rate, x = Year, color = Risk, fill = Risk)) + geom_point() #+ geom_smooth()

  
# Group values of comorbidities by procedure_year
comorbidities_by_year <-
  d %>% 
  select(YEAR, AGE, 
         SEX, 
         BMI35, 
         DiabetesMellitus,  
         ActiveSmoking,   
         HxSepsis,  
         MRSA_MSSAColonization, 
         CKD,  
         AutoimmuneDz,
         EOAPrescribed  ) %>%  #,
         # HTN, 
         # Elix_Hypothyroid, 
         # Elix_Arrythmia, 
         # Elix_ChronicPulm, 
         # Elix_Depression, 
         # Elix_FluidElectrolyte, 
         # Elix_Valvular, 
         # Elix_RA, 
         # Elix_PVD, 
         # Elix_SolidTumor, 
         # Elix_DeficiencyAnemia ) %>%
  group_by(YEAR) %>% 
  summarise(n_cases = n(), 
            rate = mean(EOAPrescribed),
            mean_AGE = mean(AGE),
            mean_BMI35 = mean(BMI35), 
            mean_DiabetesMellitus = mean(DiabetesMellitus),  
            mean_ActiveSmoking = mean(ActiveSmoking),
            mean_HxSepsis = mean(HxSepsis),
            mean_MRSA_MSSAColonization = mean(MRSA_MSSAColonization),
            mean_CKD = mean(CKD),
            mean_AutoimmuneDz = mean(AutoimmuneDz) ) %>% #,
            # mean_HTN = mean(HTN) #,
            # mean_Elix_Hypothyroid = mean(Elix_Hypothyroid),
            # mean_Elix_Arrythmia = mean(Elix_Arrythmia),
            # mean_Elix_ChronicPulm = mean(Elix_ChronicPulm),
            # mean_Elix_Depression = mean(Elix_Depression),
            # mean_Elix_FluidElectrolyte = mean(Elix_FluidElectrolyte),
            # mean_Elix_Valvular = mean(Elix_Valvular),
            # mean_Elix_RA = mean(Elix_RA),
            # mean_Elix_PVD = mean(Elix_PVD),
            # mean_Elix_SolidTumor = mean(Elix_SolidTumor),
            # mean_Elix_DeficiencyAnemia = mean(Elix_DeficiencyAnemia) ) %>% 
  mutate(Rate = round(100*rate, digits = 2)) %>% 
  mutate(mean_EOA = rate) %>% 
  mutate(mean_EOA_nolog = 10*rate) %>% #Might want to use Rate instead -- scale by 100
  mutate(Year = YEAR) %>% 
  mutate(Cases = n_cases)

# PLOT: Rates of EOA and of comorbidities by year

comorbidities_by_year_pivoted <- comorbidities_by_year %>% 
  select(-Rate, -rate, -YEAR, -n_cases, -Cases, -mean_AGE, -mean_EOA_nolog) %>%
  pivot_longer(cols = starts_with("mean_"), names_to = "Comorbidity", values_to = "Rate")
ggplot(comorbidities_by_year_pivoted, aes(y = Rate, x = Year, color = Comorbidity, group = Comorbidity)) + 
  geom_point(show.legend = TRUE) + 
  geom_smooth(se = FALSE,show.legend = FALSE) + 
  geom_vline(xintercept = 2018, linetype = "dashed") + 
  ylab("Log Rate") + 
  theme_bw() + 
  scale_color_manual(values = c(rep("grey", 5), "blue", rep("grey", 2))) + 
  # scale_color_manual(values = c(rep("darkgrey", 15), "blue", rep("darkgrey", 3))) + 
  scale_y_continuous(trans = "log10")#, sec.axis = sec_axis(~./10, "test"))

# With color
ggplot(comorbidities_by_year_pivoted, aes(y = Rate, x = Year, color = Comorbidity, group = Comorbidity)) + 
  geom_point() + 
  geom_smooth(se = TRUE) + 
  geom_vline(xintercept = 2018, linetype = "dashed") + 
  ylab("Log Rate") + 
  theme_bw() + 
  # scale_color_manual(values = c(rep("grey", 15), "blue", rep("grey", 3))) + 
  scale_y_continuous(trans = "log10")#, sec.axis = sec_axis(~./10, "test"))




# PLOT: Rates of EOA and of comorbidities by year -- VERSION WITHOUT LOG
comorbidities_by_year_pivoted_nolog <- comorbidities_by_year %>% 
  select(-Rate, -rate, -YEAR, -n_cases, -Cases, -mean_AGE, -mean_EOA) %>%
  pivot_longer(cols = starts_with("mean_"), names_to = "Comorbidity", values_to = "Rate")
ggplot(comorbidities_by_year_pivoted_nolog, aes(y = Rate, x = Year, color = Comorbidity, group = Comorbidity)) + 
  geom_point() + 
  geom_smooth() + geom_vline(xintercept = 2018, linetype = "dashed") + 
  ylab("Rate of Comorbidities") + 
  scale_y_continuous(sec.axis = sec_axis(~./10, "Rate of EOA"))

# PLOT: Rates of EOA and of comorbidities by year -- VERSION WITHOUT LOG
comorbidities_by_year_pivoted_nolog_noscale <- comorbidities_by_year %>% 
  select(-Rate, -rate, -YEAR, -n_cases, -Cases, -mean_AGE, -mean_EOA) %>%
  mutate(mean_EOA_nolog = mean_EOA_nolog/10) %>%
  pivot_longer(cols = starts_with("mean_"), names_to = "Comorbidity", values_to = "Rate")
ggplot(comorbidities_by_year_pivoted_nolog_noscale, aes(y = Rate, x = Year, color = Comorbidity, group = Comorbidity)) + 
  geom_point() + 
  geom_smooth() + geom_vline(xintercept = 2018, linetype = "dashed") + 
  ylab("Rate") #+ 
  # scale_y_continuous(sec.axis = sec_axis(~./10, "Rate of EOA"))

View(EOA_stats_by_year)


#
# Mann Kendall Test
#
library(trend)
mk <- mk.test(EOA_stats_by_year$rate)
#

####
# Prior UKA patients have now been removed from the cohort 
###
# 
# # Group by procedure_year AND prior UKA
# EOA_stats_by_year_priorUKA <-
#   d %>% 
#   group_by(YEAR, priorUKA.factor) %>% 
#   summarise(n_cases = n(), rate = mean(EOAPrescribed), sd = sd(EOAPrescribed)) %>% 
#   mutate(Rate = round(100*rate, digits = 2)) %>% 
#   mutate(Year = YEAR) %>% 
#   mutate(Cases = n_cases) %>%
#   mutate(Prior_UKA = priorUKA.factor)
# 
# # TABLE: EOA rates by year and prior UKA
# EOA_stats_by_year_priorUKA %>% ungroup() %>% 
#   select(Year, Cases, Rate, Prior_UKA) %>% group_by(Year) %>%
#   pivot_wider(names_from = Prior_UKA, values_from = c(Rate, Cases)) %>% 
#   View()
# 
# # PLOT: Rate of EOA vs year by prior UKA
# ggplot(EOA_stats_by_year_priorUKA, aes(y = Rate, x = Year, color = Prior_UKA, fill = Prior_UKA)) + 
#   geom_bar(stat = "identity", position = position_dodge()) +
#   xlab("Year") + ylab("Rate (%)") + #ggtitle("Rate of EOA by UKA vs TKA") +
#   scale_x_continuous(breaks = seq(min(EOA_stats_by_year_priorUKA$Year), max(EOA_stats_by_year_priorUKA$Year), by = 1)) + geom_vline(xintercept = 2018, linetype = "dashed")
# 

# 
# d %>% filter(risk_sum >= 1) %>% group_by(AutoimmuneDz, 
#                                        CKD,  
#                                        Elix_RenalFailure, 
#                                        ActiveSmoking,  
#                                        Elix_DiabetesUncomp, 
#                                        Elix_DiabetesComp,  
#                                        StasisDermatitis,  
#                                        ChronicCystitis, 
#                                        BMI35,
#                                        HxSepsis, 
#                                        MRSA_MSSAColonization,
#                                        HepC) %>% summarise(n = count())

# Table: Risk factors by risk cohort
# table_risk_factors <- d %>% 
#   group_by(high_risk.factor) %>% summarise(AutoimmuneDz_n = sum(AutoimmuneDz), 
#                                          CKD_n = sum(CKD),
#                                          Elix_RenalFailure_n = sum(Elix_RenalFailure), 
#                                          ActiveSmoking_n = sum(ActiveSmoking),  
#                                          Elix_DiabetesUncomp_n = sum(Elix_DiabetesUncomp), 
#                                          Elix_DiabetesComp_n = sum(Elix_DiabetesComp),  
#                                          StasisDermatitis_n = sum(StasisDermatitis),
#                                          ChronicCystitis_n = sum(ChronicCystitis), 
#                                          BMI35_n = sum(BMI35),
#                                          HxSepsis_n = sum(HxSepsis),
#                                          MRSA_MSSAColonization_n = sum(MRSA_MSSAColonization),
#                                          HepC_n = sum(HepC)) %>%
#   pivot_longer(cols = !high_risk.factor) %>% pivot_wider(names_from = high_risk.factor)

# Table: Risk factors by risk cohort
table_risk_factors_veryhigh <- d %>% 
  group_by(very_high_risk.factor) %>% summarise(AutoimmuneDz_n = sum(AutoimmuneDz), 
                                           CKD_n = sum(CKD),
                                           Elix_RenalFailure_n = sum(Elix_RenalFailure), 
                                           ActiveSmoking_n = sum(ActiveSmoking),  
                                           Elix_DiabetesUncomp_n = sum(Elix_DiabetesUncomp), 
                                           Elix_DiabetesComp_n = sum(Elix_DiabetesComp),  
                                           StasisDermatitis_n = sum(StasisDermatitis),
                                           ChronicCystitis_n = sum(ChronicCystitis), 
                                           BMI35_n = sum(BMI35),
                                           HxSepsis_n = sum(HxSepsis),
                                           MRSA_MSSAColonization_n = sum(MRSA_MSSAColonization),
                                           HepC_n = sum(HepC)) %>%
  pivot_longer(cols = !very_high_risk.factor) %>% pivot_wider(names_from = very_high_risk.factor)


# Table: Risk factors by risk sum
table_risk_sum <- d %>% 
  group_by(risk_sum) %>% summarise(AutoimmuneDz_n = sum(AutoimmuneDz), 
                                                CKD_n = sum(CKD),
                                                Elix_RenalFailure_n = sum(Elix_RenalFailure), 
                                                ActiveSmoking_n = sum(ActiveSmoking),  
                                                Elix_DiabetesUncomp_n = sum(Elix_DiabetesUncomp), 
                                                Elix_DiabetesComp_n = sum(Elix_DiabetesComp),  
                                                StasisDermatitis_n = sum(StasisDermatitis),
                                                ChronicCystitis_n = sum(ChronicCystitis), 
                                                BMI35_n = sum(BMI35),
                                                HxSepsis_n = sum(HxSepsis),
                                                MRSA_MSSAColonization_n = sum(MRSA_MSSAColonization),
                                                HepC_n = sum(HepC)) %>%
  pivot_longer(cols = !risk_sum) %>% pivot_wider(names_from = risk_sum)

# Totals
# table_risk_sum %>% summarise(name = "Total", across(!name, sum))

library(table1)

# Table-making code
# Parts of the code below is from this website
# https://cran.r-project.org/web/packages/table1/vignettes/table1-examples.html
cont_vars <- function(x) { 
  # with(stats.apply.rounding(stats.default(x), digits=2), 
  #      c("", "Mean (SD)"=sprintf("%s (&plusmn;%s)", MEAN, SD)))
  with(stats.apply.rounding(stats.default(x), digits=3),
       c("", "Median (IQR)"=sprintf("%s (%s)", MEDIAN, IQR)))
       # c("", "Mean (SD)"=sprintf("%s (%s)", MEAN, SD)))
}

cat_vars <- function(x) {
  c("", sapply(stats.default(x),
               function(y) with(y, sprintf("%d (%0.1f%%)", FREQ, PCT))))
}

pvalue <- function(x, ...) {
  # Construct vectors of data y, and groups (strata) g
  x <- x[names(x) != "overall"]
  y <- unlist(x)
  g <- factor(rep(1:length(x), times=sapply(x, length)))
  if (is.numeric(y)) {
    # For numeric variables, perform a standard 2-sample t-test
    p <- t.test(y ~ g)$p.value
  } else {
    # For categorical variables, perform a chi-squared test of independence
    p <- chisq.test(table(y, g), simulate.p.value = TRUE)$p.value
  }
  # Format the p-value, using an HTML entity for the less-than sign.
  # The initial empty string places the output on the line below the variable label.
  c("", sub("<", "&lt;", format.pval(p, digits=3, eps=0.001))) }


label(d$AGE) <- "Age"
label(d$AGEGRP.factor) <- "Age Group"
label(d$SEX) <- "Sex"
label(d$female) <- "Female"
label(d$BMI35) <- "BMI >= 35"
label(d$Elix_Obesity) <- "Obese"
label(d$DAYSUPP) <- "Length of antibiotic course"
label(d$UKA) <- "UKA"
label(d$priorUKA) <- "History of UKA"
label(d$AutoimmuneDz) = "Autoimmune Disease"
label(d$CKD)= "CKD"
label(d$Elix_RenalFailure)= "Renal Failure"
label(d$ActiveSmoking)= "Current Smoker"
label(d$DiabetesMellitus)= "Diabetes Mellitus"
label(d$Elix_DiabetesUncomp)= "Diabetes (uncomplicated)"
label(d$Elix_DiabetesComp)= "Diabetes (with complication)"
label(d$StasisDermatitis) = "Stasis Dermatitis"
label(d$ChronicCystitis)= "Chronic Cystitis"
label(d$Elix_Obesity) = "Obesity"
label(d$HxSepsis) = "History of Sepsis"
label(d$MRSA_MSSAColonization)= "MRSA or MSSA Nasal Colonization"
label(d$HepC) = "Hepatitis C"
label(d$HTN) <- "HTN"
label(d$Elix_Hypothyroid) <- "Hypothyroid"
label(d$Elix_Arrythmia) <- "Arrythmia"
label(d$Elix_ChronicPulm) <- "Chronic Pulmonary Disease"
label(d$Elix_Depression) <- "Depression"
label(d$Elix_FluidElectrolyte) <- "Electrolyte Abnormalities"
label(d$Elix_Valvular) <- "Valvular Heart Disease"
label(d$Elix_RA) = "Rheumatoid Arthritis"
label(d$Elix_PVD) = "Peripheral Vascular Disease"
label(d$Elix_SolidTumor) = "Solid Tumor"
label(d$Elix_DeficiencyAnemia) = "Iron Deficiency Anemia"


label(d$BMI35.factor) <- "BMI >= 35"
label(d$Obesity.factor) <- "Obese"
label(d$DAYSUPP.factor) <- "Length of antibiotic course"
label(d$UKA.factor) <- "UKA"
label(d$priorUKA.factor) <- "History of UKA"
label(d$AutoimmuneDz.factor) = "Autoimmune Disease"
label(d$CKD.factor)= "CKD"
label(d$RenalFailure.factor)= "Renal Failure"
label(d$ActiveSmoking.factor)= "Current Smoker"
label(d$DiabetesMellitus.factor)= "Diabetes Mellitus"
label(d$DiabetesUncomp.factor)= "Diabetes (uncomplicated)"
label(d$DiabetesComp.factor)= "Diabetes (with complication)"
label(d$StasisDermatitis.factor) = "Stasis Dermatitis"
label(d$ChronicCystitis.factor)= "Chronic Cystitis"
label(d$Obesity.factor) = "Obesity"
label(d$HxSepsis.factor) = "History of Sepsis"
label(d$MRSA_MSSAColonization.factor)= "MRSA or MSSA Nasal Colonization"

label(d$HepC.factor) = "Hepatitis C"
label(d$HTN.factor) <- "HTN"
label(d$Elix_Hypothyroid.factor) <- "Hypothyroid"
label(d$Elix_Arrythmia.factor) <- "Arrythmia"
label(d$Elix_ChronicPulm.factor) <- "Chronic Pulmonary Disease"
label(d$Elix_Depression.factor) <- "Depression"
label(d$Elix_FluidElectrolyte.factor) <- "Electrolyte Abnormalities"
label(d$Elix_Valvular.factor) <- "Valvular Heart Disease"
label(d$Elix_RA.factor) = "Rheumatoid Arthritis"
label(d$Elix_PVD.factor) = "Peripheral Vascular Disease"
label(d$Elix_SolidTumor.factor) = "Solid Tumor"
label(d$Elix_DeficiencyAnemia.factor) = "Iron Deficiency Anemia"


label(d$Elix) = "Elixhauser Comorbidity Index"

# Table: Demographics of overall cohort, brief
# table1(~ AGE + 
#          female +
#          UKA.factor +
#          priorUKA.factor + 
#          Elix, 
#        caption = "Table: Descriptive statistics",
#        data = d,
#        render.continuous=cont_vars,
#        render.categorical=cat_vars,
#        overall = T)

# Table: Demographics of overall cohort, extensive
table1(~ AGE + 
         female +
         # priorUKA.factor + 
         Elix + 
         BMI35.factor +
         DiabetesMellitus.factor + 
         ActiveSmoking.factor +  
         HxSepsis.factor + 
         MRSA_MSSAColonization.factor +
         CKD.factor +  
         RenalFailure.factor + 
         AutoimmuneDz.factor + 
         StasisDermatitis.factor +  
         ChronicCystitis.factor + 
         HepC.factor, 
       caption = "Table: Descriptive statistics",
       data = d,
       render.continuous=cont_vars,
       render.categorical=cat_vars,
       overall = T)

# elix_table <- d %>% select(starts_with("Elix")) %>% colSums()

# Table: Demographics of overall cohort, w and w/out abx 
table1(~AGE + 
         female +
         # priorUKA.factor + 
         Elix + 
         BMI35.factor +
         DiabetesMellitus.factor + 
         ActiveSmoking.factor +  
         HxSepsis.factor + 
         MRSA_MSSAColonization.factor +
         CKD.factor +  
         RenalFailure.factor + 
         AutoimmuneDz.factor + 
         StasisDermatitis.factor +  
         ChronicCystitis.factor + 
         HepC.factor| EOAPrescribed.factor, 
       caption = "Table: Descriptive statistics",
       data = d,
       render.continuous=cont_vars,
       render.categorical=cat_vars,
       # overall = F, 
       extra.col=list(`P-value`=pvalue))

# TABLE: Additional variables to explore 
table1(~ HTN.factor +
         Elix_Hypothyroid.factor +
         Elix_Arrythmia.factor +
         Elix_ChronicPulm.factor +
         Elix_Depression.factor +
         Elix_FluidElectrolyte.factor +
         Elix_Valvular.factor +
         Elix_RA.factor +
         Elix_PVD.factor +
         Elix_SolidTumor.factor +
         Elix_DeficiencyAnemia.factor | EOAPrescribed.factor, 
       caption = "Table: Descriptive statistics",
       data = d,
       render.continuous=cont_vars,
       render.categorical=cat_vars,
       # overall = F, 
       extra.col=list(`P-value`=pvalue))


# Check for correlation between the "numeric" patient variables  
# install.packages("ggcorrplot")
library(ggcorrplot)
reduced_data <- d %>% select(AutoimmuneDz,
                               CKD,
                               Elix_RenalFailure,
                               ActiveSmoking,
                               DiabetesMellitus,
                               #Elix_DiabetesUncomp,
                               #Elix_DiabetesComp,
                               StasisDermatitis,
                               ChronicCystitis,
                               BMI35,
                               HxSepsis,
                               MRSA_MSSAColonization,
                               HepC, 
                               HTN,
                               Elix_Hypothyroid,
                               Elix_Arrythmia,
                               Elix_ChronicPulm,
                               Elix_Depression,
                               Elix_FluidElectrolyte,
                               Elix_Valvular) #Removed prior UKA
                               
reduced_data.factor <- d %>% select(female,
                                    AutoimmuneDz.factor,
                                   CKD.factor,
                                   RenalFailure.factor,
                                   ActiveSmoking.factor,
                                   DiabetesMellitus.factor,
                                   # DiabetesUncomp.factor,
                                   # DiabetesComp.factor,
                                   StasisDermatitis.factor,
                                   ChronicCystitis.factor,
                                   BMI35.factor,
                                   HxSepsis.factor,
                                   MRSA_MSSAColonization.factor,
                                   HepC.factor) #Removed prior UKA
corr_matrix = cor(reduced_data)
ggcorrplot(corr_matrix)

# https://stackoverflow.com/questions/52554336/plot-the-equivalent-of-correlation-matrix-for-factors-categorical-data-and-mi
model.matrix(~0+., data=reduced_data.factor) %>% 
  cor(use="pairwise.complete.obs") %>% 
  ggcorrplot(show.diag=FALSE, type="lower", lab=TRUE, lab_size=2)
model.matrix(~0+., data=reduced_data) %>% 
  cor(use="pairwise.complete.obs") %>% 
  ggcorrplot(show.diag=FALSE, type="lower", lab=TRUE, lab_size=2)

# Logistic regression
# Start w all correlated vars
# Then remove systematically

# Commented out this version.
# This model was made w factor vars.
# Apparently sometimes the implementation of the HL test 
# in the package ResrouceSelection doesn't work well w factor vars
#
# model_full <- 
# glm(EOAPrescribed ~ AGE +
#       female +
#       # Elix + 
#       BMI35.factor +
#       DiabetesMellitus.factor + 
#       ActiveSmoking.factor +  
#       HxSepsis.factor + 
#       MRSA_MSSAColonization.factor +
#       CKD.factor +  
#       # RenalFailure.factor + 
#       AutoimmuneDz.factor, data = d, family=binomial(link="logit"))
# summary(model_full)

model_full_not_factors <- 
  glm(EOAPrescribed ~ AGE +
        female +
        BMI35 +
        DiabetesMellitus + 
        ActiveSmoking +  
        HxSepsis + 
        MRSA_MSSAColonization +
        CKD + 
        AutoimmuneDz +
        Elix_Depression +
        Elix_FluidElectrolyte +
        Elix_Valvular +
        Elix_RA +
        Elix_PVD +
        Elix_SolidTumor, data = d, family=binomial(link="logit"))
summary(model_full_not_factors)

# 
# Hosmer and Lemeshow Goodness-of-fit Test
#
# Large p-val suggests good fit. 
#
# Resources:
# https://galton.uchicago.edu/~burbank/stat224/lectures/12chapter_part2_OLD_logisticRegression.pdf
# https://rpubs.com/mbounthavong/logistic_regression
library(ResourceSelection)
hoslem_test_result <- hoslem.test(d$EOAPrescribed, fitted(model_full_not_factors))
# hoslem.test(d$EOAPrescribed, fitted(model_full_not_factors), g = 10)

# Compute a McFadden Pseudo-R^2 statistic
# https://stats.oarc.ucla.edu/other/mult-pkg/faq/general/faq-what-are-pseudo-r-squareds/
# https://stats.stackexchange.com/questions/8511/how-to-calculate-pseudo-r2-from-rs-logistic-regression#:~:text=The%20pseudo%2DR2%2C%20in,model%20with%20constant%20and%20predictors.
model_null <- glm(EOAPrescribed ~ 1, data = d, family=binomial(link="logit"))
# summary(model_null)
mcfadden_pseudo_rsq <- 1 - logLik(model_full_not_factors)/logLik(model_null)

#
# Trying to include year of surgery in the logistic model
# Not sure how to do this correctly -- expect an exponential relationship w time
#
# model_time <- 
#   glm(EOAPrescribed ~ AGE +
#         female +
#         BMI35 +
#         DiabetesMellitus + 
#         HxSepsis + 
#         MRSA_MSSAColonization +
#         CKD +  
#         AutoimmuneDz + YEAR, data = d, family=binomial(link="logit"))
# summary(model_time)
# # logLik(model_time)/logLik(model_full_not_factors)
# 1 - logLik(model_time)/logLik(model_null)
# hoslem.test(d$EOAPrescribed, fitted(model_time))
# 
# model_time_reduced <- 
#   glm(EOAPrescribed ~ AGE +
#         female +
#         BMI35 +
#         DiabetesMellitus + 
#         AutoimmuneDz + YEAR, data = d, family=binomial(link="logit"))
# summary(model_time_reduced)
# logLik(model_time_reduced)/logLik(model_time)
# 1 - logLik(model_time_reduced)/logLik(model_null)



# http://www.sthda.com/english/articles/36-classification-methods-essentials/148-logistic-regression-assumptions-and-diagnostics-in-r/
# probabilities <- predict(model_full_not_factors, type = "response")
# predictors <-  c("AGE", 
#                  "female", 
#                  "BMI35", 
#                  "DiabetesMellitus",  
#                  "ActiveSmoking",   
#                  "HxSepsis",  
#                  "MRSA_MSSAColonization", 
#                  "CKD",  
#                  "AutoimmuneDz")

# library(broom)
# 
# d2 <- d %>% select(all_of(predictors))
# d2 <- d2 %>% bind_cols(probabilities) %>% rename(probabilities = ...10)
# d2 <- d2 %>% mutate(logit = log(probabilities/(1-probabilities))) %>%
#   gather(key = "predictors", value = "predictor.value", -logit)


# 
# ggplot(d2, aes(logit, predictor.value))+
#   geom_point(size = 0.5, alpha = 0.5) +
#   geom_smooth(method = "loess") + 
#   theme_bw() + 
#   facet_wrap(~predictors, scales = "free_y")



#######
# Separate graphs for vars added later (not in def of high risk)
# 
# # Group values of comorbidities by procedure_year
# comorbidities_by_year_2 <-
#   d %>% 
#   select(YEAR, EOAPrescribed,
#          HTN,
#          Elix_Hypothyroid,
#          Elix_Arrythmia,
#          Elix_ChronicPulm,
#          Elix_Depression,
#          Elix_FluidElectrolyte,
#          Elix_Valvular,
#          Elix_RA,
#          Elix_PVD,
#          Elix_SolidTumor,
#          Elix_DeficiencyAnemia ) %>%
#   group_by(YEAR) %>% 
#   summarise(n_cases = n(), 
#             rate = mean(EOAPrescribed),
#             mean_HTN = mean(HTN) ,
#             mean_Elix_Hypothyroid = mean(Elix_Hypothyroid),
#             mean_Elix_Arrythmia = mean(Elix_Arrythmia),
#             mean_Elix_ChronicPulm = mean(Elix_ChronicPulm),
#             mean_Elix_Depression = mean(Elix_Depression),
#             mean_Elix_FluidElectrolyte = mean(Elix_FluidElectrolyte),
#             mean_Elix_Valvular = mean(Elix_Valvular),
#             mean_Elix_RA = mean(Elix_RA),
#             mean_Elix_PVD = mean(Elix_PVD),
#             mean_Elix_SolidTumor = mean(Elix_SolidTumor),
#             mean_Elix_DeficiencyAnemia = mean(Elix_DeficiencyAnemia) ) %>%
#   mutate(Rate = round(100*rate, digits = 2)) %>% 
#   mutate(mean_EOA = rate) %>% 
#   mutate(mean_EOA_nolog = 10*rate) %>% #Might want to use Rate instead -- scale by 100
#   mutate(Year = YEAR) %>% 
#   mutate(Cases = n_cases)
# 
# # PLOT: Rates of EOA and of comorbidities by year
# 
# comorbidities_by_year_pivoted_2 <- comorbidities_by_year_2 %>% 
#   select(-Rate, -rate, -YEAR, -n_cases, -Cases, -mean_EOA_nolog) %>%
#   pivot_longer(cols = starts_with("mean_"), names_to = "Comorbidity", values_to = "Rate")
# 
# ggplot(comorbidities_by_year_pivoted, aes(y = Rate, x = Year, color = Comorbidity, group = Comorbidity)) + 
#   geom_point(show.legend = TRUE) + 
#   geom_smooth(se = FALSE,show.legend = FALSE) + 
#   geom_vline(xintercept = 2018, linetype = "dashed") + 
#   ylab("Log Rate") + 
#   theme_bw() + 
#   scale_color_manual(values = c(rep("grey", 5), "blue", rep("grey", 2))) + 
#   # scale_color_manual(values = c(rep("darkgrey", 15), "blue", rep("darkgrey", 3))) + 
#   scale_y_continuous(trans = "log10")#, sec.axis = sec_axis(~./10, "test"))
# 
# # With color
# ggplot(comorbidities_by_year_pivoted_2, aes(y = Rate, x = Year, color = Comorbidity, group = Comorbidity)) + 
#   geom_point() + 
#   geom_smooth(se = FALSE) + 
#   geom_vline(xintercept = 2018, linetype = "dashed") + 
#   ylab("Log Rate") + 
#   theme_bw() + 
#   # scale_color_manual(values = c(rep("grey", 15), "blue", rep("grey", 3))) + 
#   scale_y_continuous(trans = "log10")#, sec.axis = sec_axis(~./10, "test"))



#####