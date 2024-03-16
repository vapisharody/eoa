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
# EOAMainCohort <- read_excel("Latest Data/EOAMainCohort.xlsx")
# EOAMainCohort_untouched <- EOAMainCohort
d  <- EOAMainCohort_untouched
# 
# drugfilecombined <- read_sas("Latest Data/drugfilecombined.sas7bdat", NULL)
# drugfilecombined_untouched <- drugfilecombined
# drugfilecombined <- drugfilecombined_untouched

# Seems that the main cohort excel file from 3/15/2024 had a merge that didn't quite go right
# Clearing out the issues w the merge

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

# Make EOA factor variable
d <- d %>% mutate(EOAPrescribed.factor = factor(EOAPrescribed, levels = c(0, 1), labels = c("No EOA", "EOA")))
# d$EOAPrescribed.factor %>% summary()

# Make age group, sex, UKA factor variables
# SLOWWWWWW
d <- d %>% mutate(AGEGRP.factor = factor(AGEGRP))
d <- d %>% mutate(female = (SEX == "2")) %>% mutate(female = factor(female))
d <- d %>% mutate(UKA.factor = factor(UKA))
d <- d %>% mutate(priorUKA.factor = factor(priorUKA))
d <- d %>% mutate(DiabetesMellitus.factor = factor(DiabetesMellitus))
d <- d %>% mutate(Elix_Obesity.factor = factor(Elix_Obesity))
d <- d %>% mutate(DAYSUPP.factor = factor(DAYSUPP))
d <- d %>% mutate(UKA.factor = factor(UKA))
d <- d %>% mutate(priorUKA.factor = factor(priorUKA))
d <- d %>% mutate(AutoimmuneDz.factor = factor(AutoimmuneDz))
d <- d %>% mutate(CKD.factor = factor(CKD))
d <- d %>% mutate(Elix_RenalFailure.factor = factor(Elix_RenalFailure))
d <- d %>% mutate(ActiveSmoking.factor = factor(ActiveSmoking))
d <- d %>% mutate(DiabetesMellitus.factor = factor(DiabetesMellitus))
d <- d %>% mutate(Elix_DiabetesUncomp.factor = factor(Elix_DiabetesUncomp))
d <- d %>% mutate(Elix_DiabetesComp.factor = factor(Elix_DiabetesComp))
d <- d %>% mutate(StasisDermatitis.factor = factor(StasisDermatitis))
d <- d %>% mutate(ChronicCystitis.factor = factor(ChronicCystitis))
d <- d %>% mutate(Elix_Obesity.factor = factor(Elix_Obesity))
d <- d %>% mutate(HxSepsis.factor = factor(HxSepsis))
d <- d %>% mutate(MRSA_MSSAColonization.factor = factor(MRSA_MSSAColonization))
d <- d %>% mutate(HepC.factor = factor(HepC))


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
                        Elix_Obesity,
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

d <- d %>% 
  rowwise() %>%
  mutate(Elix = sum(c_across(starts_with("Elix_")), na.rm = FALSE)) 

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

View(EOA_stats_by_year_cohort)


# Group by procedure_year AND very high risk cohort
EOA_stats_by_year_cohort_veryhigh <-
  d %>% 
  group_by(YEAR, very_high_risk.factor) %>% 
  summarise(n_cases = n(), rate = mean(EOAPrescribed), sd = sd(EOAPrescribed)) %>% 
  mutate(Rate = round(100*rate, digits = 2)) %>% 
  mutate(Year = YEAR) %>% 
  mutate(Cases = n_cases) %>%
  mutate(Risk = very_high_risk.factor)

View(EOA_stats_by_year_cohort_veryhigh)


# PLOT: Rate of EOA vs year for overall cohort
# ggplot(EOA_stats_by_year, aes(y = rate, x = YEAR)) + geom_point() + geom_smooth()
ggplot(EOA_stats_by_year, aes(y = Rate, x = Year)) + 
  geom_bar(stat = "identity") + 
  xlab("Year") + ylab("Rate (%)") + ggtitle("Rate of EOA in overall cohort") +
  scale_x_continuous(breaks = seq(min(EOA_stats_by_year$Year), max(EOA_stats_by_year$Year), by = 2))

# PLOT: Rate of EOA vs year by risk cohort
ggplot(EOA_stats_by_year_cohort, aes(y = Rate, x = Year, color = Risk, fill = Risk)) + 
  geom_bar(stat = "identity", position = position_dodge()) +
  xlab("Year") + ylab("Rate (%)") + ggtitle("Rate of EOA in overall cohort") +
  scale_x_continuous(breaks = seq(min(EOA_stats_by_year$Year), max(EOA_stats_by_year$Year), by = 2))


# PLOT: Rate of EOA vs year by risk cohort (w very high risk)
ggplot(EOA_stats_by_year_cohort_veryhigh, aes(y = Rate, x = Year, color = Risk, fill = Risk)) + 
  geom_bar(stat = "identity", position = position_dodge()) +
  xlab("Year") + ylab("Rate (%)") + ggtitle("Rate of EOA in overall cohort") +
  scale_x_continuous(breaks = seq(min(EOA_stats_by_year$Year), max(EOA_stats_by_year$Year), by = 2))



# PLOT: Rate of EOA vs year by risk cohort
ggplot(EOA_stats_by_year_cohort, aes(y = Rate, x = Year, color = Risk, fill = Risk)) + geom_point() #+ geom_smooth()


d %>% filter(risk_sum >= 1) %>% group_by(AutoimmuneDz, 
                                       CKD,  
                                       Elix_RenalFailure, 
                                       ActiveSmoking,  
                                       Elix_DiabetesUncomp, 
                                       Elix_DiabetesComp,  
                                       StasisDermatitis,  
                                       ChronicCystitis, 
                                       Elix_Obesity,
                                       HxSepsis, 
                                       MRSA_MSSAColonization,
                                       HepC) %>% summarise(n = count())

# Table: Risk factors by risk cohort
table_risk_factors <- d %>% 
  group_by(high_risk.factor) %>% summarise(AutoimmuneDz_n = sum(AutoimmuneDz), 
                                         CKD_n = sum(CKD),
                                         Elix_RenalFailure_n = sum(Elix_RenalFailure), 
                                         ActiveSmoking_n = sum(ActiveSmoking),  
                                         Elix_DiabetesUncomp_n = sum(Elix_DiabetesUncomp), 
                                         Elix_DiabetesComp_n = sum(Elix_DiabetesComp),  
                                         StasisDermatitis_n = sum(StasisDermatitis),
                                         ChronicCystitis_n = sum(ChronicCystitis), 
                                         Elix_Obesity_n = sum(Elix_Obesity),
                                         HxSepsis_n = sum(HxSepsis),
                                         MRSA_MSSAColonization_n = sum(MRSA_MSSAColonization),
                                         HepC_n = sum(HepC)) %>%
  pivot_longer(cols = !high_risk.factor) %>% pivot_wider(names_from = high_risk.factor)

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
                                           Elix_Obesity_n = sum(Elix_Obesity),
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
                                                Elix_Obesity_n = sum(Elix_Obesity),
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

label(d$Elix_Obesity.factor) <- "Obese"
label(d$DAYSUPP.factor) <- "Length of antibiotic course"
label(d$UKA.factor) <- "UKA"
label(d$priorUKA.factor) <- "History of UKA"
label(d$AutoimmuneDz.factor) = "Autoimmune Disease"
label(d$CKD.factor)= "CKD"
label(d$Elix_RenalFailure.factor)= "Renal Failure"
label(d$ActiveSmoking.factor)= "Current Smoker"
label(d$DiabetesMellitus.factor)= "Diabetes Mellitus"
label(d$Elix_DiabetesUncomp.factor)= "Diabetes (uncomplicated)"
label(d$Elix_DiabetesComp.factor)= "Diabetes (with complication)"
label(d$StasisDermatitis.factor) = "Stasis Dermatitis"
label(d$ChronicCystitis.factor)= "Chronic Cystitis"
label(d$Elix_Obesity.factor) = "Obesity"
label(d$HxSepsis.factor) = "History of Sepsis"
label(d$MRSA_MSSAColonization.factor)= "MRSA or MSSA Nasal Colonization"
label(d$HepC.factor) = "Hepatitis C"

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
         UKA.factor +
         priorUKA.factor + 
         Elix + 
         Elix_Obesity.factor +
         DiabetesMellitus.factor + 
         ActiveSmoking.factor +  
         HxSepsis.factor + 
         MRSA_MSSAColonization.factor +
         CKD.factor +  
         Elix_RenalFailure.factor + 
         AutoimmuneDz.factor + 
         StasisDermatitis.factor +  
         ChronicCystitis.factor + 
         HepC.factor, 
       caption = "Table: Descriptive statistics",
       data = d,
       render.continuous=cont_vars,
       render.categorical=cat_vars,
       overall = T)


# Table: Demographics of overall cohort, w and w/out abx 
table1(~AGE + 
         female +
         UKA.factor +
         priorUKA.factor + 
         Elix + 
         Elix_Obesity.factor +
         DiabetesMellitus.factor + 
         ActiveSmoking.factor +  
         HxSepsis.factor + 
         MRSA_MSSAColonization.factor +
         CKD.factor +  
         Elix_RenalFailure.factor + 
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

# Check for correlation between the "numeric" patient variables  
# install.packages("ggcorrplot")
library(ggcorrplot)
reduced_data <- d %>% select(AutoimmuneDz,
                               CKD,
                               Elix_RenalFailure,
                               ActiveSmoking,
                               Elix_DiabetesUncomp,
                               Elix_DiabetesComp,
                               StasisDermatitis,
                               ChronicCystitis,
                               Elix_Obesity,
                               HxSepsis,
                               MRSA_MSSAColonization,
                               HepC,
                               priorUKA)
# reduced_data.factor <- d %>% select(female,
#                                     AutoimmuneDz.factor,
#                                    CKD.factor,
#                                    Elix_RenalFailure.factor,
#                                    ActiveSmoking.factor,
#                                    Elix_DiabetesUncomp.factor,
#                                    Elix_DiabetesComp.factor,
#                                    StasisDermatitis.factor,
#                                    ChronicCystitis.factor,
#                                    Elix_Obesity.factor,
#                                    HxSepsis.factor,
#                                    MRSA_MSSAColonization.factor,
#                                    HepC.factor,
#                                    priorUKA.factor)
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
model_full <- 
glm(EOAPrescribed ~ AGE +
      female +
      # Elix + 
      Elix_Obesity.factor +
      DiabetesMellitus.factor + 
      ActiveSmoking.factor +  
      HxSepsis.factor + 
      MRSA_MSSAColonization.factor +
      CKD.factor +  
      # Elix_RenalFailure.factor + 
      AutoimmuneDz.factor, data = d, family=binomial(link="logit"))
summary(model_full)

# Removing
model_1 <- 
  glm(EOAPrescribed ~ AGE +
        female +
        # Elix + 
        Elix_Obesity.factor +
        DiabetesMellitus.factor + 
        # ActiveSmoking.factor +  
        HxSepsis.factor + 
        MRSA_MSSAColonization.factor +
        CKD.factor +  
        AutoimmuneDz.factor, data = d, family=binomial(link="logit"))
summary(model_1)


