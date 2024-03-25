#Assign New columns with indication for surgery based on DX codes 1 -4 
d_dx <- d


# Added additional codes:
# Primary OA: 71596, M1711, M1712  


# These codes are just pain: 
# M2556, 71946

# Other
# Varus deformity: M2116  
# Obesity: E6601, E669

primaryoa_codes <- c("71516", "M170", "M171", "71596", "M1711")
ptoa_codes <- c("71526", "M172", "M173")
inflamoa_codes <- c("714", "M0506", "M0516", "M0526", "M0536", 
                    "M0546", "M0556", "M0566", "M0576", "M0586", 
                    "M064", "M0766", "M0806", "M3505")

d_dx2 <- d_dx %>% 
  mutate(primaryOA = case_when( DX1 %in% primaryoa_codes | 
                                  DX2 %in% primaryoa_codes | 
                                  DX3 %in% primaryoa_codes | 
                                  DX4 %in% primaryoa_codes ~ 1, 
                                .default = 0))
d_dx2 <- d_dx2 %>% 
  mutate(posttraumaticOA = case_when( DX1 %in% ptoa_codes | 
                                  DX2 %in% ptoa_codes | 
                                  DX3 %in% ptoa_codes | 
                                  DX4 %in% ptoa_codes ~ 1, 
                                  .default = 0))
d_dx2 <- d_dx2 %>% 
  mutate(inflamOA = case_when( DX1 %in% inflamoa_codes | 
                                        DX2 %in% inflamoa_codes | 
                                        DX3 %in% inflamoa_codes | 
                                        DX4 %in% inflamoa_codes ~ 1, 
                                      .default = 0))
d_dx2 <- d_dx2 %>% 
  mutate(other_indication = case_when( primaryOA + 
                                         posttraumaticOA + 
                                         inflamOA == 0 & !(is.na(DX1) & 
                                                             is.na(DX2) & 
                                                             is.na(DX3) & 
                                                             is.na(DX4)) ~ 1,
                               .default = 0))


# Mult indications isn't technically quite right but won't be used in actual results 

d_dx2 <- d_dx2 %>% 
  mutate(multiple_indications = primaryOA + posttraumaticOA + inflamOA + other_indication) %>% 
  mutate(indication = case_when(multiple_indications == 0 ~ 0, 
                                multiple_indications > 1 ~ 5,
                                primaryOA == 1 ~ 1, 
                                posttraumaticOA == 1 ~ 2,
                                inflamOA == 1 ~ 3,
                                other_indication == 1 ~ 4,
                                .default = NA ))

d_dx2 <- d_dx2 %>% 
  mutate(indication.factor = factor(indication, levels = c(0:5), 
                                    labels = c("Not available", 
                                               "Primary OA",
                                               "Post-traumatic OA",
                                               "Inflammatory OA",
                                               "Other",
                                               "Multiple Indications"))) 




# Find codes we missed
d_dx2$indication.factor %>% summary()
d_dx2 %>% filter(indication.factor == "Other") %>% select(DX1, DX2, DX3, DX4) %>% View()
d_dx2 %>% filter(indication.factor == "Other") %>% select(DX1) %>% mutate(DX1.factor = factor(DX1)) %>% summary()
d_dx2 %>% filter(indication.factor == "Other") %>% select(DX2) %>% mutate(DX2.factor = factor(DX2)) %>% summary()













d_dx <- d_dx %>%
  mutate(PrimaryOA = if_else(
    stringr::str_detect(DX1, "71516|M170|M171") |
      stringr::str_detect(DX2, "71516|M170|M171") |
      stringr::str_detect(DX3, "71516|M170|M171") |
      stringr::str_detect(DX4, "71516|M170|M171"), 1,0 ))

d_dx <- d_dx %>%
  mutate(PostTraumaticOA = if_else(
    stringr::str_detect(DX1, "71526|M172|M173") |
      stringr::str_detect(DX2, "71526|M172|M1713") |
      stringr::str_detect(DX3, "71526|M172|M1713") |
      stringr::str_detect(DX4, "71526|M172|M1713"), 1,0 ))

d_dx <- d_dx %>%
  mutate(InflammatoryOA = if_else(
    stringr::str_detect(DX1, "714|M0506|M0516|M0526|M0536|M0546|M0556|M0566|M0576|M0586|M064|M0766|M0806|M3505") |
      stringr::str_detect(DX2, "714|M0506|M0516|M0526|M0536|M0546|M0556|M0566|M0576|M0586|M064|M0766|M0806|M3505") |
      stringr::str_detect(DX3, "714|M0506|M0516|M0526|M0536|M0546|M0556|M0566|M0576|M0586|M064|M0766|M0806|M3505") |
      stringr::str_detect(DX4, "714|M0506|M0516|M0526|M0536|M0546|M0556|M0566|M0576|M0586|M064|M0766|M0806|M3505"), 1,0 ))


#If not assigned to first 3 groups then will place into other category 
d_dx <- d_dx %>%
  mutate(Otherindication = if_else(PrimaryOA == 0 & PostTraumaticOA == 0 & InflammatoryOA == 0, 1, 0))

d_dx <- d_dx %>% mutate(multiple_indications = PrimaryOA + PostTraumaticOA + InflammatoryOA)

# These groups aren't exclusive :( 
d_dx <- d_dx %>%
  mutate(indication = case_when(PrimaryOA == 1 ~ 1,
                                PostTraumaticOA == 1 ~ 2,
                                InflammatoryOA == 1 ~ 3,
                                Otherindication == 1 ~ 4,
                                .default = NA)) %>%
  mutate(indication.factor = factor(indication, levels = c(1:4),
                                    labels = c("Primary OA",
                                               "Post-traumatic OA",
                                               "Inflammatory OA",
                                               "Other")))
d_dx %>% select(PrimaryOA, PostTraumaticOA, InflammatoryOA, indication, indication.factor) %>% View()

# Group by procedure_year and indication
EOA_stats_by_year_indication <-
  d_dx %>%
  group_by(YEAR, PrimaryOA, PostTraumaticOA, InflammatoryOA, Otherindication) %>%
  summarise(n_cases = n(), rate = mean(EOAPrescribed), sd = sd(EOAPrescribed))

EOA_stats_by_year_indication <- EOA_stats_by_year_indication %>% 
  mutate(Rate = round(100*rate, digits = 2)) %>% 
  mutate(Year = YEAR) %>% 
  mutate(Cases = n_cases)

# d_primaryOA <- d_dx %>% filter(PrimaryOA == 1)
# d_posttraumOA <- d_dx %>% filter(PostTraumaticOA == 1)
# d_inflamOA <- d_dx %>% filter(InflammatoryOA == 1)
# d_otherindication <- d_dx %>% filter(Otherindication == 1)


ggplot(EOA_stats_by_year_indication, aes(y = Rate, x = Year, color = PrimaryOA)) + 
  geom_bar(stat = "identity") + 
  xlab("Year") + ylab("Rate (%)") + #ggtitle("Rate of EOA in overall cohort") +
  scale_x_continuous(breaks = seq(min(EOA_stats_by_year_indication$Year), 
                                  max(EOA_stats_by_year_indication$Year), by = 1)) + 
  geom_vline(xintercept = 2018, linetype = "dashed")


