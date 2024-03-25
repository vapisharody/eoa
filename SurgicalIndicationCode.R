#Assign New columns with indication for surgery based on DX codes 1 -4 
d_dx <- d


# Added additional codes:
# Primary OA: M1711, M1712  
# Post-traumatic: M1730, M1731, M1732
# 71596, 71536, 71696 not clear -- could be primary or secondary 

# These codes are just pain: 
# M2556, 71946

# Other
# V4365 is just "knee joint replacement" -- not a surgical indication
# Varus deformity: M2116, 73642
# Obesity: E6601, E669, Z6841
# GERD: K219
# OSA: G4733, 27801

primaryoa_codes <- c("71516", "M170", "M171",
                     "M1710", "M1711", "M1712")
ptoa_codes <- c("71526", "M172", "M173", "M1730", "M1731", "M1732")
inflamoa_codes <- c("714", "7140", "M0506", "M0516", "M0526", "M0536", 
                    "M0546", "M0556", "M0566", "M0576", "M0586", 
                    "M064", "M0766", "M0806", "M3505")
unspecOA_codes <- c("71596",  "71536", "71696")

d_dx <- d
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
  mutate(unspecOA = case_when( primaryOA == 0 & 
                                 posttraumaticOA == 0 & 
                                 inflamOA == 0 &
                                 (DX1 %in% unspecOA_codes | 
                                 DX2 %in% unspecOA_codes | 
                                 DX3 %in% unspecOA_codes | 
                                 DX4 %in% unspecOA_codes)  ~ 1, 
                               .default = 0))
d_dx2 <- d_dx2 %>% 
  mutate(other_indication = case_when( primaryOA + 
                                         posttraumaticOA + 
                                         inflamOA + 
                                         unspecOA == 0 & !(is.na(DX1) & 
                                                             is.na(DX2) & 
                                                             is.na(DX3) & 
                                                             is.na(DX4)) ~ 1,
                               .default = 0))


# Should think more about multiple indications var 

d_dx2 <- d_dx2 %>% 
  mutate(multiple_indications = primaryOA + posttraumaticOA + inflamOA + unspecOA + other_indication) %>% 
  mutate(indication = case_when(multiple_indications == 0 ~ 0, 
                                primaryOA == 1 ~ 1, 
                                posttraumaticOA == 1 ~ 2,
                                inflamOA == 1 ~ 3,
                                unspecOA == 1 ~ 4,
                                other_indication == 1 ~ 5,
                                multiple_indications > 1 ~ 6,
                                .default = NA ))
# 
d_dx2 <- d_dx2 %>% 
  mutate(indication.factor = factor(indication, levels = c(0:2), # c(0:6), 
                                    labels = c("Not available", 
                                               "Primary OA",
                                               "Post-traumatic OA"#,
                                               # "Inflammatory OA",
                                               # "Unspecified OA",
                                               # "Other",
                                               # "Multiple Indications"
                                               ))) 




# Find codes we missed
# d_dx2$indication.factor %>% summary()
# # d_dx2 %>% filter(indication.factor == "Other") %>% select(DX1, DX2, DX3, DX4) %>% View()
# d_dx2 %>% filter(indication.factor == "Other") %>% select(DX1) %>% mutate(DX1.factor = factor(DX1)) %>% summary()
# d_dx2 %>% filter(indication.factor == "Other") %>% select(DX3) %>% mutate(DX3.factor = factor(DX3)) %>% summary()
# d_dx2 %>% filter(indication.factor == "Other") %>% select(DX4) %>% mutate(DX4.factor = factor(DX4)) %>% summary()

# Group by procedure_year and indication
EOA_stats_by_year_indication <-
  d_dx2 %>%
  group_by(YEAR, indication.factor) %>%
  summarise(n_cases = n(), rate = mean(EOAPrescribed), sd = sd(EOAPrescribed))

EOA_stats_by_year_indication <- EOA_stats_by_year_indication %>% 
  mutate(Rate = round(100*rate, digits = 2)) %>% 
  mutate(Year = YEAR) %>% 
  mutate(Cases = n_cases)

# d_primaryOA <- d_dx %>% filter(PrimaryOA == 1)
# d_posttraumOA <- d_dx %>% filter(PostTraumaticOA == 1)
# d_inflamOA <- d_dx %>% filter(InflammatoryOA == 1)
# d_otherindication <- d_dx %>% filter(Otherindication == 1)


ggplot(EOA_stats_by_year_indication, aes(y = Rate, x = Year, fill = indication.factor)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  xlab("Year") + ylab("Rate (%)") + #ggtitle("Rate of EOA in overall cohort") +
  scale_x_continuous(breaks = seq(min(EOA_stats_by_year_indication$Year), 
                                  max(EOA_stats_by_year_indication$Year), by = 1)) + 
  geom_vline(xintercept = 2018, linetype = "dashed")



# ggplot(EOA_stats_by_year_indication, aes(y = n_cases, x = Year, fill = indication.factor)) + 
#   geom_bar(stat = "identity", position = "dodge") + 
#   xlab("Year") + ylab("Rate (%)") + #ggtitle("Rate of EOA in overall cohort") +
#   scale_x_continuous(breaks = seq(min(EOA_stats_by_year_indication$Year), 
#                                   max(EOA_stats_by_year_indication$Year), by = 1)) + 
#   geom_vline(xintercept = 2018, linetype = "dashed")



# TABLE: EOA rates by year and cohort
tab_rates <- EOA_stats_by_year_indication %>% 
  select(Year, Rate, indication.factor) %>% 
  pivot_wider(names_from = indication.factor, values_from = Rate)

tab_cases <- EOA_stats_by_year_indication %>% 
  select(Year, n_cases, indication.factor) %>% 
  pivot_wider(names_from = indication.factor, values_from = n_cases)

View(tab_cases)






