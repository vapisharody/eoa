
# Run top of analysis_with_combined_file.r
# Through end of code on high risk cohort
# Should be w file EOARatesCohort.xlsx

d_same_day <- d %>% filter(is.na(SVCDATEDrug) | SVCDATE==SVCDATEDrug)


d <- d_same_day
# TABLE: EOA vs high-risk
table(d$high_risk.factor, d$EOAPrescribed.factor)


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


# TABLE: EOA rates by year and cohort
EOA_stats_by_year_cohort %>% ungroup() %>% 
  select(Year, Cases, Rate, Risk) %>% group_by(Year) %>%
  pivot_wider(names_from = Risk, values_from = c(Rate, Cases)) %>% 
  View()


# PLOT: Rate of EOA vs year by risk cohort
ggplot(EOA_stats_by_year_cohort, aes(y = Rate, x = Year, color = Risk, fill = Risk)) + 
  geom_bar(stat = "identity", position = position_dodge()) +
  xlab("Year") + ylab("Rate (%)") + ggtitle("Rate of EOA by risk cohort -- day-of-surgery scripts only") +
  scale_x_continuous(breaks = seq(min(EOA_stats_by_year$Year), max(EOA_stats_by_year$Year), by = 1)) + geom_vline(xintercept = 2018, linetype = "dashed")
