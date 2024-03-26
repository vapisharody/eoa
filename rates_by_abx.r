
# Get abx rates out of the cohort that had EOA

# Overall
overall <- (d %>% filter(EOAPrescribed == 1))$drug %>% factor() %>% summary() %>% as.data.frame()
colnames(overall) <- "n"
t = sum(overall)
overall %>% mutate(rate = n/t)

# By year
abx_tbl_long <- d %>% filter(EOAPrescribed == 1) %>%
  select(YEAR, drug) %>% 
  group_by(YEAR, drug) %>% 
  summarize(n_cases = n()) %>% ungroup()

abx_tbl <- pivot_wider(abx_tbl_long, names_from = drug, values_from = n_cases) 
# abx_tbl %>% ungroup() %>% mutate(total = rowSums(across(-`YEAR`)))
abx_tbl <- abx_tbl %>% ungroup() %>% mutate(total = rowSums(across(c(where(is.numeric), -`YEAR`))))

abx_tbl = abx_tbl %>% mutate(across(c(where(is.numeric), -`YEAR`), ~./total, .names = "{.col}_rate"))

View(abx_tbl)

ggplot(data = abx_tbl_long) + 
  geom_bar(aes(x = YEAR, y = n_cases, color = drug, fill = drug), stat = "identity") + 
  xlab("Year") + ylab("Number of EOA prescriptions")

abx_tbl_long_rates <- abx_tbl %>% 
  select(YEAR, ends_with("_rate"), -total_rate) %>% 
  pivot_longer(cols = -YEAR, names_to = "drug", values_to = "rate")

ggplot(data = abx_tbl_long_rates) + 
  geom_bar(aes(x = YEAR, y = rate, color = drug, fill = drug), stat = "identity") + 
  xlab("Year") + ylab("Fraction of EOA cases")

###########
# Redo as a fraction of TOTAL cases 
# WARNING: THIS CHANGES ALL THE KEY VARIABLES MADE ABOVE!
##########
# By year
abx_tbl_long <- d %>% #filter(EOAPrescribed == 1) %>%
  select(YEAR, drug) %>% 
  group_by(YEAR, drug) %>% 
  summarize(n_cases = n()) %>% ungroup()

abx_tbl <- pivot_wider(abx_tbl_long, names_from = drug, values_from = n_cases) 
# abx_tbl %>% ungroup() %>% mutate(total = rowSums(across(-`YEAR`)))
abx_tbl <- abx_tbl %>% ungroup() %>% mutate(total = rowSums(across(c(where(is.numeric), -`YEAR`))))

abx_tbl = abx_tbl %>% mutate(across(c(where(is.numeric), -`YEAR`), ~./total, .names = "{.col}_rate"))

abx_tbl_long_rates <- abx_tbl %>% 
  select(YEAR, ends_with("_rate"), -total_rate) %>% 
  pivot_longer(cols = -YEAR, names_to = "drug", values_to = "rate") 

# remove the "no EOA" info
abx_tbl_long <- abx_tbl_long %>% filter(drug != "no EOA")
abx_tbl_long_rates <- abx_tbl_long_rates %>% filter(drug != "no EOA_rate")


## USE THIS ONE: 
ggplot(data = abx_tbl_long_rates) + 
  geom_bar(aes(x = YEAR, y = rate, color = drug, fill = drug), stat = "identity") + 
  xlab("Year") + ylab("Fraction of all cases") + 
  geom_vline(xintercept = 2018, linetype = "dashed") +
  scale_x_continuous(breaks = seq(min(2009), max(2022), by = 1)) #+ 
  # annotate("text", x = 2017, y = 0.07, label = "Landmark publication \n \\(Inabathula et al., 2018\\))", parse=FALSE)
  # geom_text(aes(x=2018, label="\nPaper", y=0.7), colour="red", angle=90)

# 
# ggplot(data = abx_tbl_long) + 
#   geom_bar(aes(x = YEAR, y = n_cases, color = drug, fill = drug), stat = "identity") + 
#   xlab("Year") + ylab("Number of EOA prescriptions")
