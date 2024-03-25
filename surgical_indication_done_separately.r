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


d_primaryOA <- d_dx2 %>% filter(primaryOA == 1)
d_posttraumOA <- d_dx2 %>% filter(posttraumaticOA == 1)
d_inflamOA <- d_dx2 %>% filter(inflamOA == 1)
d_unspecOA <- d_dx2 %>% filter(unspecOA == 1)
d_otherindication <- d_dx2 %>% filter(other_indication == 1)






make_stats <- function(dat){
  
  stats_by_year <-
    dat %>%
    group_by(YEAR) %>%
    summarise(n_cases = n(), rate = mean(EOAPrescribed), sd = sd(EOAPrescribed))
  
  stats_by_year <- stats_by_year %>% 
    mutate(Rate = round(100*rate, digits = 2)) %>% 
    mutate(Year = YEAR) %>% 
    mutate(Cases = n_cases)
  
  return(stats_by_year)
}

make_graph <- function(dat, s){
  
  stats_by_year <- make_stats(dat)
  
  p <- ggplot(stats_by_year, aes(y = Rate, x = Year)) + 
    geom_bar(stat = "identity", position = "dodge") + 
    xlab("Year") + ylab("Rate (%)") + ggtitle(paste0("Rate of EOA: ", s)) +
    scale_x_continuous(breaks = seq(min(EOA_stats_by_year_indication$Year), 
                                    max(EOA_stats_by_year_indication$Year), by = 1)) + 
    geom_vline(xintercept = 2018, linetype = "dashed")
  
  return(p)
}

t_primary <- make_stats(d_primaryOA)
t_posttrauma <- make_stats(d_posttraumOA)
t_inflam <- make_stats(d_inflamOA)
t_unspec <- make_stats(d_unspecOA)
t_other <- make_stats(d_otherindication)


t_primary
t_posttrauma
t_inflam
t_unspec
t_other



p_primary <- make_graph(d_primaryOA, "Primary OA")
p_posttrauma <- make_graph(d_posttraumOA, "Post-traumatic osteoarthritis")
p_inflam <- make_graph(d_inflamOA, "Inflammatory arthritis")
p_unspec <- make_graph(d_unspecOA, "Unspecified arthritis")
p_other <- make_graph(d_otherindication, "Other indication")



p_primary
p_posttrauma
p_inflam
p_unspec
p_other

# library(readr)
# rates_by_indication <- read_csv("rates_by_indication.csv")
 
# 
# ggplot(data = rates_by_indication) + 
#   geom_point(aes(x = YEAR, y = Primary_Rate)) +
#   geom_point(aes(x = YEAR, y = `Post-traumatic_Rates`)) + 
#   geom_point(aes(x = YEAR, y = `Unspec_Rate`)) + 
#   geom_point(aes(x = YEAR, y = Inflammatory_Rate))
# 
# 
