# Seems that the main cohort excel file from 3/15/2024 had a merge that didn't quite go right
# Clearing out the issues w the merge
# Keeping in a separate file since this probably won't be a permanent issue
# Need to check why there is an NDC column in the EOA excel file 

# EOAMainCohort %>% select(ends_with(".x"), ends_with(".y")) %>% colnames() %>% View()

EOAMainCohort <- EOAMainCohort %>% 
  rename_with( ~ unlist(strsplit(.x, ".x")), ends_with(".x")) %>% 
  select(-ends_with(".y")) # %>% colnames() %>% View()


# Need to remove NDC column from the EOA excel file 

EOAMainCohort <- EOAMainCohort %>% select(-NDCNUM)

# Was this file already limited to a smaller subset?
EOAMainCohort <- EOAMainCohort %>% select(-contains(t))

t <- 
c("NDCNUM", "AGEGRP", "COPAY", "DATATYP", "DEDUCT", "DOBYR", "EESTATU", "EGEOLOC",
  "EIDFLAG", "EMPREL", "ENRFLAG", "HLTHPLAN", "MHSACOVG", "MSA", "NETPAY",
  "NTWKPROV", "PAIDNTWK", "PAY", "PHYFLAG", "PLANTYP", "QTY", "REGION",
  "SEQNUM", "SEX", "VERSION", "LOS", "AWP", "CAP_SVC", "COB",
  "COINS", "DAWIND", "DAYSUPP", "DEACLAS", "DISPFEE", "EECLASS", "GENERID",
  "GENIND", "INDSTRY", "INGCOST", "MAINTIN", "METQTY", "PDDATE", "PHARMID",
  "REFILL", "RXMR", "SALETAX", "SVCDATEDrug", "THERCLS", "THERGRP", "SVCDATEProcedure",
  "daysbefore", "daysafter")


# EOAMainCohort %>% colnames() %>% View()
