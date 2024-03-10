library(haven, tidyverse)
drugfilecombined <- read_sas("Latest Data/drugfilecombined.sas7bdat", NULL)
drugfilecombined_head <- head(drugfilecombined)

colnames(drugfilecombined) %>% View()

drugfilecombined_head %>% View()
