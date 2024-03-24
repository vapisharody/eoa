#Assign New columns with indication for surgery based on DX codes 1 -4 

EOAFinal <- EOAFinal %>%
  mutate(PrimaryOA = if_else(
    stringr::str_detect(DX1, "71516|M170|M171") |
      stringr::str_detect(DX2, "71516|M170|M171") |
      stringr::str_detect(DX3, "71516|M170|M171") |
      stringr::str_detect(DX4, "71516|M170|M171"), 1,0 ))

EOAFinal <- EOAFinal %>%
  mutate(PostTraumaticOA = if_else(
    stringr::str_detect(DX1, "71526|M172|M173") |
      stringr::str_detect(DX2, "71526|M172|M1713") |
      stringr::str_detect(DX3, "71526|M172|M1713") |
      stringr::str_detect(DX4, "71526|M172|M1713"), 1,0 ))

EOAFinal <- EOAFinal %>%
  mutate(InflammatoryOA = if_else(
    stringr::str_detect(DX1, "714|M0506|M0516|M0526|M0536|M0546|M0556|M0566|M0576|M0586|M064|M0766|M0806|M3505") |
      stringr::str_detect(DX2, "714|M0506|M0516|M0526|M0536|M0546|M0556|M0566|M0576|M0586|M064|M0766|M0806|M3505") |
      stringr::str_detect(DX3, "714|M0506|M0516|M0526|M0536|M0546|M0556|M0566|M0576|M0586|M064|M0766|M0806|M3505") |
      stringr::str_detect(DX4, "714|M0506|M0516|M0526|M0536|M0546|M0556|M0566|M0576|M0586|M064|M0766|M0806|M3505"), 1,0 ))


#If not assigned to first 3 groups then will place into other category 
EOAFinal <- EOAFinal %>%
  mutate(Otherindication = if_else(PrimaryOA == 0 & PostTraumaticOA == 0 & InflammatoryOA == 0, 1, 0))