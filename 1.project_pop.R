##' ** 1. Import projected country population and associated data - project_bites.R**
##' Data compiled for Gavi VIS (prepped_data_final.csv): 
##' - world bank human population projections (2018 onwards)
##' - Human Dog Ratios (exposure risk)
##' - Urban/ rural populations (relates to dog ownership & PEP access)
##' - Human development index, HDI (relates to PEP access)
##' - Dog vaccination coverage (exposure risk)
##' 
##' ** 2. Project metrics (rabid & healthy bites, PEP demand & vials used) - R/HelperFun.R**
##' - incorporate vial sharing calculations - free, decentralize, vial size 
##' - revised HDR to consider religion (% christian)
##' - project over time horizon for 2 scenarios: SQ v free PEP with improved decentralization

library(tidyverse)
source("R/HelperFun.R")

#  DATA 
df <- read_csv("data/prepped_data_final.csv") # WHO modelling consortium, Lancet Inf Dis, supplementary files
dogs_df <- read_csv("data/wallace_Front2017.csv") # Wallace et al, Frontiers, supplementary file
pop_df <- left_join(df, dogs_df, join_by("CODE"=="Code"))
religion <- read_csv("data/religion_by_country_2024.csv") # https://worldpopulationreview.com/country-rankings/religion-by-country
df2 <- left_join(df, religion, join_by("country")) # include proportion muslim
df2$prop_muslim <- df2$Muslim/df2$total

# Checks (CDC estimates slightly lower HDR)
hdr_cdc <- dogs_df$WB_Human_Pop2015/ dogs_df$TotalDogs
hdr <-  (df$Percent.Urban/100 * df$urban_HDR) + # urban prop dogs
  (1-(df$Percent.Urban/100)) * df$rural_HDR # rural prop dogs
hdr_rel <- (df2$prop_muslim * hdr * 2) + ((1-df2$prop_muslim) * hdr) # Assume dog pop halves in muslim areas 
sum(hdr_cdc); sum(hdr, na.rm=TRUE); sum(hdr_rel, na.rm=TRUE)

# A few fixes
df2$vaccine_free_SQ[grep("KHM|TUN|VNM", df$CODE)] <- TRUE # Tunisia and Cambodia have free PEP & Vietnam have improved access
df2$free <- df2$vaccine_free_SQ
hdr_rel[which(df2$country == "Republic of Moldova")] <- hdr_rel[which(df2$country == "Romania")] # create HDR for Moldova
hdr_rel[which(df2$country == "South Sudan")] <- hdr_rel[which(df2$country == "Sudan")] # create HDR for Sudan
# test <- CalcPEP(code = "TZA", year = i2024, hdr = hdr_rel[172], BI = 0.01, HDI = df2$HDI[172],
#                 free = FALSE, vials_low = 3, vials_high = 1.5); test # Test

# update gavi info
df2$gavi_2024 <- df2$gavi
df2$gavi_2024[which(df2$country == "Syrian Arab Republic")] <- TRUE # Syria now Gavi eligibel! 
df2$gavi_2024[grep(
  "Angola|Armenia|Azerbaijan|Bhutan|Bolivia|Cuba|Georgia|Guyana|Honduras|India|Indonesia|Kiribati|Mongolia|Nicaragua|Moldova|Sri Lanka|Timor-Leste|Ukraine|Uzbekistan|Vietnam", 
  df2$country)] <- FALSE
table(df2$gavi_2024) # Still got some wrong

df2$hdr_rel <- hdr_rel # Assume dog pop halves in muslim areas 

# df2$country[which(df2$ID == TRUE)]
# [1] "Bangladesh"  "Bhutan"      "Cambodia"    "China"       "India"       "Madagascar"  "Pakistan"    "Philippines" "Sri Lanka"   "Thailand"   
# [11] "Vietnam" 
# 
# > df2$country[which(df2$ID_2015 == TRUE)]
# [1] "Bangladesh"  "Bhutan"      "Cambodia"    "China"       "India"       "Madagascar"  "Pakistan"    "Philippines" "Sri Lanka"   "Tanzania"   
# [11] "Thailand"    "Uganda" 
df2$ID_2015[which(df2$country == "Nepal")] 

# > df2$country[which(df2$ID_prop > 0)]
# [1] "Bangladesh"  "Bhutan"      "Philippines" "Vietnam"

# PREP A DATAFRAME FOR EXPORT!
df_small <- df2 %>%
  filter(gavi_2024 == TRUE) %>% 
  select(country, CODE, HDI, gavi_2024, endemic, hdr_rel, ID_2015, free,
         pop2024, pop2025, pop2026, pop2027, pop2028, pop2029, pop2030, pop2031, pop2032, 
         pop2033, pop2034, pop2035, pop2036, pop2037, pop2038, pop2039, pop2040) 
names(df_small)[which(names(df_small) == "ID_2015")] <- "ID"
names(df_small)[which(names(df_small) == "gavi_2024")] <- "gavi"

# df2$country[which(df2$ID == TRUE)]
# [1] "Bangladesh"  "Bhutan"      "Cambodia"    "China"       "India"       "Madagascar"  "Pakistan"    "Philippines" "Sri Lanka"   "Thailand"   
# [11] "Vietnam" 
# 
# > df2$country[which(df2$ID_2015 == TRUE)]
# [1] "Bangladesh"  "Bhutan"      "Cambodia"    "China"       "India"       "Madagascar"  "Pakistan"    "Philippines" "Sri Lanka"   "Tanzania"   
# [11] "Thailand"    "Uganda" 
# 
# > df2$country[which(df2$ID_prop > 0)]
# [1] "Bangladesh"  "Bhutan"      "Philippines" "Vietnam"]


#### ID countries
# > df_small$country[which(df_small$ID == TRUE)]
# [1] "Bangladesh" "Cambodia"   "Madagascar" "Pakistan"   "Tanzania"   "Uganda"    
write_csv(df_small, "output/pop_proj.csv")
write_csv(df_small, "output/pop_proj2024_2040.csv")


# Change the endemic ones!
# df2$endemic[grep("Timor", df2$country)] <- 1 # Timor-Leste now endemic :(
# # Comoros, Papua New Guinea, Sao Tome & Principe, Solomon islands all not endemic
# grep("Comoros|Papua New Guinea|Sao Tome|Solomon", df2$country)

