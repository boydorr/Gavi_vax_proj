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
hdr_rel[which(df2$country == "Republic of Moldova")] <- hdr_rel[which(df2$country == "Romania")] # create HDR for Moldova
hdr_rel[which(df2$country == "South Sudan")] <- hdr_rel[which(df2$country == "Sudan")] # create HDR for Sudan
# test <- CalcPEP(code = "TZA", year = i2024, hdr = hdr_rel[172], BI = 0.01, HDI = df2$HDI[172],
#                 free = FALSE, vials_low = 3, vials_high = 1.5); test # Test

# set up calculations
i2024 <- which(names(df) == "pop2024")
proj_df <- data.frame(country = c(), HDI = c(), pop = c(), 
                      bites = c(), healthy = c(), rabid = c(), 
                      PEP = c(), free = c(), vials = c())
country_idx <- which(df2$gavi == TRUE & df2$endemic == 1)
df_gavi <- df2[country_idx,]
hdr_gavi <- hdr_rel[country_idx]

# set up reporting structures for projections over time horizon
yrs <- 2024:2033
proj_PEP_demand <- proj_vial_demand <- matrix(NA, nrow = nrow(df_gavi), ncol = length(yrs))

# Run model across Gavi countries over 10y time horizon under SQ
for (y in 1:length(yrs)){ # determine year
  yi <- grep(yrs[y], names(df_gavi)) # select yr index
  print(yrs[y]) # print year to keep check
  
  for (i in 1: nrow(df_gavi)){ # loop through countries
    # run projection
    country_proj <- CalcPEP(
      code = df_gavi$CODE[i], 
      year = yi, 
      hdr = hdr_gavi[i],
      BI = 0.01, # Bite incidence - adj by vax cov for rabies, vs PEP access/system for healthy dog bites (HDI used as proxy)
      HDI = df_gavi$HDI[i], # captures decentralization
      free =  df_gavi$vaccine_free_SQ[i], # vaccine free? (alternately could use $vaccine_paid_by_patient )
      vials_low = 3, vials_high = 1.5) # Incorporate vial calculations
    # return key variables
    proj_PEP_demand[i,y] <- country_proj$PEP
    proj_vial_demand[i,y] <- country_proj$vials    
  }
}

# Create data frames for export
proj_PEP_demand <- proj_PEP_demand %>% data.frame() %>% setNames(yrs)
proj_vial_demand <- proj_vial_demand %>% data.frame() %>% setNames(yrs)
proj_vial_demand$code <- df_gavi$CODE
proj_vial_demand$country <- df_gavi$country
proj_PEP_demand$code <- df_gavi$CODE
proj_PEP_demand$country <- df_gavi$country
write_csv(proj_PEP_demand, "output/proj_patients_SQ.csv")
write_csv(proj_vial_demand, "output/proj_vials_SQ.csv")

# Run model across Gavi countries over 10y time horizon under Gavi investment
proj_PEP_demand <- proj_vial_demand <- matrix(NA, nrow = nrow(df_gavi), ncol = length(yrs))

for (y in 1:length(yrs)){ # determine year
  yi <- grep(yrs[y], names(df_gavi)) # select yr index
  print(yrs[y]) # print year to keep check
    
  for (i in 1: nrow(df_gavi)){ # loop through countries
    cap <- ifelse(df_gavi$HDI[i]>0.65, df_gavi$HDI[i], 0.65) # set a cap for impact of decentralization
    # run projection
    country_proj <- CalcPEP(
      code = df_gavi$CODE[i], 
      year = yi, 
      hdr = hdr_gavi[i],
      BI = 0.01, # Bite incidence - adj by vax cov for rabies, vs PEP access/system for healthy dog bites (HDI used as proxy)
      HDI = min(df_gavi$HDI[i] + (0.01*y), cap), # increment increased decentralization (0.01 per year)
      free =  TRUE, # vaccine free? (alternately could use $vaccine_paid_by_patient )
      vials_low = 3, vials_high = 1.5) # Incorporate vial calculations
    
    # return key variables
    proj_PEP_demand[i,y] <- country_proj$PEP
    proj_vial_demand[i,y] <- country_proj$vials
  }
}

# Create data frames for export
proj_PEP_demand <- proj_PEP_demand %>% data.frame() %>% setNames(yrs)
proj_vial_demand <- proj_vial_demand %>% data.frame() %>% setNames(yrs)
proj_vial_demand$code <- df_gavi$CODE
proj_vial_demand$country <- df_gavi$country
proj_PEP_demand$code <- df_gavi$CODE
proj_PEP_demand$country <- df_gavi$country
write_csv(proj_PEP_demand, "output/proj_patients_gavi.csv")
write_csv(proj_vial_demand, "output/proj_vials_gavi.csv")

# Compare scenarios
vials_SQ <- read_csv("output/proj_vials_SQ.csv")
vials_gavi <- read_csv("output/proj_vials_gavi.csv")
pep_SQ <- read_csv("output/proj_patients_SQ.csv")
pep_gavi <- read_csv("output/proj_patients_gavi.csv")

# save plots of patients presenting by scenario & vaccine use under Gavi (SQ, Gavi investment)
pdf("figs/proj_vials&PEP.pdf")
par(mfrow=c(4,4), mar = c(2,2,1,1))
for (i in 1:nrow(vials_SQ)){
  plot(2024:2033, pep_SQ[i,1:10], lty = 2, type = "l", ylim = c(0, 250000), 
       ylab = "Numbers", xlab = "", main = pep_SQ$country[i]) # HDI of countries
  # lines(2024:2033, vials_SQ[i,1:10])
  lines(2024:2033, pep_gavi[i,1:10], col="red", lty = 2)
  lines(2024:2033, vials_gavi[i,1:10], col="red")
}
dev.off()

# Plot change in patients presenting & vaccine use under Gavi in 2030 - according to each countries HDI
pdf("figs/proj_demand_PC.pdf")
par(mfrow=c(1,1), mar = c(4,5,1,1))
plot(df_gavi$HDI, pep_SQ$'2030'*100000/df_gavi$pop2030, 
     pch=20, ylab = "patients or vials/100k", xlab = "HDI", ylim = c(0, 800))
points(df_gavi$HDI, pep_gavi$'2030'*100000/df_gavi$pop2030, col="red")
points(df_gavi$HDI, vials_gavi$'2030'*100000/df_gavi$pop2030, col="blue")
dev.off()
