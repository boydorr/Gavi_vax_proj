##' ** 1. Import projected country population and associated data - 1. proj_pp.R**
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
df <- read_csv("output/pop_proj2024_2040.csv") 
endemic <- df %>% 
  filter(endemic == 1)

vials <- read_csv("data/vaccine_use.csv") 
ipc <- subset(vials, regimen == "IPC")
V_low_com <- subset(ipc, completeness =="Complete" & Setting == "rural")$vial
V_high_com <- subset(ipc, completeness =="Complete" & Setting == "urban")$vial
V_low_incom <- subset(ipc, completeness =="Incomplete" & Setting == "rural")$vial
V_high_incom <- subset(ipc, completeness =="Incomplete" & Setting == "urban")$vial


# set up simulation info
yrs <- 2024:2040 # y for projection
intro_y <- which(yrs == 2025) # date of intro
pseek_cap <- 0.95
pcomplete_cap <- 0.9
step <- 0.1
inc <- 0.03

# gavi_fun(j-1, df$HDI[i], step = step, inc = inc, cap_p = pcomplete_cap)

# create vectors for recording probabilities/ projections
Pseek_rabid <- Pseek_healthy <- Pcomplete <- rep(NA, length(yrs))
proj_bite_patients <- proj_rabid <- proj_healthy <- proj_vials <- matrix(NA, ncol = length(yrs), nrow = nrow(df))

# Look through all countries, and create their probability vector
for(i in 1:nrow(df)){

  # set baseline probabilities based on if PEP is free/ charged for
  if(df$free[i] == TRUE){
    Pseek_rabid_base =  df$HDI[i]^(1/2)
    Pseek_health_base = df$HDI[i]^(1/5) 
  } else {
    Pseek_rabid_base =  sqrt(df$HDI[i])
    Pseek_health_base = sqrt(df$HDI[i]) 
  }
  
  # create projections
  for(j in 1: length(yrs)){
    # vector of probabilities assuming Gavi support from 2025
    Pseek_rabid[j] <- gavi_fun(j-1, Pseek_rabid_base, step = step, inc = inc, cap_p = pseek_cap)
    Pseek_healthy[j] <- gavi_fun(j-1, Pseek_health_base, step = step, inc = inc, cap_p = pseek_cap)
    Pcomplete[j] <- gavi_fun(j-1, df$HDI[i]^(1/2), step = step, inc = inc, cap_p = pcomplete_cap)
    
    # rabid bites, healthy bites and bite patients
    yi <- grep(yrs[j], names(df)) 
    bites_res <- CalcBites(BI = 0.01, pop = df[i, yi], hdr = df$hdr_rel[i], HDI = df$HDI[i], 
                           Pseek_R = Pseek_rabid[j], 
                           Pseek_H = Pseek_healthy[j])
    # store projections
    proj_bite_patients[i,j] <- unlist(bites_res$patients)
    proj_rabid[i,j] <- unlist(bites_res$rabid)
    proj_healthy[i,j] <- unlist(bites_res$healthy)
    
    proj_vials[i,j] <- VialCalc2(unlist(bites_res$patients), df$HDI[i],  Pcomplete[j],
                                 vials_low_complete = V_low_com,
                                 vials_high_complete = V_high_com,
                                 vials_low_incomplete = V_low_incom,
                                 vials_high_incomplete = V_low_incom)
  }
  print(i)
}


# Output projections
proj_vials <- proj_vials %>% data.frame() %>% setNames(yrs)
proj_bite_patients <- proj_bite_patients %>% data.frame() %>% setNames(yrs)
proj_rabid <- proj_rabid %>% data.frame() %>% setNames(yrs)
proj_vials$code <- df$CODE
proj_vials$country <- df$country
proj_bite_patients$code <- df$CODE
proj_bite_patients$country <- df$country
proj_rabid$code <- df$CODE
proj_rabid$country <- df$country

write_csv(proj_vials, "output/proj_vials_2024_2040.csv")
write_csv(proj_bite_patients, "output/proj_patients_2024_2040.csv")
write_csv(proj_rabid, "output/proj_rabid_2024_2040.csv")


# save plots of patients presenting by scenario & vaccine use under Gavi (SQ, Gavi investment)
pdf("figs/proj_vials&PEP_2024_2040.pdf")
par(mfrow=c(4,4), mar = c(2,2,1,1))
for (i in 1:nrow(proj_vials)){
  plot(2024:2040, proj_vials[i,1:length(yrs)], lty = 2, type = "l", ylim = c(0, 120000), 
       ylab = "Numbers", xlab = "", main = proj_vials$country[i]) # HDI of countries
  # lines(2024:2033, vials_SQ[i,1:10])
  lines(2024:2040, proj_rabid[i,1:length(yrs)], col="red", lty = 2)
  lines(2024:2040, proj_bite_patients[i,1:length(yrs)], col="blue")
}
dev.off()

# Plot change in patients presenting & vaccine use under Gavi in 2030 - according to each countries HDI
pdf("figs/proj_demand_2030.pdf")
par(mfrow=c(1,1), mar = c(4,5,1,1))
plot(df$HDI, proj_vials$'2030'*100000/df$pop2030, 
     pch=20, ylab = "patients or vials/100k", xlab = "HDI", ylim = c(0, 250))
points(df$HDI, proj_rabid$'2030'*100000/df$pop2030, col="red")
points(df$HDI, proj_bite_patients$'2030'*100000/df$pop2030, col="blue")
dev.off()
