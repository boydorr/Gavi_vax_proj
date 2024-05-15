# Functions for calculating rabies incidence & PEP demand
RabidBites <- function(dogs, BI){ 
  return(dogs * BI * 0.38) # rabid dog bites = incidence (bites per dog)
  } 


# calculate vial use based on bite patients attending high vs low throughout clinics 
# i.e. level of decentralization
VialCalc <- function(bites, HDI, vials_low, vials_high) # could specify if 1 or 0.5ml vials
{
  # Determine how decentralized PEP is based on HDI
  main <- bites * (1-(HDI^(1/2))) 
  outlying <- bites - main
  
  # assign to high/ medium/ low throughput in terms of vials per patient
  main_thru <- main * vials_high # 1.5 vials/ patient if 1mL vials; 2 if 0.5mL vials bc sharing
  out_thru <- outlying * vials_low # 3 vial/ patient if 1mL or 0.5 mL vials, bc v little vial sharing
  
  output <- main_thru + out_thru
  return(output)
}
# vial_calc(5.98e4, 0.466, vials_high = 1.5, vials_low = 3) # 1mL vials = more sharing
# vial_calc(5.98e4, 0.466, vials_high = 2, vials_low = 3) # 0.5 mL vials = less vial sharing

VialCalc2 <- function(bites, HDI, Pcomplete,
                      vials_low_complete, vials_high_complete,
                      vials_low_incomplete, vials_high_incomplete) 
{
  # Determine how decentralized PEP is based on HDI
  main <- bites * (1-(HDI^(1/2))) 
  outlying <- bites - main
  
  # assign to high/ medium/ low throughput in terms of vials per patient
  main_vials <- (main * vials_high_complete * Pcomplete) + (main * vials_high_incomplete * (1-Pcomplete))
  out_vials <- (outlying * vials_low_complete * Pcomplete) + (outlying * vials_low_incomplete * (1-Pcomplete))

  output <- main_vials + out_vials
  return(output)
}

# VialCalc2(200, 0.6, 0,
#           vials_low_complete = 2.2, vials_high_complete = 0.67,
#           vials_low_incomplete = 1.47, vials_high_incomplete = 0.45) 
# VialCalc2(200, 0.6, 0,
#           vials_low_complete = subset(ipc, completeness =="Complete" & Setting == "rural")$vial, 
#           vials_high_complete = subset(ipc, completeness =="Complete" & Setting == "urban")$vial,
#           vials_low_incomplete = subset(ipc, completeness =="Incomplete" & Setting == "rural")$vial, 
#           vials_high_incomplete = subset(ipc, completeness =="Incomplete" & Setting == "urban")$vial)
# 

# Back-of-the-envelope function to calculate bite incidence & PEP demand, split by rabid & healthy
# Gavi VIS - vaccine_paid_by_patient (TRUE/ FALSE) or vaccine_free_SQ (TRUE/FALSE)
CalcPEP <- function(code, year, hdr, BI = 0.01, HDI, 
                     free = FALSE, vials_low = 2.2, vials_high = 0.67){
  
  ind <- which(df$CODE == code)
  pop <- df[ind,year]
  dogs <- pop/hdr
  
  # Adjust bites (rabid & healthy) by HDI 
  rabid_bites <- RabidBites(dogs, BI) * sqrt(1-HDI) # HDI as marker of dog vax
  healthy_bites <- dogs * BI * HDI # HDI as marker of health seeking (inc with higher wealth)

  # Assume free PEP improves health seeking by all bite victims
  # Potential HDI levers: (1-HDI), 1/(1-HDI) vs 1/(1-sqrt(HDI)) vs (1/(1-HDI))^2 # But notice large increase as HDI>>0.5
  if(free == TRUE){
    Pseek_rabid = HDI^(1/2) # saturates towards 1 (--> 1/N twds saturation)
    Pseek_health = HDI^(1/5) 
    # Pseek_health = 1/(1-HDI) # alternative projection scenario (more relevant to high seeking SE Asia countries!)
  } else {
    Pseek_rabid = sqrt(HDI) # below 1:1 HDI
    Pseek_health = sqrt(HDI)
    }
  # Assume decentralized improves health seeking by healthy bite victims
  decentral <- 1/(1-HDI) # exceeds 1 after HDI > 0.5 

  # combine estimates (PEP, bites, vials) 
  bites <- (rabid_bites + healthy_bites)
  PEP <- (rabid_bites * Pseek_rabid) + (healthy_bites * Pseek_health * decentral)
  vials <- VialCalc(bites = PEP, HDI = HDI, vials_low = vials_low, vials_high = vials_high)

  # Set up results dataframe
  res <- data.frame(country = code, HDI = HDI, pop = pop,
                    bites = bites, healthy = healthy_bites, rabid = rabid_bites,
                    PEP = PEP, free = free, vials = vials)
  names(res) <- c("code", "HDI", "pop", "bites", "healthy", "rabid", "PEP", "free", "vials")
  return(res)
}


# function to calculate bite patients
CalcBites <- function(BI = 0.01, pop, hdr, HDI, Pseek_R, Pseek_H){
  dogs <- pop/hdr # estimate dogs

  # Adjust bites (rabid & healthy) by HDI 
  rabid_bites <- RabidBites(dogs, BI) * sqrt(1-HDI) # HDI as marker of dog vax
  healthy_bites <- dogs * BI * HDI # HDI as marker of health seeking (inc with higher wealth)
    
  # Assume decentralized improves health seeking by healthy bite victims
  decentral <- 1/(1-HDI) # exceeds 1 after HDI > 0.5 
  
  # combine estimates (PEP, bites, vials) 
  bites <- (rabid_bites + healthy_bites)
  bite_patients <- (rabid_bites * Pseek_R) + (healthy_bites * Pseek_H * decentral)

  # Set up results dataframe
  res <- list(healthy = healthy_bites, rabid = rabid_bites, patients = bite_patients)
  return(res)
}
# 
# CalcBites(BI = 0.01, pop = df$pop2024[1], hdr = df$hdr_rel[1], HDI = df$HDI[1], 
#                   Pseek_R = Pseek_rabid_base, Pseek_H = Pseek_health_base)
# CalcBites(BI = 0.01, pop = df$pop2024[1], hdr = df$hdr_rel[1], HDI = df$HDI[1], 
#           Pseek_R = 0.7, Pseek_H = 0.8)



# Function to adjust Pseek according to year of Gavi introduction, step + increment function till reach the cap
gavi_fun <- function(ys_gavi, base_p, step, inc, cap_p){
  if(ys_gavi == 0){ p = base_p}
  if(ys_gavi ==1){ p = min(base_p + step, cap_p)}
  if(ys_gavi >1){ p = min(base_p + step + ((ys_gavi-1)*inc), cap_p)}
  return(p)
}
# gavi_fun(15, 0.6, 0.1, 0.03, 0.9)


# plot(seq(0,1,by = 0.01), seq(0,1,by = 0.01))
# lines(seq(0,1,by = 0.01), sqrt(seq(0,1,by = 0.01)))
# lines(seq(0,1,by = 0.01), seq(0,1,by = 0.01)^(1/2))
# lines(seq(0,1,by = 0.01), seq(0,1,by = 0.01)^(1/5))
# lines(seq(0,1,by = 0.01), seq(0,1,by = 0.01)^2)
# 
# 0.5^(1/2)
# sqrt(0.5)

