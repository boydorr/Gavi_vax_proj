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


# Back-of-the-envelope function to calculate bite incidence & PEP demand, split by rabid & healthy
# Gavi VIS - vaccine_paid_by_patient (TRUE/ FALSE) or vaccine_free_SQ (TRUE/FALSE)
CalcPEP <- function(code, year, hdr, BI = 0.01, HDI, 
                     free = FALSE, vials_low = 3, vials_high = 1.5){
  
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




