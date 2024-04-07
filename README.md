# Sense checks for Gavi human rabies vaccine projections

1. Import projected country population and associated data - project_bites.R

   Data compiled for Gavi VIS (prepped_data_final.csv):
   - world bank human population projections (2018 onwards)
   - Human Dog Ratios (exposure risk)
   - Urban/ rural populations (relates to dog ownership & PEP access)
   - Human development index, HDI (relates to PEP access)
   - Dog vaccination coverage (exposure risk)
  Revised HDR to consider religion (% muslim data sourced)

2. Use functions to project bites (rabid & healthy & PEP) - R/HelperFun.R
  
    Incorporate vial sharing calculations: free; decentralized & vial size 

3. Project over time horizon for 2 scenarios:
   - SQ vs free PEP with improved access (decentralization) using IPC regimen

# Key assumptions & concerns
   
   % increase PEP seeking with Gavi support & saturation   
   - free PEP increases demand (saturating curve - but may increase further in Asian settings
   - decentralization increases access (exponential curve - starts lower in SSA based on HDI)

Vial size = 1 ml - but little change with 0.5mL vials, though increased resilience in settings prone to surges
Full compliance

# Identified sensitivities

Human dog ratio is more variable than country estimates
Expect +/- 20% per annum based on bite variability in data
Assumptions on health seeking changes with improved access likely to differ considerably
