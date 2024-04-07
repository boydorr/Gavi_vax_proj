# Sense checks for Gavi human rabies vaccine estimates 

##' ** 1. Import projected country population and associated data - project_bites.R**
##' Data compiled for Gavi VIS (prepped_data_final.csv): 
##' - world bank human population projections (2018 onwards)
##' - Human Dog Ratios (exposure risk)
##' - Urban/ rural populations (relates to dog ownership & PEP access)
##' - Human development index, HDI (relates to PEP access)
##' - Dog vaccination coverage (exposure risk)
##' 
##' ** 2. Use functions to project bites (rabid & healthy & PEP) - R/HelperFun.R**
##' - incorporate vial sharing calculations - free, decentralize, vial size 
##' - revised HDR to consider religion (% christian)
##' - project over time horizon for 2 scenarios: SQ v free PEP with improved decentralization
##' 
##' ** REPORT KEY ASSUMPTIONS **
##' % increase PEP seeking with Gavi support & saturation
##'   - free PEP increases demand (saturating curve - but may increase further in Asian settings)
##'   - decentralization increases access (exponential curve - starts lower in SSA based on HDI)
##' Regimen = IPC ID
##' Vial size = 1 ml - but little change with 0.5mL vials, though increased resilience in settings prone to surges
##' Full compliance
##' IMPORTANT SENSITIVITIES:
##' Human dog ratio is more variable than country estimates
##' Expect +/- 20% per annum based on bite variability in data
##' Assumptions on health seeking changes with improved access likely to differ considerably
##' 
