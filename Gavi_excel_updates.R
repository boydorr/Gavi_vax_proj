# Gavi back calculations

# Tanzania
pep <- 30000
urban <- 0.4
buffer <- 0.2
dropout <- 0.3
complete_urban <- 0.67
complete_rural <- 2.2
incomplete_urban <- 0.45
incomplete_rural <- 1.47

vpp_SQ <- 1 + 1*(1-dropout) + 1*(1-dropout)*(1-dropout) # vials per patient SQ
vpp_SQ_ID <- 0.4 + 0.4*(1-dropout) + 0.4*(1-dropout)*(1-dropout) # vials per patient SQ

bites <- pep/vpp_SQ
  
(bites * urban * dropout * incomplete_urban +
bites * (1-urban) * dropout * incomplete_rural +
bites * urban * (1-dropout) * complete_urban +
bites * (1-urban) * (1-dropout) * complete_rural)*1.5
# 23500

bites <- pep/vpp_SQ_ID

(bites * urban * dropout * incomplete_urban +
    bites * (1-urban) * dropout * incomplete_rural +
    bites * urban * (1-dropout) * complete_urban +
    bites * (1-urban) * (1-dropout) * complete_rural)*1.2
# 60k

# Assuming PEP population
(pep * urban * dropout * incomplete_urban +
    pep * (1-urban) * dropout * incomplete_rural +
    pep * urban * (1-dropout) * complete_urban +
    pep * (1-urban) * (1-dropout) * complete_rural)* 1.5

# Assuming PEP population
((pep * urban * dropout * incomplete_urban) +
    (pep * (1-urban) * dropout * incomplete_rural) +
    (pep * urban * (1-dropout) * complete_urban) +
    (pep * (1-urban) * (1-dropout) * complete_rural))* 1.5

((pep * urban * complete_urban) + (pep * (1-urban) * complete_rural)) * 2.19/3 * 1.5


# Mael's method
doses = pep +
  (pep*(1-dropout)) + 
  (pep*(1-dropout)*(1-dropout))

WF_urban = 0.67/0.6
WF_rural = 2.2/0.6
dose_vol = 0.2
buffer = 1.5

((doses * (1-urban) * WF_rural * dose_vol) + 
    (doses * urban * WF_urban * dose_vol)) * buffer

doses_calc <- function(pep_pop, DOR){ # pop population & drop out rate
  pep_pop + (pep_pop*(1-DOR)) + (pep_pop*(1-DOR)*(1-DOR))
}
doses_calc(30000, 0.3)
doses_calc(30000, 0.2)
doses_calc(30000, 0.1)


(pep * urban * dropout * incomplete_urban) +
(pep * urban * (1-dropout) * complete_urban) +
(pep * (1-urban) * dropout * incomplete_rural) +
(pep * (1-urban) * (1-dropout) * complete_rural)
* 1.5




# I wonder, if you are getting higher values because you are starting off using the whole vials? Rather than translating intro fractional doses? 
#   I realise my calculation above combines the boxes of: (no of doses) & (Vaccine waste) together (from your slide 7) whereas I think you process these multiplicatively?
#   If I have understood correctly, this might end up reducing the total vial volume by >50%.





# Madagascar
pep <- 8900 # 85/100,000
vpp_SQ_ID <- 0.4 + 0.4*(1-dropout) + 0.4*(1-dropout)*(1-dropout) # vials per patient SQ

bites <- pep/vpp_SQ_ID

(bites * urban * dropout * incomplete_urban +
    bites * (1-urban) * dropout * incomplete_rural +
    bites * urban * (1-dropout) * complete_urban +
    bites * (1-urban) * (1-dropout) * complete_rural)*1.2
# 17500

pep <- 85*30325000/100000 # 85/100,000

bites <- pep/vpp_SQ_ID

(bites * urban * dropout * incomplete_urban +
    bites * (1-urban) * dropout * incomplete_rural +
    bites * urban * (1-dropout) * complete_urban +
    bites * (1-urban) * (1-dropout) * complete_rural)*1.2
# 50,500k


# Cambodia
pep <- 45000 # 85/100,000
vpp_SQ_ID <- 0.4 + 0.4*(1-dropout) + 0.4*(1-dropout)*(1-dropout) # vials per patient SQ

bites <- pep/vpp_SQ_ID

(bites * urban * dropout * incomplete_urban +
    bites * (1-urban) * dropout * incomplete_rural +
    bites * urban * (1-dropout) * complete_urban +
    bites * (1-urban) * (1-dropout) * complete_rural)*1.2
# 88k

