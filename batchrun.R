library(prime)
library(data.table)
library(dplyr)
library(tidyr)
library(doParallel)
library(foreach)
library(readxl)

# load coverage dataset
# hpv_cov_prime <- read_excel("data/hpv_coverage.xlsx")

batch_cohort_vaccination <- function(hpv_cov_prime) {
  
  batch <- hpv_cov_prime %>%
    dplyr::select(
      country_code,
      year = year_vac,
      age_first = agevac,
      age_last  = agevac,
      coverage
    ) 
  
  data.table::as.data.table(batch)
}

estimate_vaccine_impact <- function(vaccine, vaccination_age,canc.inc="2020") {
  
  results <- BatchRun(
    countries                       = -1,
    coverage                        = -1,
    agevac                          = -1,
    agecohort                       = -1,
    sens                            = -1,
    year_born                       = -1,
    year_vac                        = -1,
    runs                            = 1,
    vaccine_efficacy_beforesexdebut = 1,
    vaccine_efficacy_aftersexdebut  = 0,
    log                             = -1,
    by_calendaryear                 = FALSE,
    use_proportions                 = TRUE,
    analyseCosts                    = FALSE,
    psa                             = 0,
    psa_vals                        = ".data.batch.psa",
    unwpp_mortality                 = TRUE,
    disability.weights              = "gbd_2017",
    canc.inc                        = canc.inc,
    vaccine                         = vaccine
  )
  
  outfile <- paste0("../output/results_vac14_", vaccine, ".csv")
  fwrite(results, outfile)
  
  return(outfile)
}

# Create PRIME batch input
batch_cohorts <- batch_cohort_vaccination(hpv_cov_prime)

# Register batch with PRIME
RegisterBatchData(batch_cohorts, force = TRUE)

outfile <- estimate_vaccine_impact(vaccine = "2vHPV", vaccination_age = 14)

