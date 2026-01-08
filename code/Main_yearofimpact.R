library(data.table)
library(readr)
library(doParallel)
library(prime)

setDTthreads(threads = parallel::detectCores())
batch_data <- read_csv("data/cov6jan_final.csv")
batch_data <- as.data.table(batch_data)

batch_run_obs <- batch_data[, .(
  country_code,
  year,
  age_first,
  age_last,
  coverage
)]

batch_run_obs[, `:=`(
  year      = as.integer(year),
  age_first = as.integer(age_first),
  age_last  = as.integer(age_last),
  coverage  = as.numeric(coverage)
)]

# ----------------------------
# Remove countries with missing data (check later)
# ----------------------------
countries <- unique(batch_run_obs$country_code)

exclude <- c("AND","ATG","KNA","NIU","PLW","SMR","COK","SYC")

countries_clean <- setdiff(countries, exclude)

# ----------------------------
# 1. Real world scenario run
# ----------------------------
cat("Running OBSERVED rollout scenario...\n")

batch_run_obs_clean <- batch_run_obs[!country_code %in% exclude]

RegisterBatchData(batch_run_obs_clean, force = TRUE)
.data.batch[, vaccine := "4vHPV"]

cl <- makeCluster(detectCores())
registerDoParallel(cl)

results_observed <- BatchRun(
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
  by_calendaryear                 = TRUE,   # change to TRUE for result visualisation - year of impact
  use_proportions                 = TRUE,
  analyseCosts                    = FALSE,
  psa                             = 0,
  psa_vals                        = ".data.batch.psa",
  unwpp_mortality                 = TRUE,
  disability.weights              = "gbd_2017",
  canc.inc                        = "2020",
  vaccine                         = "4vHPV"
)

stopCluster(cl)

dir.create("results", showWarnings = FALSE)

run_date <- format(Sys.Date(), "%Y-%m-%d")

out_file <- paste0(
  "results/observed_bycalendaryear_",
  run_date,
  ".csv"
)

fwrite(results_observed, out_file)


# ----------------------------
# 2. What if scenario run - 90% coverage 2006-2024
# ----------------------------
cat("Running WHAT-IF scenario (2006–2024, age 14, 90%)...\n")

# initial parameters
start_year <- 2006
end_year   <- 2024
vacc_age   <- 14
coverage   <- 0.90


batch_run_whatif <- CJ(
  country_code = countries_clean,
  year         = start_year:end_year,
  age_first    = vacc_age,
  age_last     = vacc_age
)

batch_run_whatif[, coverage := coverage]

batch_run_whatif[, `:=`(
  year      = as.integer(year),
  age_first = as.integer(age_first),
  age_last  = as.integer(age_last),
  coverage  = as.numeric(coverage)
)]

RegisterBatchData(batch_run_whatif, force = TRUE)
.data.batch[, vaccine := "4vHPV"]

cl <- makeCluster(detectCores())
registerDoParallel(cl)

results_whatif <- BatchRun(
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
  by_calendaryear                 = TRUE,   # change to true for result visualisation
  use_proportions                 = TRUE,
  analyseCosts                    = FALSE,
  psa                             = 0,
  psa_vals                        = ".data.batch.psa",
  unwpp_mortality                 = TRUE,
  disability.weights              = "gbd_2017",
  canc.inc                        = "2020",
  vaccine                         = "4vHPV"
)

stopCluster(cl)

dir.create("results", showWarnings = FALSE)

run_datetime <- format(Sys.time(), "%Y-%m-%d")

out_file_whatif <- paste0(
  "results/whatif_bycalendaryear_",
  run_datetime,
  ".csv"
)

fwrite(results_whatif, out_file_whatif)


