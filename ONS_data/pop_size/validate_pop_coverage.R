## validate_pop_coverage.R
##
## Lightweight coverage-validation script for ONS_data/pop_size/pop_proj.fst.
## Run from the repository root:
##   Rscript ONS_data/pop_size/validate_pop_coverage.R
##
## Exits with status 1 (and prints actionable error messages) if any check fails.

library(data.table)
library(fst)
library(yaml)

errors <- character(0)

## ── 0. Load model configuration ───────────────────────────────────────────────
cfg_path <- "./simulation/sim_design.yaml"
if (!file.exists(cfg_path)) {
  stop("Cannot find simulation/sim_design.yaml; run from repository root.")
}
cfg <- yaml::read_yaml(cfg_path)
init_year    <- as.integer(cfg$init_year_long)
horizon      <- as.integer(cfg$sim_horizon_max)
required_max <- init_year + horizon          # e.g. 2013 + 34 = 2047
age_L        <- as.integer(cfg$ageL)        # e.g. 30
age_H        <- as.integer(cfg$ageH)        # e.g. 89

cat(sprintf(
  "Config: init_year=%d  sim_horizon_max=%d  required_max_year=%d  ages=%d-%d\n",
  init_year, horizon, required_max, age_L, age_H
))

## ── 1. Load pop_proj.fst ──────────────────────────────────────────────────────
fst_path <- "./ONS_data/pop_size/pop_proj.fst"
if (!file.exists(fst_path)) {
  stop("pop_proj.fst not found at ", fst_path,
       "\nRun ONS_data/pop_size/transform_pops.R first to generate it.")
}
pp <- read_fst(fst_path, as.data.table = TRUE)
cat(sprintf("pop_proj.fst loaded: %d rows, years %d–%d\n",
            nrow(pp), min(pp$year), max(pp$year)))

## ── 2. Year coverage ──────────────────────────────────────────────────────────
if (max(pp$year) < required_max) {
  errors <- c(errors, sprintf(
    "FAIL year coverage: max(year)=%d < required %d (init_year + sim_horizon_max)",
    max(pp$year), required_max
  ))
} else {
  cat(sprintf("PASS year coverage: max(year)=%d >= %d\n", max(pp$year), required_max))
}

## ── 3. No missing or negative populations ────────────────────────────────────
n_missing  <- sum(is.na(pp$pops))
n_negative <- sum(pp$pops < 0, na.rm = TRUE)
if (n_missing > 0L) {
  errors <- c(errors, sprintf("FAIL %d NA population values", n_missing))
} else {
  cat("PASS no NA population values\n")
}
if (n_negative > 0L) {
  errors <- c(errors, sprintf("FAIL %d negative population values", n_negative))
} else {
  cat("PASS no negative population values\n")
}

## ── 4. No duplicate key rows ──────────────────────────────────────────────────
dupes <- pp[duplicated(pp, by = c("year", "age", "sex", "LAD17CD"))]
if (nrow(dupes) > 0L) {
  errors <- c(errors, sprintf(
    "FAIL %d duplicated (year, age, sex, LAD17CD) rows", nrow(dupes)
  ))
} else {
  cat("PASS no duplicate key rows\n")
}

## ── 5. Age coverage within model range ────────────────────────────────────────
proj_years  <- seq(init_year + 1L, required_max)  # projection period (post-historical)
proj_subset <- pp[year %in% proj_years]
ages_present <- sort(unique(proj_subset$age))
missing_ages <- setdiff(age_L:age_H, ages_present)
if (length(missing_ages) > 0L) {
  errors <- c(errors, sprintf(
    "FAIL missing ages %s in projection period",
    paste(range(missing_ages), collapse = "-")
  ))
} else {
  cat(sprintf("PASS all ages %d-%d present in projection period\n", age_L, age_H))
}

## ── 6. LAD coverage: all locality-index LADs in pop_proj ─────────────────────
lsoa_idx_path <- "./synthpop/lsoa_to_locality_indx.fst"
if (file.exists(lsoa_idx_path)) {
  lsoa_idx   <- read_fst(lsoa_idx_path, as.data.table = TRUE)
  lads_needed <- unique(as.character(lsoa_idx$LAD17CD))
  lads_avail  <- unique(as.character(pp$LAD17CD))
  missing_lads <- setdiff(lads_needed, lads_avail)
  if (length(missing_lads) > 0L) {
    errors <- c(errors, sprintf(
      "FAIL %d LAD17CD codes from locality index not in pop_proj.fst: %s",
      length(missing_lads),
      paste(head(missing_lads, 10), collapse = ", ")
    ))
  } else {
    cat(sprintf("PASS all %d locality-index LAD17CD codes present in pop_proj\n",
                length(lads_needed)))
  }
} else {
  message("WARNING: lsoa_to_locality_indx.fst not found; LAD check skipped")
}

## ── 7. Plausibility: England total for required_max year ─────────────────────
## ONS 2022-based projections imply England age 30-89 total ~ 30-35 million in 2047
eng_total <- pp[year == required_max & between(age, age_L, age_H),
                sum(pops, na.rm = TRUE)]
if (eng_total < 1e6) {
  errors <- c(errors, sprintf(
    "FAIL England population sum for year %d, ages %d-%d is implausibly low: %.0f",
    required_max, age_L, age_H, eng_total
  ))
} else {
  cat(sprintf("PASS England total (year=%d, ages %d-%d): %.0f\n",
              required_max, age_L, age_H, eng_total))
}

## ── Summary ───────────────────────────────────────────────────────────────────
if (length(errors) > 0L) {
  cat("\n=== COVERAGE VALIDATION FAILED ===\n")
  cat(paste(errors, collapse = "\n"), "\n")
  quit(status = 1L)
} else {
  cat("\n=== COVERAGE VALIDATION PASSED ===\n")
  quit(status = 0L)
}
