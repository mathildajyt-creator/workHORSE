## workHORSE is an implementation of the IMPACTncd framework, developed by Chris
## Kypridemos with contributions from Peter Crowther (Melandra Ltd), Maria
## Guzman-Castillo, Amandine Robert, and Piotr Bandosz. This work has been
## funded by NIHR  HTA Project: 16/165/01 - workHORSE: Health Outcomes
## Research Simulation Environment.  The views expressed are those of the
## authors and not necessarily those of the NHS, the NIHR or the Department of
## Health.
##
## Copyright (C) 2018-2020 University of Liverpool, Chris Kypridemos
##
## workHORSE is free software; you can redistribute it and/or modify it under
## the terms of the GNU General Public License as published by the Free Software
## Foundation; either version 3 of the License, or (at your option) any later
## version. This program is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
## FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
## details. You should have received a copy of the GNU General Public License
## along with this program; if not, see <http://www.gnu.org/licenses/> or write
## to the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
## Boston, MA 02110-1301 USA.

## ── Population projection data import ──────────────────────────────────────────
##
## Updated 2025: Replaced ONS 2016-based SNPP (through 2041) with the ONS
## 2022-based local-authority population projections, 10-year migration variant
## (through 2047).
##
## Source: ONS "Population projections for local authorities by single year of
##   age and sex, England" — 2022-based, 10-year migration variant.
##   Landing page:
##   https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/
##   populationprojections/datasets/localauthoritiesinenglandz1
##   Release date: 24 June 2025.
##   Download timestamp: see ONS_data/pop_size/README.md.
##
## Usage:
##   By default the script downloads the source file from ONS at runtime.
##   To use a pre-downloaded file set the environment variable:
##     ONS_LAPP_CSV=/path/to/downloaded.csv
##   or pass its path as the first command-line argument:
##     Rscript transform_pops.R /path/to/downloaded.csv
##
## Output schema (unchanged): year, age, sex, LAD17CD, LAD17NM, pops
##   - sex ∈ {"men", "women"}
##   - age: integer 0-89 (90+ group capped at 89 to match model ageH limit)
##   - year: integer calendar year

library(data.table)
library(fst)

## ── 1. Locate / download the ONS 2022-based SNPP source file ──────────────────

## ONS 2022-based local-authority projections, 10-year migration variant (Z1)
ONS_LAPP_URL <- paste0(
  "https://www.ons.gov.uk/generator?format=csv&uri=/peoplepopulationandcommunity",
  "/populationandmigration/populationprojections/datasets/",
  "localauthoritiesinenglandz1/2022basedmigrationcategoryvariant"
)

## Determine source file path: CLI arg > env var > download
args <- commandArgs(trailingOnly = TRUE)
if (length(args) >= 1 && nzchar(args[1])) {
  src_file <- args[1]
} else if (nzchar(Sys.getenv("ONS_LAPP_CSV"))) {
  src_file <- Sys.getenv("ONS_LAPP_CSV")
} else {
  src_file <- tempfile(fileext = ".csv")
  message("Downloading ONS 2022-based SNPP (10-year migration variant) ...")
  download.file(ONS_LAPP_URL, destfile = src_file, mode = "wb", quiet = FALSE)
  message("Download complete: ", src_file)
}

## ── 2. Parse the 2022-based ONS file ──────────────────────────────────────────
## Expected columns: AREA_CODE, AREA_NAME, SEX, AGE_GROUP, <year columns>
## SEX values in source: "Male" / "Female" (or "males"/"females"; normalised below)

raw <- fread(src_file, header = TRUE, skip = "AREA_CODE")
setnames(raw, tolower(names(raw)))

## Normalise sex labels
raw[, sex := fcase(
  tolower(sex) %in% c("male", "males", "m"),   "men",
  tolower(sex) %in% c("female", "females", "f"), "women"
)]
raw <- raw[!is.na(sex)]

## Handle 90-and-over group: assign to age 89 to stay within model ageH limit
raw[age_group == "90 and over", age_group := "89"]
raw[, age := suppressWarnings(as.integer(age_group))]
raw <- raw[!is.na(age)]  # drops "All ages" / total rows

## Identify year columns (numeric names)
year_cols <- grep("^[0-9]{4}$", names(raw), value = TRUE)
if (length(year_cols) == 0L)
  stop("No year columns found in ONS source file. Check column names.")

## Pivot to long format
pops22 <- melt(
  raw[, c("area_code", "area_name", "age", "sex", year_cols), with = FALSE],
  id.vars      = c("area_code", "area_name", "age", "sex"),
  measure.vars = year_cols,
  variable.name = "year",
  value.name    = "pops",
  variable.factor = FALSE
)

setnames(pops22, c("area_code", "area_name"), c("LAD17CD", "LAD17NM"))
pops22[, `:=`(
  LAD17CD = factor(LAD17CD),
  LAD17NM = factor(LAD17NM),
  sex     = factor(sex),
  year    = as.integer(year),
  age     = as.integer(age),
  pops    = as.numeric(pops)
)]

## Aggregate the 90+ group that was reassigned to age 89
pops22 <- pops22[, .(pops = sum(pops, na.rm = TRUE)),
                 keyby = .(year, age, sex, LAD17CD, LAD17NM)]

## Keep only projection years that are within the supported horizon
pops22 <- pops22[year > 2017L & year <= 2047L]

## Warn about any LAD codes not present in the locality index
localities_indx <- read_fst("./synthpop/lsoa_to_locality_indx.fst",
                            as.data.table = TRUE)
lads_in_idx <- unique(localities_indx$LAD17CD)
lads_in_proj <- unique(as.character(pops22$LAD17CD))
missing_lads  <- setdiff(lads_in_proj, lads_in_idx)
dropped_lads  <- setdiff(lads_in_idx,  lads_in_proj)
if (length(missing_lads) > 0L)
  message("LADs in 2022-based projection but not in locality index (not used): ",
          paste(missing_lads, collapse = ", "))
if (length(dropped_lads) > 0L)
  warning("LADs in locality index but not in 2022-based projection (no weights): ",
          paste(dropped_lads, collapse = ", "))

## ── 3. Build historical data (2003–2017) from LSOA estimates ──────────────────
## (unchanged from original transform_pops.R)
dt <- read_fst("./synthpop/lsoa_mid_year_population_estimates.fst",
               as.data.table = TRUE)
dt[localities_indx, on = "LSOA11CD", `:=`(LAD17CD = i.LAD17CD, LAD17NM = i.LAD17NM)]
dt[, c("LSOA11CD", "LAD11CD", "LAD11NM") := NULL]
dt <- melt(
  dt,
  grep("^[0-9]", names(dt), value = TRUE, invert = TRUE),
  variable.name   = "age",
  value.name      = "pops",
  variable.factor = FALSE
)[year > 2002L]
dt <- dt[, .(pops = sum(pops)), keyby = .(year, age, sex, LAD17CD, LAD17NM)]
dt[, `:=`(
  LAD17CD = factor(LAD17CD),
  LAD17NM = factor(LAD17NM),
  sex     = factor(sex),
  year    = as.integer(as.character(year)),
  age     = as.integer(as.character(age))
)]

## ── 4. Combine and write ──────────────────────────────────────────────────────
pops <- rbind(pops22, dt)
setkey(pops, year, age, sex, LAD17CD)

write_fst(pops, "./ONS_data/pop_size/pop_proj.fst", 100)
message("Written: ONS_data/pop_size/pop_proj.fst  (years ",
        min(pops$year), "–", max(pops$year), ")")
