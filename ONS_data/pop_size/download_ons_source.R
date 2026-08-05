## download_ons_source.R
##
## One-time script to download the ONS 2022-based local-authority population
## projection source CSV and save it into the repository alongside the legacy
## 2016-based files.
##
## Run ONCE from the repository root (requires internet access):
##   Rscript ONS_data/pop_size/download_ons_source.R
##
## After running, commit the downloaded file:
##   git add "ONS_data/pop_size/2022 LAPP Population.csv"
##   git commit -m "Add ONS 2022-based LAPP source CSV (10-year migration variant)"
##
## Source:
##   ONS "Population projections for local authorities by single year of age and
##   sex, England" — 2022-based, 10-year migration variant.
##   Landing page:
##   https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/
##   populationprojections/datasets/localauthoritiesinenglandz1
##   Release date: 24 June 2025.

ONS_LAPP_URL <- paste0(
  "https://www.ons.gov.uk/generator?format=csv",
  "&uri=/peoplepopulationandcommunity/populationandmigration",
  "/populationprojections/datasets/localauthoritiesinenglandz1",
  "/2022basedmigrationcategoryvariant"
)

dest <- "./ONS_data/pop_size/2022 LAPP Population.csv"

if (file.exists(dest)) {
  message("File already exists: ", dest)
  message("Delete it first if you want to re-download.")
  quit(status = 0L)
}

message("Downloading ONS 2022-based LAPP (10-year migration variant) ...")
message("URL: ", ONS_LAPP_URL)

tryCatch(
  download.file(ONS_LAPP_URL, destfile = dest, mode = "wb", quiet = FALSE),
  error = function(e) {
    file.remove(dest)
    stop("Download failed: ", conditionMessage(e),
         "\nCheck your internet connection and that the ONS URL is still valid.\n",
         "If the URL has changed, update ONS_LAPP_URL in this script and in ",
         "ONS_data/pop_size/transform_pops.R and ONS_data/pop_size/README.md.")
  }
)

## Basic sanity check on the downloaded file
lines <- readLines(dest, n = 3L, warn = FALSE)
if (!any(grepl("AREA_CODE|area_code", lines, ignore.case = TRUE))) {
  file.remove(dest)
  stop(
    "Downloaded file does not look like the expected ONS CSV (no AREA_CODE header).\n",
    "Inspect the URL manually: ", ONS_LAPP_URL
  )
}

sz <- file.size(dest)
message(sprintf("Downloaded: %s  (%.1f MB)", dest, sz / 1024 / 1024))
message("")
message("Next steps:")
message('  git add "ONS_data/pop_size/2022 LAPP Population.csv"')
message('  git commit -m "Add ONS 2022-based LAPP source CSV (10-year migration variant)"')
message("Then run transform_pops.R to regenerate pop_proj.fst.")
