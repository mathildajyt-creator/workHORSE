# Population Projection Data — `ONS_data/pop_size/`

## Original source (2016-based, through 2041)

The original population projection input was built from:

- `ONS_data/pop_size/2016 SNPP Population males.csv`
- `ONS_data/pop_size/2016 SNPP Population females.csv`

**Source:** Office for National Statistics (ONS), 2016-based Subnational Population
Projections (SNPP), principal projection, English local authorities.  
**Published:** 24 May 2018.  
**Coverage:** mid-2016 through mid-2041, single year of age, by sex and local authority.

These files contain projected resident populations by local authority district (LAD),
single year of age (0–90+), sex, and year.  The transformation script
(`transform_pops.R`) combined them with LSOA-level historical mid-year population
estimates (2003–2017) and wrote `pop_proj.fst`.

---

## Replacement source (2022-based, through 2047)

**Source:** Office for National Statistics (ONS), 2022-based local-authority population
projections, **10-year migration variant**, for local authorities in England, by single
year of age and sex.

**Dataset landing page:**  
<https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationprojections/datasets/localauthoritiesinenglandz1>

**Release date:** 24 June 2025.  
**Download URL (used by `transform_pops.R`):**  
```
https://www.ons.gov.uk/generator?format=csv&uri=/peoplepopulationandcommunity/populationandmigration/populationprojections/datasets/localauthoritiesinenglandz1/2022basedmigrationcategoryvariant
```

**Coverage:** mid-2022 through mid-2047, single year of age (0–90+), by sex and local
authority district, England.  The 10-year migration variant is the nationally consistent
continuation appropriate for this model.

**The 90-and-over group** in the ONS source is assigned to age 89 in `pop_proj.fst`, to
stay within the model's `ageH = 89` upper limit, consistent with the original transform.

**Note:** This extension uses official ONS local-authority data through 2047 and adds
**no post-2047 demographic extrapolation**.  The simulation horizon is set to 2047
accordingly.

---

## Geography notes

The ONS 2022-based source uses current LAD codes.  Where codes differ from the legacy
`LAD17CD` identifiers in `synthpop/lsoa_to_locality_indx.fst`, `transform_pops.R`
emits a warning listing any unmatched codes; it does **not** silently drop them.  Run
`ONS_data/pop_size/validate_pop_coverage.R` to confirm all locality-index LADs are
covered after building `pop_proj.fst`.

---

## Build instructions

Run from the **repository root**:

```r
# Option 1 — download automatically (requires internet access):
Rscript ONS_data/pop_size/transform_pops.R

# Option 2 — use a pre-downloaded file:
Rscript ONS_data/pop_size/transform_pops.R /path/to/ons_lapp_2022.csv

# Option 3 — set an environment variable:
ONS_LAPP_CSV=/path/to/ons_lapp_2022.csv Rscript ONS_data/pop_size/transform_pops.R
```

The script writes `ONS_data/pop_size/pop_proj.fst`.

---

## Coverage validation

```r
Rscript ONS_data/pop_size/validate_pop_coverage.R
```

This script checks:
- `max(year) >= init_year_long + sim_horizon_max` (i.e. ≥ 2047)
- No missing or negative population values
- No duplicated `(year, age, sex, LAD17CD)` rows
- All ages in model range (30–89) covered in the projection period
- All `LAD17CD` codes in the locality index are present in `pop_proj.fst`
- England total for the final year is plausible (> 1 million for ages 30–89)

Exits with status 1 and actionable error messages if any check fails.

---

## Simulation horizon

| Configuration file | Key | Value |
|---|---|---|
| `simulation/sim_design.yaml` | `sim_horizon_max` | 34 (2013 + 34 = 2047) |
| `validation/sim_design_for_trends_validation.yaml` | `sim_horizon_max` | 34 |
| `ui/simulation_parameters_tab.R` | slider max/default | 2047 |
| `ONS_data/project_mortality.R` | `hor` | 37 (covers through 2050) |
