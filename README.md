# Spatial Sampling Framework for Dental Facilities — Riyadh

[![Code license: MIT](https://img.shields.io/badge/code%20license-MIT-blue.svg)](LICENSE)
[![Data license: CC BY 4.0](https://img.shields.io/badge/data%20license-CC%20BY%204.0-lightgrey.svg)](LICENSE-data.txt)
[![DOI](https://img.shields.io/badge/DOI-pending%20Zenodo-orange.svg)](#how-to-cite)

Data and analysis code for the study **“Validating Dental Sampling Frames in Rapidly
Expanding Cities: Riyadh Case”** (JDR Clinical & Translational Research).

This repository accompanies a field-validated census of **private dental facilities in
Riyadh, Saudi Arabia**. It provides the calibration, validation, and quality-control
datasets from the field census; a de-identified facility-level dataset; district-level
allocation and street-sampling data; the city district basemap; and the R pipeline used
to produce the study’s results, figures, and appendix tables.

---

## Overview

Administrative licensing registries substantially undercount private dental facilities in
rapidly expanding cities, and are not designed to serve as research sampling frames. This
project builds and validates a spatially explicit sampling frame for private dental
facilities in Riyadh through a citywide field census, a calibration/validation exercise,
and a coverage/misclassification correction, and maps the resulting spatial distribution.

Outputs that reproduce from the data in this repository include the facility enumeration
(732 private facilities providing dental services among 876 private health facilities
surveyed), the calibration/validation/quality-control metrics, the spatial join to city
districts, facility counts by district and region, and the kernel-density maps. See the
[Reproducibility](#reproducibility) section for the one class of results that requires
restricted data.

- **Manuscript:** Alshihayb TS, Aldossri M. *Validating Dental Sampling Frames in Rapidly
  Expanding Cities: Riyadh Case.* JDR Clinical & Translational Research (under review).
- **Setting:** Riyadh, Saudi Arabia · **Unit:** private (and Ministry of Health) dental
  facilities · **Basemap:** Riyadh administrative districts.

---

## Repository contents

| File | What it is |
|---|---|
| `Analysis_shared.R` | The full R analysis pipeline (calibration → validation → quality control → spatial join → counts, maps, and regional summaries). Documented, portable, and runnable from the repository root. |
| `sampling_streets_per_round.sas` | SAS script that drew the stratified random street sample used to plan each field round (10% SRS stratified by district, fixed seed). |
| `Facility_data_stripped.xlsx` | De-identified facility-level dataset (one row per enumerated facility). |
| `District_population_street_data_stripped.xlsx` | District list, region assignment, team/round allocation, and the sampled/de-duplicated street lists. **District population has been removed** (see below). |
| `Calibration_data.xlsx` | Calibration counts per field-team group per district (inter-rater calibration). |
| `Validation_data.xlsx` | Independent validation subsample re-checked against Google Maps. |
| `Quality_control_data.xlsx` | Quality-control re-survey comparing facilities found by the QC team vs. the original collectors. |
| `districts.geojson` | Riyadh administrative district polygons (basemap for the spatial join and maps). |
| `DATA_DICTIONARY.md` | Variable-level documentation for every file and sheet. |
| `CITATION.cff` | Machine-readable citation metadata. |
| `LICENSE` / `LICENSE-data.txt` | Code (MIT) and data (CC BY 4.0) licenses. |

---

## Data

- **Formats:** open where practical — `.geojson` (RFC 7946) for geometry, `.R`/`.sas` for
  code. Tabular data are provided as `.xlsx`; column-level documentation is in
  [`DATA_DICTIONARY.md`](DATA_DICTIONARY.md).
- **De-identification:** the facility file is stripped of directly identifying free-text
  where not needed for analysis; coordinates are retained because facility locations are
  the object of study and are publicly observable.

### ⚠️ Restricted data notice (district population)

Under the data-governance rules of the Saudi **General Authority for Statistics (GASTAT)**,
**district/neighbourhood-level population counts cannot be shared publicly** — only
region/governorate totals may be published. District population is therefore **removed**
from `District_population_street_data_stripped.xlsx`.

Consequently:

- ✅ **Reproduce from the public data:** facility enumeration, calibration, validation,
  quality control, the spatial join, facility **counts** by district and region, and the
  kernel-density maps.
- 🔒 **Require the restricted district-population file** (available from the authors on
  reasonable request): the **population-standardised** results — facilities per 10,000
  residents, Figure 3, Appendix Figure 5 panel B, and the per-10,000 regional means.

When `Analysis_shared.R` is run on the public data it completes every step above and then
stops with an explanatory message at the population-standardisation step.

---

## Reproducibility

**Requirements**

- R ≥ 4.3 (developed on R 4.5.2).
- R packages: `tidyverse`, `readxl`, `janitor`, `labelled`, `irr`, `flextable`,
  `gtsummary`, `officer`, `scales`, `sf`, `ggrepel`, `RColorBrewer`, `viridis`, `ragg`,
  `patchwork`, `ggnewscale`, `ggtext`.
  ```r
  install.packages(c("tidyverse","readxl","janitor","labelled","irr","flextable",
                     "gtsummary","officer","scales","sf","ggrepel","RColorBrewer",
                     "viridis","ragg","patchwork","ggnewscale","ggtext"))
  ```

**Run**

1. Download or clone the repository.
2. Open the folder in R/RStudio so the working directory is the repository root (the data
   files are read by relative name; no absolute path is hard-coded).
3. Run `Analysis_shared.R` top to bottom.

**Notes**

- The street sample was drawn with a fixed seed (`set.seed(123)` in R; `seed=100` in SAS),
  so the sampling steps are deterministic.
- The single administrative region variable used throughout is **`new_region`**, the
  official Riyadh Municipality five-region classification (North, East, Center, West,
  South).

---

## How to cite

If you use these data or code, please cite both the software/dataset and the article.
Citation metadata is in [`CITATION.cff`](CITATION.cff); GitHub renders a “Cite this
repository” button from it.

> Alshihayb TS, Aldossri M. *Spatial Sampling Framework for Dental Facilities — Riyadh*
> [data and code]. 2026. https://github.com/tshihayb/Spatial-Sampling-Framework-for-Dental-Facilities

> Alshihayb TS, Aldossri M. *Validating Dental Sampling Frames in Rapidly Expanding
> Cities: Riyadh Case.* JDR Clinical & Translational Research. 2026.

A persistent DOI will be minted by archiving a tagged release to **Zenodo**; the DOI badge
above will link to it once available.

---

## License

- **Code** (`.R`, `.sas`): [MIT](LICENSE).
- **Data** (`.xlsx`, `.geojson`): [Creative Commons Attribution 4.0 International
  (CC BY 4.0)](LICENSE-data.txt).

You may reuse either, including commercially, provided you give appropriate credit.

---

## Data sources & acknowledgements

- Facility locations: original field census by the study team.
- District basemap and boundaries: Riyadh administrative districts.
- Population (region-level, in the manuscript): Saudi General Authority for Statistics
  (GASTAT), 2022 Census. District-level population is governed by GASTAT and not shared here.
- Region-level resident income (in the manuscript): derived from resident-income platforms
  as described in the paper.

---

## Contact

**Talal S. Alshihayb** — corresponding author. Please open a
[GitHub issue](https://github.com/tshihayb/Spatial-Sampling-Framework-for-Dental-Facilities/issues)
for questions about the code or data, or contact the corresponding author for access to
restricted data.
