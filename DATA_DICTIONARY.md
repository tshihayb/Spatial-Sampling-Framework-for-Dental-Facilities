# Data dictionary

Variable-level documentation for every data file in this repository. Column names are
listed as they appear in the files; the R pipeline standardises them with
`janitor::clean_names()` (spaces → underscores, lower case) on import.

> **Note on the region variable.** The study’s single administrative classification is
> **`new_region`** — the official Riyadh Municipality five-region scheme
> (North, East, Center, West, South). An earlier, unofficial grouping (`Region`) survives
> in one sheet (`Distributed districts clean`) and is **superseded**; use `new_region`.

> **Restricted field.** District/neighbourhood **population** is withheld under Saudi
> GASTAT census governance and is therefore **absent** from the public files. See the
> README “Restricted data notice.”

---

## `Facility_data_stripped.xlsx` — sheet `Sheet1`
One row per enumerated facility (n ≈ 910 records; 732 provide dental services).

| Column | Description |
|---|---|
| `id` | Unique facility identifier. |
| `round` | Field-survey round in which the facility was recorded. |
| `group` | Field-team group (A–G) that surveyed the facility. |
| `district_in_english` | District (neighbourhood) name, English. |
| `new_region` | Official Riyadh Municipality region (North/East/Center/West/South). |
| `does_the_clinic_pin_show_on_google_maps` | Whether the facility appears as a pin on Google Maps (Yes/No). |
| `type_of_clinic` | Facility/clinic type. |
| `street` | Street where the facility is located. |
| `longitudes` | Longitude (decimal degrees). |
| `latitudes` | Latitude (decimal degrees). |
| `clinic_name_in_english_from_sign_status` | Facility name from physical signage, English (status/value). |
| `clinic_name_in_arabic_from_sign_status` | Facility name from physical signage, Arabic. |
| `clinic_name_in_english_from_google_maps_status` | Facility name from Google Maps, English. |
| `clinic_name_in_arabic_from_google_maps_status` | Facility name from Google Maps, Arabic. |
| `phone_number_from_receptionist_status` | Phone obtained from the receptionist (presence/status). |
| `phone_number_from_google_maps_status` | Phone listed on Google Maps (presence/status). |
| `email_from_receptionist_status` | Email obtained from the receptionist (presence/status). |
| `email_from_google_maps_status` | Email listed on Google Maps (presence/status). |
| `website_from_receptionist_status` | Website obtained from the receptionist (presence/status). |
| `website_from_google_maps_status` | Website listed on Google Maps (presence/status). |
| `with_dental_services` | Whether the facility provides dental services (Yes/No). |
| `type_with_dental` | Facility sub-type among those providing dental services. |
| `type_without_dental` | Facility sub-type among those not providing dental services. |
| `private_or_public` | Sector (private / public). |

## `District_population_street_data_stripped.xlsx`
District registry, region/team/round allocation, and street sampling. **Population removed.**

**Sheet `New Riyadh Regions`** — district registry.
| Column | Description |
|---|---|
| `Number` | District number. |
| `District Name in English` | District name, English. |
| `New Region` | Official Riyadh Municipality region. |
| `Done` | Whether the district was surveyed (Yes/No). |
| `Need to be checked` | QC flag. |
| `Empty based on google map` | Flag: district appears to contain no facilities on Google Maps. |
| `Comments` | Free text. |
| *(population)* | **Withheld** under GASTAT governance — not present in the public file. |

**Sheet `List`** — district registry with alternate English name key.
| Column | Description |
|---|---|
| `Number`, `District Name in English`, `New Region`, `Done (Yes or No)`, `Need to be checked (Yes or No)`, `Empty based on google map (Yes or No)`, `Comments` | As above. |
| `name_en` | Normalised English district name used to join to the basemap. |

**Sheet `Distributed districts clean`** — team allocation of districts.
| Column | Description |
|---|---|
| `ID` | District identifier. |
| `District in English` | District name, English. |
| `Region` | **Superseded, unofficial grouping — do not use; use `new_region`.** |
| `Group A` … `Group G` | Field-team group allocation. |
| `Round` | Field round. |

**Sheet `Random selection of streets`** — sampled streets per round.
| Column | Description |
|---|---|
| `ID` | Street record identifier. |
| `District Number` | District number (stratum). |
| `District in Arabic` / `District in English` | District name. |
| `Street` | Street name. |
| `Sampled for quality check` | Whether the street was drawn for QC. |
| `Round` | Field round. |
| `Comments` | Free text. |

**Sheet `Clean deduplicated streets`** — de-duplicated street frame.
| Column | Description |
|---|---|
| `ID`, `District Number`, `District in English`, `Street`, `Sampled for quality check`, `Round`, `Comments` | As above. |
| `Groups that went to it for quality` | QC team group(s) that visited the street. |
| `New_region` | Official Riyadh Municipality region. |

## `Calibration_data.xlsx`
Inter-rater calibration: facility counts per team group per district.

**Sheet `Sheet 1`** (used by the pipeline)
| Column | Description |
|---|---|
| `district_in_english` | District name, English. |
| `Health facility` | Reference count of health facilities in the district. |
| `Group A` … `Group G` | Facilities counted by each field-team group (calibration). |

**Sheet `Sheet 2`** — same layout with `District` as the key (Arabic/alternate).

## `Validation_data.xlsx` — sheet `Sheet1`
Independent validation subsample re-checked against Google Maps.
| Column | Description |
|---|---|
| `id` | Validation record identifier. |
| `latitude_from_google_maps` / `longitude_from_google_maps` | Coordinates from Google Maps. |
| `type_of_facility` | Facility type. |
| `has_dental_clinic` | Whether the facility has a dental clinic (Yes/No). |
| `open_or_closed` | Operating status at validation. |
| `comments` | Free text. |
| `facility_group` | Group/category used in the validation analysis. |

## `Quality_control_data.xlsx` — sheet `Sheet1`
Quality-control re-survey: facilities found by the QC team vs. the original collectors.
| Column | Description |
|---|---|
| `Facility found number (different than unique id in other data)` | QC record number (not the `id` used elsewhere). |
| `district_in_english` | District name, English. |
| `new_region` | Official Riyadh Municipality region. |
| `street` | Street name. |
| `type_of_clinic` | Facility type. |
| `group` | Field-team group. |
| `round` | Field round. |
| `Found by quality` | Facility found by the QC team (Yes/No). |
| `Found by original data collectors` | Facility found by the original collectors (Yes/No). |
| `Found by both` | Facility found by both (Yes/No). |

## `districts.geojson`
Riyadh administrative district polygons (basemap). Feature geometry plus district name
attributes used to join facility points to districts and to draw the maps. CRS: WGS 84
(EPSG:4326), per the GeoJSON standard.

---

*Descriptions of `_status`/flag fields reflect the field-collection protocol; the
corresponding author can confirm any coding not evident from the values.*
