#######################################################################################
# programmer:   Talal Alshihayb
# Date:         December 4, 2025
# Purpose:      Analysis of private dental clinics distribution in Riyadh City
# Last updated: December 4, 2025 
#######################################################################################


###################################################
###################################################
# Section 1: Preparation before reading the dataset
###################################################
###################################################
{
  # 1.1   Cleaning global environment (remove any previously saved objects in environment)
  rm(list = ls())
  
  # 1.2   Setting the working space so objects can be saved in it without referring to it
  # again in saving functions
  # you can change the path below to a location you prefer
  # Try / or \\ or \ if you are using Mac
  setwd("C:/Users/Tshih/OneDrive/Private dental clinics analysis in Riyadh/Manuscript/Submission to JDR")
  
  # 1.3   Checking the working space location
  getwd()
  
  # 1.4   Installing the needed packages
  #required_packages <- c("tidyverse", "janitor", "haven", "labelled", "epitools",
  #"viridis", "lmtest", "performance", "ggfortify", "patchwork",
  #"see", "ggdag", "gtsummary", "rstatix", "skimr",
  #"flextable", "readxl", "stringr", "scales", "RColorBrewer",
  
  # Need for interrupted time series analysis
  #"foreign", "tsModel", "Epi", "splines", "vcd", "tseries", "forecast")
  
  # Install missing packages
  #new_packages <- required_packages[!(required_packages %in% installed.packages()
  #[, "Package"])]
  #if (length(new_packages)) install.packages(new_packages)
  
  # 1.5   Loading packages that we will use
  #lapply(required_packages, library, character.only = TRUE)
  
  # Load needed packages only
  library(tidyverse)
  library(readxl)
  library(janitor)
  library(labelled)
}


########################################
# Section 2: Calibration data of round 1
########################################

# Reading in calibration dataset
calibration <- read_excel("calibration_data.xlsx", sheet = "Sheet 1") %>% 
  clean_names() %>% 
  select(district_in_english, group_a, group_b, group_c, group_d, group_e, group_f, group_g)

Al_yasmin <- calibration %>% filter(district_in_english=="Al Yasmeen") %>% select(-district_in_english) 

Al_narjis <- calibration %>% filter(district_in_english=="Al Narjis") %>% select(-district_in_english)

library(irr)

irr::kripp.alpha(t(as.matrix(Al_yasmin)), method = "nominal")
irr::kripp.alpha(t(as.matrix(Al_narjis)), method = "nominal")


#####################################  
#####################################  
# Section 3: Generate Table 1 summary
#####################################
#####################################

library(flextable)
library(gtsummary)  

# Adding unqique ID
all <- read_excel("Facility_data_stripped.xlsx", sheet = "Sheet1") %>% 
  arrange(round, group) 

# Restrict to Private only
all_priv <- all %>% filter(private_or_public=="Private")

tbl1 <- all_priv %>%
  select(
    with_dental_services, type_with_dental, type_without_dental,
    #type_of_clinic, 
    #region, 
    new_region, clinic_name_in_english_from_sign_status,
    clinic_name_in_arabic_from_sign_status, does_the_clinic_pin_show_on_google_maps,
    clinic_name_in_english_from_google_maps_status,
    clinic_name_in_arabic_from_google_maps_status,
    phone_number_from_receptionist_status,
    phone_number_from_google_maps_status,
    email_from_receptionist_status,
    email_from_google_maps_status,
    website_from_receptionist_status,
    website_from_google_maps_status
  ) %>%
  tbl_summary(
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{n} ({p})"
    ),
    digits = list(
      all_continuous() ~ 1,
      all_categorical() ~ c(0, 1)  # 0 for n, 1 for %
    ),
    missing_text = "Missing"
  ) %>%
  modify_footnote(everything() ~ NA)

{
  # Convert to flextable
  ft <- as_flex_table(tbl1)
  
  ft
  
  # Step 1: Get grouping structure (which rows belong to each variable)
  group_ids <- tbl1$table_body$variable  # same for each level of a variable
  row_groups <- rle(group_ids)
  
  # ✅ Fix: Use cumsum for accurate group boundaries
  row_ranges <- cumsum(row_groups$lengths)
  row_starts <- c(1, head(row_ranges, -1) + 1)
  row_ends <- row_ranges
  
  # Step 3: Assign alternating background colors by group
  for (i in seq_along(row_starts)) {
    bg_color <- ifelse(i %% 2 == 0, "#FFFFFF", "#E6E6E6")
    ft <- bg(ft, i = row_starts[i]:row_ends[i], j = 1:2, bg = bg_color, part = "body")
  }
  
  
  library(officer)
  
  # Optional: Styling
  ft <- ft %>%
    set_caption("Table 1. Distribution of variables in private health facilities (N=876).") %>% 
    set_header_labels(
      label = "Variable",
      stat_0 = "n (%)"
    ) %>% 
    fontsize(size = 12, part = "all") %>%
    bold(part = "header") %>%
    bg(part = "header", bg = "#FFFFFF") %>%
    width(j = 1, width = 5) %>%
    width(j = 2, width = 2) %>%
    border_remove() %>%
    border(j = 1:2, border.left = fp_border(width = 0), part = "all") %>%
    border(j = 1:2, border.right = fp_border(width = 0), part = "all") %>%
    border(j = 1:2, border.top = fp_border(width = 0), part = "body") %>%
    border(j = 1:2, border.bottom = fp_border(width = 0), part = "body") %>%
    border(i = nrow(tbl1$table_body), j = 1:2, border.bottom = fp_border(width = 1), part = "body") %>%
    border(j = 1:2, border.top = fp_border(width = 1.5), part = "header") %>%
    border(j = 1:2, border.bottom = fp_border(width = 1.5), part = "header") %>%
    add_footer_lines(values = c(
      ""
    )) %>%
    italic(part = "footer") %>%
    fontsize(size = 8, part = "footer") %>%
    align(align = "left", part = "footer")
  }


# Table 2. Stratified distribution by region

# Table stratified by region
tbl2 <- all_priv %>%
  select(
    with_dental_services, type_with_dental, type_without_dental,
    #type_of_clinic,
    #region, 
    new_region,
    clinic_name_in_english_from_sign_status,
    clinic_name_in_arabic_from_sign_status, does_the_clinic_pin_show_on_google_maps,
    clinic_name_in_english_from_google_maps_status,
    clinic_name_in_arabic_from_google_maps_status,
    phone_number_from_receptionist_status,
    phone_number_from_google_maps_status,
    email_from_receptionist_status,
    email_from_google_maps_status,
    website_from_receptionist_status,
    website_from_google_maps_status
  ) %>%
  tbl_summary(
    by = new_region,
    statistic = list(all_categorical() ~ "{p}"),
    type = all_categorical() ~ "categorical",
    digits = all_categorical() ~ 1,  # 1 decimal place for %
    missing = "no",
    missing_text = "Missing"
  ) %>%
  modify_footnote(everything() ~ NA)

# Keeping Yes and missing only (removing no rows)

# Table stratified by region
tbl2 <- priv %>%
  select(
    with_dental_services, type_with_dental, type_without_dental,
    #type_of_clinic,
    #region,
    new_region, clinic_name_in_english_from_sign_status,
    clinic_name_in_arabic_from_sign_status, does_the_clinic_pin_show_on_google_maps,
    clinic_name_in_english_from_google_maps_status,
    clinic_name_in_arabic_from_google_maps_status,
    phone_number_from_receptionist_status,
    phone_number_from_google_maps_status,
    email_from_receptionist_status,
    email_from_google_maps_status,
    website_from_receptionist_status,
    website_from_google_maps_status
  ) %>%
  tbl_summary(
    by = new_region,
    statistic = list(all_categorical() ~ "{p}"),
    type = all_categorical() ~ "categorical",
    digits = all_categorical() ~ 1,  # 1 decimal place for %
    missing = "no",
    missing_text = "Missing"
  ) %>%
  modify_table_body(
    ~ .x %>%
      filter(
        row_type == "label" | label %in% c("Yes", 
                                           "Dental clinic only",
                                           "Polyclinic/Cosmetic clinic",
                                           "Polyclinic/Cosmetic clinic",
                                           "Hospital"
        )
      )
  ) %>%
  modify_footnote(everything() ~ NA)

# Flextable (for Word export)
# Convert to flextable
ft <- as_flex_table(tbl2)

ft

{
  # Step 1: Get grouping structure (which rows belong to each variable)
  group_ids <- tbl2$table_body$variable  # same for each level of a variable
  row_groups <- rle(group_ids)
  
  # ✅ Fix: Use cumsum for accurate group boundaries
  row_ranges <- cumsum(row_groups$lengths)
  row_starts <- c(1, head(row_ranges, -1) + 1)
  row_ends <- row_ranges
  
  # Step 3: Assign alternating background colors by group
  for (i in seq_along(row_starts)) {
    bg_color <- ifelse(i %% 2 == 0, "#FFFFFF", "#E6E6E6")
    ft <- bg(ft, i = row_starts[i]:row_ends[i], j = 1:6, bg = bg_color, part = "body")
  }
  
  # Optional: Styling
  ft <- ft %>%
    set_caption("Table 2. Distribution of variables in private health facilities stratified by region (N=876).") %>% 
    set_header_labels(
      label = "Variable",
      stat_0 = "n (%) / mean (SD)"
    ) %>% 
    fontsize(size = 9, part = "all") %>%
    bold(part = "header") %>%
    bg(part = "header", bg = "#FFFFFF") %>%
    width(j = 1, width = 2) %>%
    width(j = 2:6, width = 0.7) %>%
    border_remove() %>%
    border(j = 1:6, border.left = fp_border(width = 0), part = "all") %>%
    border(j = 1:6, border.right = fp_border(width = 0), part = "all") %>%
    border(j = 1:6, border.top = fp_border(width = 0), part = "body") %>%
    border(j = 1:6, border.bottom = fp_border(width = 0), part = "body") %>%
    border(i = nrow(tbl2$table_body), j = 1:2, border.bottom = fp_border(width = 1), part = "body") %>%
    border(j = 1:6, border.top = fp_border(width = 1.13), part = "header") %>%
    border(j = 1:6, border.bottom = fp_border(width = 1.13), part = "header") %>%
    add_footer_lines(values = c(
      ""
    )) %>%
    italic(part = "footer") %>%
    fontsize(size = 8, part = "footer") %>%
    align(align = "left", part = "footer")
}


##################################################
# Section 4: Quality control check for rounds 3-11
##################################################

##############################################
##############################################
# - After deduplication, a total of 90 health facilities were found by either original data
#     collectors or the quality team.
#         - 58 were found by both original data collectors and quality  (64.4%)
#         - 25 facilities were found by original data collectors only   (27.8%)
#         - 7 facilities were found by quality only                     (7.8%)
#
#         - Original data collectors found 83 of those 90 (92.2%).   .
#         - Quality team found 65 out of those 90 (72.2%).
#       
# - Disagreements between original data collectors and quality occured in 
#   21 districts out of 154 we visited after round 2 (11.9%):
#       - South region  5 out of 33 we visited  (15.2%).
#       - Center region 8 out of 70 we visited  (11.4%).
#       - West region   4 out of 22 we visited  (18.2%).
#       - North region  2 out of 12 we visited  (16.7%).
#       - East region   2 out of 17 we visited  (11.8%).
#
# - Disagreements between original data collectors and quality occured in
#   25 street sections out of 301 street sections we randomly selected after (8.3%).
#       - South region  5 out of 65   (7.7%).
#       - Center region 8 out of 119  (6.7%).
#       - West region   4 out of 47   (8.5%).
#       - North region  2 out of 25   (8.0%).
#       - East region   2 out of 45   (4.4%).
##############################################
##############################################

# Publication ready table of quality check results
#### A) Packages ----
library(flextable)
library(officer)
library(scales)

#### Helper: pretty percent
pct <- function(x) percent(x/100, accuracy = 0.1)

#### Source numbers (from your notes) ----
# Facility discovery (N = 90)
discover_df <- tribble(
  ~Category,                                        ~Count, ~Percent,
  "Found by both original team & quality",             58,     64.4,
  "Found by original only",                            25,     27.8,
  "Found by quality only",                               7,      7.8
)
coverage_df <- tribble(
  ~Team,                        ~Count, ~Percent,
  "Original data collectors",      83,     92.2,
  "Quality team",                  65,     72.2
)

# Disagreements — districts (overall: 21 / 154 = 11.9%)
districts_df <- tribble(
  ~Region, ~Disagreements, ~Visited, ~Percent,
  "South",           5,        33,     15.2,
  "Center",          8,        70,     11.4,
  "West",            4,        22,     18.2,
  "North",           2,        12,     16.7,
  "East",            2,        17,     11.8
)

# Disagreements — street sections (overall: 25 / 301 = 8.3%)
streets_df <- tribble(
  ~Region, ~Disagreements, ~Sampled, ~Percent,
  "South",           5,        65,      7.7,
  "Center",          8,       119,      6.7,
  "West",            4,        47,      8.5,
  "North",           2,        25,      8.0,
  "East",            2,        45,      4.4
)

#### Build a single tidy table (Section / Item / Value) ----
one_table <- bind_rows(
  # Section 1: Facility discovery summary
  discover_df %>%
    transmute(
      Section = "Facility discovery summary (N = 90)",
      Item    = Category,
      Value   = paste0(format(Count, big.mark=","), " (", pct(Percent), ")")
    ),
  # Add coverage rows inside same section
  coverage_df %>%
    transmute(
      Section = "Facility discovery summary (N = 90)",
      Item    = paste0(Team, " coverage among the 90"),
      Value   = paste0(format(Count, big.mark=","), " / 90 (", pct(Percent), ")")
    ),
  
  # Section 2: Disagreements by region — Districts
  districts_df %>%
    transmute(
      Section = "Disagreements by region — Districts (overall: 21 / 154 = 11.9%)",
      Item    = Region,
      Value   = paste0(format(Disagreements, big.mark=","), " / ",
                       format(Visited, big.mark=","), " (", pct(Percent), ")")
    ),
  
  # Section 3: Disagreements by region — Street sections
  streets_df %>%
    transmute(
      Section = "Disagreements by region — Street sections (overall: 25 / 301 = 8.3%)",
      Item    = Region,
      Value   = paste0(format(Disagreements, big.mark=","), " / ",
                       format(Sampled, big.mark=","), " (", pct(Percent), ")")
    )
)

#### Make a single publication-ready flextable ----
ft_all <- one_table %>%
  flextable() %>%
  set_header_labels(
    Section = "",           # we’ll add a top title, no need for a "Section" header text
    Item    = "Item",
    Value   = "Value"
  ) %>%
  # Merge the Section cells vertically so each section title spans its block
  merge_v(j = "Section") %>%
  valign(j = "Section", valign = "top") %>%
  # Style
  theme_vanilla() %>%
  theme_zebra(even_body = "#F5F7FA", odd_body = "white") %>%
  # Emphasize Section column
  bold(j = "Section", part = "body") %>%
  fontsize(j = "Section", size = 10, part = "body") %>%
  # Alignment & padding
  align(j = c("Item","Value"), align = "center", part = "body") %>%
  align(align = "center", part = "header") %>%
  padding(padding = 6, part = "all") %>%
  # Clean borders with outer box
  border_remove() %>%
  hline_top(border = fp_border(color = "black", width = 1)) %>%
  hline_bottom(border = fp_border(color = "black", width = 1)) %>%
  vline_left(border = fp_border(color = "black", width = 1)) %>%
  vline_right(border = fp_border(color = "black", width = 1)) %>%
  # Overall table title as a spanning header row
  add_header_row(values = "Summary of Findings", colwidths = ncol_keys(.)) %>%
  bold(i = 1, part = "header") %>%
  align(i = 1, align = "center", part = "header") %>%
  autofit()

ft_all <- ft_all %>%
  set_caption(
    caption = "Table 3. Results of comparison between quality team and original data collecting teams."
  )

# Show it
ft_all


#########################################################
#########################################################
# Section 5: Taking a random sample of 100 for validation
#########################################################
#########################################################

# Adding unqique ID
all <- read_excel("Facility_data_stripped.xlsx", sheet = "Sheet1") %>% 
  arrange(round, group) 

# Restrict to Private only
all_priv <- all %>% filter(private_or_public=="Private")

# Randomly sorting the dataset before taking a random sample
set.seed(123)
final_data <- all_priv %>%
  slice_sample(prop = 1)

# Excluding manually added facilities by Talal
final_data_exc <- final_data %>% filter(!(group=="TA"))

# Taking a random sample of 100 for validation
set.seed(123)
final_data_exc_sample <- final_data_exc %>%
  slice_sample(n = 100) %>% 
  select(id, district_in_english, new_region, street)

###########################################
###########################################
# Section 6: Results of validation by Fahad
###########################################
###########################################

library(sf)

all <- read_excel("Facility_data_stripped.xlsx", sheet = "Sheet1") %>% 
  clean_names() %>% 
  select(
    id, 
    latitudes,
    longitudes,
    type_of_clinic, 
    with_dental_services) %>% 
  mutate(
    type_of_clinic = factor(
      type_of_clinic,
      levels = c(
        "Dental clinic only",
        "Polyclinic/Cosmetic clinic with dental",
        "Polyclinic/Cosmetic clinic without dental",
        "Hospital with dental",
        "Hospital without dental"
      )
    ),
    with_dental_services = factor(with_dental_services, levels = c("Yes", "No")),
    longitudes = as.numeric(longitudes),
    latitudes  = as.numeric(latitudes),
    
    # --- New collapsed variable ---
    clinic_group = case_when(
      type_of_clinic == "Dental clinic only" ~ "Dental clinic only",
      type_of_clinic %in% c("Polyclinic/Cosmetic clinic with dental",
                            "Polyclinic/Cosmetic clinic without dental") ~ "Polyclinic/Cosmetic clinic",
      type_of_clinic %in% c("Hospital with dental", "Hospital without dental") ~ "Hospital",
      TRUE ~ NA_character_
    ),
    clinic_group = factor(
      clinic_group,
      levels = c("Dental clinic only", "Polyclinic/Cosmetic clinic", "Hospital")
    )
  )

fahad <- read_excel("Validation_data.xlsx", sheet = "Sheet1") %>% 
  clean_names() %>% 
  select(
    id, 
    latitude_from_google_maps,
    longitude_from_google_maps,
    type_of_facility, 
    has_dental_clinic, 
    open_or_closed, 
    comments
  ) %>% 
  mutate(
    # --- Factor ordering ---
    type_of_facility = factor(
      type_of_facility,
      levels = c(
        "Dental clinic only",
        "Polyclinic/Cosmetic clinic with dental",
        "Polyclinic/Cosmetic clinic without dental",
        "Hospital with dental",
        "Hospital without dental"
      )
    ),
    open_or_closed = factor(open_or_closed, levels = c("Open", "closed")),
    
    # --- Recode has_dental_clinic based on type_of_facility ---
    has_dental_clinic = case_when(
      type_of_facility %in% c(
        "Dental clinic only",
        "Polyclinic/Cosmetic clinic with dental",
        "Hospital with dental"
      ) ~ "Yes",
      type_of_facility %in% c(
        "Polyclinic/Cosmetic clinic without dental",
        "Hospital without dental"
      ) ~ "No",
      TRUE ~ NA_character_
    ),
    has_dental_clinic = factor(has_dental_clinic, levels = c("Yes", "No")),
    
    # --- New collapsed variable ---
    facility_group = case_when(
      type_of_facility == "Dental clinic only" ~ "Dental clinic only",
      type_of_facility %in% c(
        "Polyclinic/Cosmetic clinic with dental",
        "Polyclinic/Cosmetic clinic without dental"
      ) ~ "Polyclinic/Cosmetic clinic",
      type_of_facility %in% c(
        "Hospital with dental",
        "Hospital without dental"
      ) ~ "Hospital",
      TRUE ~ NA_character_
    ),
    facility_group = factor(
      facility_group,
      levels = c("Dental clinic only", "Polyclinic/Cosmetic clinic", "Hospital")
    )
  )

compr <- all %>%
  inner_join(fahad, by = "id") %>% 
  st_as_sf(coords = c("longitudes", "latitudes"), crs = 4326) %>% 
  st_as_sf(coords = c("longitude_from_google_maps", "latitude_from_google_maps"), crs = 4326)

compr <- all %>%
  inner_join(fahad, by = "id") %>%
  mutate(
    # Create point geometries for both sources
    geom_all   = st_sfc(mapply(function(x, y) st_point(c(x, y)), longitudes, latitudes, SIMPLIFY = FALSE)),
    geom_fahad = st_sfc(mapply(function(x, y) st_point(c(x, y)), longitude_from_google_maps, latitude_from_google_maps, SIMPLIFY = FALSE))
  ) %>%
  st_as_sf(crs = 4326)

compr <- all %>%
  inner_join(fahad, by = "id") %>%
  mutate(
    geom_all = st_sfc(
      mapply(function(x, y) st_point(c(x, y)), longitudes, latitudes, SIMPLIFY = FALSE),
      crs = 4326
    ),
    geom_fahad = st_sfc(
      mapply(function(x, y) st_point(c(x, y)), longitude_from_google_maps, latitude_from_google_maps, SIMPLIFY = FALSE),
      crs = 4326
    )
  ) %>%
  st_as_sf()   # no need to pass crs here; each sfc already has it

ggplot() +
  geom_sf(data = compr %>% st_set_geometry("geom_all"), color = "blue", size = 2, alpha = 0.6) +
  geom_sf(data = compr %>% st_set_geometry("geom_fahad"), color = "red", size = 2, alpha = 0.6) +
  theme_minimal() +
  labs(
    title = "Comparison of Coordinates (All vs Fahad)",
    subtitle = "Blue = All, Red = Fahad"
  )

compr$geom_all   <- st_set_crs(compr$geom_all,   4326)
compr$geom_fahad <- st_set_crs(compr$geom_fahad, 4326)

compr <- compr %>%
  mutate(
    geom_all_m   = st_transform(geom_all,   32638),
    geom_fahad_m = st_transform(geom_fahad, 32638)
  )

compr <- compr %>%
  mutate(
    dist_m = as.numeric(st_distance(geom_all_m, geom_fahad_m, by_element = TRUE))
  )

summary(compr$dist_m)
compr2 <- compr %>%
  filter(id != 404 & id != 609 & id != 64)

summary(compr2$dist_m)
mean_distance <- mean(compr2$dist_m, na.rm = TRUE)
mean_distance

table(compr2$open_or_closed)

table(compr2$type_of_clinic, compr2$type_of_facility, useNA = "always")

table(compr2$clinic_group, compr2$facility_group, useNA = "always")

table(compr2$with_dental_services, compr2$has_dental_clinic, useNA = "always")

table(compr2$with_dental_services)
table(compr2$has_dental_clinic)

table(compr2$has_dental_clinic, compr2$type_of_facility)
table(compr2$has_dental_clinic, compr2$facility_group)


#####################################
#####################################
# Section 7: Preparing GIS Riyadh Map
#####################################
#####################################

{
  # Need packages
  library(sf)     #only this was used

  # Reading in full data after manually adding MOH facilities
  all_teams_14 <- read_excel("Facility_data_stripped.xlsx", sheet = "Sheet1") %>%
    clean_names() %>%
    select(
      id, district_in_english, new_region,
      type_of_clinic, longitudes, latitudes,
      with_dental_services, type_with_dental, type_without_dental, private_or_public
    ) %>%
    mutate(
      # Convert already coded variables to factors
      type_of_clinic = factor(
        type_of_clinic,
        levels = c(
          "Dental clinic only",
          "Polyclinic/Cosmetic clinic with dental",
          "Polyclinic/Cosmetic clinic without dental",
          "Hospital with dental",
          "Hospital without dental",
          "Primary care center with dental clinic",
          "Specialized dental center"
        )
      ),
      with_dental_services = factor(with_dental_services, levels = c("Yes", "No")),
      type_with_dental = factor(
        type_with_dental,
        levels = c(
          "Dental clinic only",
          "Polyclinic/Cosmetic clinic",
          "Hospital",
          "Primary care center",
          "Specialized dental center"
        )
      ),
      type_without_dental = factor(
        type_without_dental,
        levels = c("Polyclinic/Cosmetic clinic", "Hospital")
      ),
      new_region = factor(new_region, levels = c("North", "East", "Center", "West", "South")),
      private_or_public = factor(private_or_public, levels = c("Private", "Public")),
      longitudes = as.numeric(gsub("[^0-9.\\-]", "", longitudes)),
      latitudes = as.numeric(gsub("[^0-9.\\-]", "", latitudes))
    ) %>%
    set_variable_labels(
      id                   = "ID",
      district_in_english  = "District of facility in English",
      new_region           = "Official region of facility recently determined by Riyadh Municipality",
      type_of_clinic       = "Type of facility",
      longitudes           = "Longitude",
      latitudes            = "Latitude",
      with_dental_services = "Facility provided dental services",
      type_with_dental     = "Type of facility that provided dental services",
      type_without_dental  = "Type of facility that did not provide dental services",
      private_or_public    = "Private or Public facility"
    )
  
  # Converting coords to numeric
  all_teams_14 <- all_teams_14 %>%
    mutate(
      longitudes = as.numeric(longitudes),
      latitudes  = as.numeric(latitudes)
    )
  
  # Convert to sf object
  gisdata <- all_teams_14 %>%
    st_as_sf(coords = c("longitudes", "latitudes"), crs = 4326)
  
  # 1. Load the Riyadh districts GeoJSON
  riyadh_distr <- st_read("districts.geojson")
  
  # 2. Filter to Riyadh city and fix geometries if needed
  riyadh_only <- riyadh_distr %>%
    filter(city_id == 3) %>%
    mutate(geometry = st_make_valid(geometry))
  
  # 3. Define Al Bashaer manual boundary coordinates
  coords_manual <- matrix(c(
    46.87291145324707,   24.968163214832895,
    46.85218334197998,   24.9918538240452,
    46.851024627685554,  24.99383755957201,
    46.850080490112305,  24.994421005104304,
    46.84879302978516,   24.994693278738193,
    46.842269897460945,  25.000566463128987,
    46.81252956390381,   25.017367665213843,
    46.793818473815925,  25.02502856137325,
    46.78300380706788,   25.028644960423627,
    46.77321910858155,   25.03078364089392,
    46.76510810852051,   25.03171687159886,
    46.76120281219483,   25.031833524937895,
    46.86741828918458,   24.965867823056886,
    46.87063694000244,   24.95968172358859,
    46.86342716217042,   24.938864577975956,
    46.86038017272949,   24.93384459026544,
    46.85720443725586,   24.927189872768054,
    46.857032775878906,  24.920729394011193,
    46.856689453125,     24.919367201667008,
    46.85514450073242,   24.918666639745364,
    46.856217384338386,  24.917537948278824,
    46.85411453247071,   24.91247816994592,
    46.85265541076661,   24.908079886329716,
    46.85265541076661,   24.908040962765924,
    46.847248077392585,  24.905705526469728,
    46.84063911437988,   24.90500488696166,
    46.83917999267579,   24.90990927997662,
    46.83587551116944,   24.91625356266607,
    46.8314552307129,    24.921858056268917,
    46.82411670684815,   24.928201724319663,
    46.810040473937995,  24.9407324281741,
    46.78853988647462,   24.959992981304392,
    46.78643703460694,   24.96248301469683,
    46.78163051605225,   24.969913908591025,
    46.78047180175781,   24.97302619154367,
    46.77472114562989,   24.98990895421319,
    46.76592350006104,   25.017017663614194
  ), ncol = 2, byrow = TRUE)
  
  # 4. Apply the specified point order: 1-12, then 37-13, then back to 1
  coords_ordered <- rbind(
    coords_manual[1:12, ],       # Forward from 1 to 12
    coords_manual[37:13, ],      # Backwards from 37 to 13
    coords_manual[1, ]           # Close the polygon back to point 1
  )
  
  # 5. Build the polygon
  al_bashaer_polygon <- st_polygon(list(coords_ordered)) %>%
    st_sfc(crs = st_crs(riyadh_only))
  
  # Check validity and fix if necessary
  if (!st_is_valid(al_bashaer_polygon)) {
    al_bashaer_polygon <- st_make_valid(al_bashaer_polygon)
  }
  
  # 6. Create sf object for Al Bashaer District
  al_bashaer_sf <- st_sf(
    district_id = NA,
    city_id = 3,
    region_id = NA,
    name_ar = "حي البشائر",
    name_en = "Al Bashaer Dist.",
    geometry = al_bashaer_polygon
  )
  al_bashaer_sf <- al_bashaer_sf[, names(riyadh_only)]
  
  # 7. Combine with existing Riyadh districts
  riyadh_with_bashaer <- rbind(riyadh_only, al_bashaer_sf)

  # Rename Al Sidrah Dist. to Asehbaa Dist. and حي السدرة to حي السهباء
  riyadh_with_bashaer <- riyadh_with_bashaer %>%
    mutate(
      name_ar = ifelse(name_ar == "حي السدرة", "حي السهباء", name_ar),
      name_en = ifelse(name_en == "Al Sidrah Dist.", "Asehbaa Dist.", name_en)
    )
  
  # 1. Define Sedrah District manual boundary coordinates
  coords_sedrah <- matrix(c(
    46.723651885986335, 24.831142982954567,
    46.760902404785156, 24.850148046867947,
    46.75867080688477,  24.85388640440865,
    46.7545509338379,   24.85154994418473,
    46.75352096557618,  24.8531075892375,
    46.77429199218751,  24.86354330510114,
    46.775321960449226, 24.860739766457296,
    46.77772521972657,  24.859026461549753,
    46.790084838867195, 24.865412295547756,
    46.7827033996582,   24.87724857884445,
    46.783561706542976, 24.879896013383163,
    46.78184509277344,  24.882387664656264,
    46.780643463134766, 24.883010569622254,
    46.77995681762696,  24.88565788068486,
    46.77206039428711,  24.891419479211137,
    46.76691055297852,  24.892665194900072,
    46.75695419311524,  24.892665194900072,
    46.751461029052734, 24.89437803345151,
    46.7464828491211,   24.893755185818428,
    46.74081802368164,  24.891263763866398,
    46.727943420410156, 24.885346435269415,
    46.725883483886726, 24.88332202092735,
    46.7245101928711,   24.879896013383163,
    46.72931671142578,  24.87226501877319,
    46.73154830932618,  24.866658273481924,
    46.7325782775879,   24.862608799282842,
    46.73360824584962,  24.861830039038882,
    46.73309326171876,  24.860428258239736,
    46.73377990722657,  24.85295182561484,
    46.732406616210945, 24.846876891350952,
    46.72914505004883,  24.838153387193202,
    46.723651885986335, 24.831142982954567  # Closing the polygon
  ), ncol = 2, byrow = TRUE)
  
  # 2. Build the polygon
  sedrah_polygon <- st_polygon(list(coords_sedrah)) %>%
    st_sfc(crs = 4326)  # Assuming WGS84
  
  # Check validity and fix if needed
  if (!st_is_valid(sedrah_polygon)) {
    sedrah_polygon <- st_make_valid(sedrah_polygon)
  }
  
  # 3. Create sf object for Sedrah District
  sedrah_sf <- st_sf(
    district_id = NA,
    city_id = 3,
    region_id = NA,
    name_ar = "سدرة",
    name_en = "Sedrah",
    geometry = sedrah_polygon
  )
  
  # ✅ If you're merging with the Riyadh dataset:
  sedrah_sf <- sedrah_sf[, names(riyadh_with_bashaer)]
  
  # 4. Combine with Riyadh dataset
  riyadh_with_sedrah <- rbind(riyadh_with_bashaer, sedrah_sf)
  
  # ✅ Define the districts to merge
  districts_to_merge <- c("Al Wahah Dist.", "Salahuddin Dist.")
  new_name_en <- "King Salman Dist."
  new_name_ar <- "حي الملك سلمان"
  
  # ✅ Apply the merge
  riyadh_merged <- riyadh_with_sedrah %>%
    mutate(
      name_en = ifelse(name_en %in% districts_to_merge, new_name_en, name_en),
      name_ar = ifelse(name_en == new_name_en, new_name_ar, name_ar)
    ) %>%
    group_by(name_en, name_ar) %>%
    summarise(geometry = st_union(geometry), .groups = "drop")
  
  # Ensuring that coords are valid
  riyadh_merged <- st_make_valid(riyadh_merged)
  
  # Sorting dataset by English name
  riyadh_merged <- riyadh_merged %>%
    arrange(name_en)
  
  # Remove Dist. and حي from names
  riyadh_merged <- riyadh_merged %>%
    mutate(
      name_en = str_remove(name_en, " Dist\\.$"),  # Remove 'Dist.' at end
      name_ar = str_remove(name_ar, "^حي\\s")      # Remove 'حي ' at start
    )
  
  # Getting official names of districts in Arabic and English
  names_dist <- read_excel("District_population_street_data.xlsx", sheet = "List") %>% 
    clean_names() %>% 
    select(district_name_in_english,	name_en, new_region) %>% 
    arrange(name_en)
  
  # Merging riyadh_merged and names_dist by name_en
  riyadh_merged_2 <- riyadh_merged %>%
    left_join(names_dist, by = "name_en") %>% 
    select(district_name_in_english, new_region,
           geometry) %>% 
    arrange(district_name_in_english)
  
  # Determining range
  health_buffered <- st_buffer(gisdata, dist = 1000)  # in meters
  
  # Controlling the order of region categories
  riyadh_merged_2 <- riyadh_merged_2 %>%
    mutate(new_region = factor(new_region, levels = c("North", "East", "Center", "West", "South")))
  
  # Getting district labels in English and wrap them as text
  district_labels <- riyadh_merged_2 %>%
    st_point_on_surface() %>%                # always inside the polygon
    cbind(st_coordinates(.)) %>%
    mutate(
      wrapped_name = str_wrap(district_name_in_english, width = 12)  # wrap names nicely
    )
  
  # --- NEW GIS-BASED FIX FOR OVERLAPPING POLYGONS ---
  
  # 1. Isolate the two districts involved
  # (Ensure spelling matches your data exactly)
  overlapping_district_name <- "Sedrah"
  overlapped_district_name <- "King Khalid International Airport"
  
  # --- FIX: Get the original column names to ensure match after spatial operation ---
  original_cols <- names(riyadh_merged_2)
  
  sedrah_poly <- riyadh_merged_2 %>%
    filter(district_name_in_english == overlapping_district_name)
  
  airport_poly <- riyadh_merged_2 %>%
    filter(district_name_in_english == overlapped_district_name)
  
  # 2. Isolate all other districts
  other_districts <- riyadh_merged_2 %>%
    filter(district_name_in_english != overlapping_district_name &
             district_name_in_english != overlapped_district_name)
  
  # 3. Perform the "difference" operation
  # This geometrically cuts the 'Sedrah' shape out of the 'Airport' shape
  # We use st_make_valid() to prevent potential geometry errors
  airport_poly_fixed <- st_difference(st_make_valid(airport_poly), st_make_valid(sedrah_poly)) %>%
    # --- FIX: Force the dataframe to have the *exact* same columns as the others ---
    select(all_of(original_cols))
  
  # 4. Recombine all polygons into one clean 'sf' dataframe
  # This new dataframe has no overlaps and matching columns
  riyadh_merged_2 <- rbind(other_districts, airport_poly_fixed, sedrah_poly)
  
  # --- End of new GIS fix ---
}


# Riyadh map with classification of regions in English
# Fine tuning

# Ensure the 'sf' library is loaded for st_difference
library(ggrepel) # <-- Make sure this library is loaded

# --- NEW STEP: Filter labels based on district area ---
# 1. Calculate area for each district and get a dataframe
district_areas <- riyadh_merged_2 %>%
  mutate(area = as.numeric(st_area(geometry))) %>%
  sf::st_drop_geometry() %>%
  # --- FIX: Use the actual shared name column ---
  select(district_name_in_english, area)

# 2. Join the area data to your existing labels dataframe
labels_with_area <- dplyr::left_join(district_labels, district_areas, by = "district_name_in_english")

# 3. Find the area cutoff for the smallest 40% of districts
area_threshold <- quantile(labels_with_area$area, 0.5, na.rm = TRUE)

# 4. Create a new dataframe of *only* the labels we want to plot
labels_to_plot <- labels_with_area %>%
  filter(area >= area_threshold)

# --- End of label filtering step ---


# Saving as JPG
ggplot() +
  geom_sf(
    data = riyadh_merged_2,
    aes(fill = new_region),
    # --- AESTHETIC TWEAK: Softer borders ---
    color = "gray50",
    linewidth = 0.2,
    alpha = 0.5
  ) +
  # --- PROFESSIONAL PLOT FIX: Use ggrepel for readable, non-overlapping labels ---
  ggrepel::geom_text_repel(
    data = labels_to_plot, # <-- Use the filtered labels
    aes(x = X, y = Y, label = wrapped_name),
    size = 4, # <-- OPTIMIZATION: Increased size slightly
    color = "gray20", # <-- OPTIMIZATION: Softer than pure black
    alpha = 1.0, # <-- OPTIMIZATION: Fully opaque for crispness
    box.padding = 0.8, # <-- Increased padding
    point.padding = 0.5, # <-- Increased padding
    max.overlaps = Inf,
    min.segment.length = 0, # Allow segments even for short moves
    segment.color = "gray30", # <-- Subtle leader lines
    segment.size = 0.3,
    family = "Arial" # <-- FONT FIX: Explicitly set family in ggrepel
  ) +
  coord_sf(expand = FALSE) +
  # --- PROFESSIONAL PLOT FIX: Use theme_void and set a specific font ---
  theme_void(
    base_size = 14,
    base_family = "Arial" # <-- Explicitly set the font family here
  ) +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    # --- AESTHETIC TWEAK: Move legend to bottom ---
    legend.position = "bottom",
    legend.direction = "horizontal", # <-- OPTIMIZATION: Force horizontal legend
    legend.title = element_text(size=16, face = "bold", hjust = 0.5), # <-- OPTIMIZATION: Center legend title
    legend.key.size = unit(1.5, "lines"),
    legend.text = element_text(size = 16) # Slightly smaller legend text
  ) +
  # --- PROFESSIONAL PLOT FIX: Remove title (put in caption) ---
  labs(
    fill = "Region" # <-- Keep legend title
  ) +
  # --- USER REQUEST: Revert to Pastel2 ---
  scale_fill_brewer(palette = "Pastel2") +
  # --- OPTIMIZATION: Ensure legend is single row ---
  guides(fill = guide_legend(nrow = 1))


###########################################################
###########################################################
# Section 8: GIS Private and Public Dental Facility density
###########################################################
###########################################################

# Loading package for color
library(RColorBrewer)

# Modifying corrdinates
gis_points <- gisdata %>%
  st_centroid() %>%
  cbind(st_coordinates(.))

# ---- Libraries ----
library(sf)
library(dplyr)
library(ggplot2)
library(viridis)
library(ragg)
library(grid)
library(patchwork)

# --- Filter once ---
gis_points_filtered <- gis_points %>% filter(with_dental_services == "Yes")

# --- Riyadh mask (true boundary) ---
riyadh_mask <- riyadh_merged_2 %>% st_union() %>% st_make_valid()

# --- bbox + outside mask ---
bbox_poly   <- st_as_sfc(st_bbox(riyadh_mask))
mask_outside <- st_difference(bbox_poly, riyadh_mask)

# --- Facet labels with n ---
group_counts <- gis_points_filtered %>% count(private_or_public, name = "n")
facet_labels <- setNames(paste0(group_counts$private_or_public, " (n = ", group_counts$n, ")"),
                         group_counts$private_or_public)

# --- Region-level polygons ---
riyadh_regions <- riyadh_merged_2 %>%
  st_make_valid() %>%
  group_by(new_region) %>%
  summarise(do_union = TRUE, .groups = "drop")

# ============================================================
# 🧭 1) Absolute (shared-scale continuous)
# ============================================================
p_abs <- ggplot() +
  geom_sf(data = riyadh_merged_2, fill = "black", color = "gray30", linewidth = 0.2) +
  stat_density_2d(
    data = gis_points_filtered,
    aes(x = X, y = Y, fill = after_stat(density)),  # absolute density (continuous)
    contour      = FALSE,
    geom         = "raster",
    n            = 200,
    show.legend  = TRUE
  ) +
  geom_sf(data = mask_outside, fill = "white", color = NA) +
  geom_sf(data = riyadh_merged_2, fill = NA, color = "gray40", linewidth = 0.2) +
  geom_sf(data = riyadh_regions, fill = NA, color = "white", linewidth = 0.7) +
  facet_wrap(~ private_or_public, labeller = labeller(private_or_public = facet_labels)) +
  scale_fill_viridis_c(option = "magma",
                       name = "Facility density\n(absolute)",
                       limits = c(0, 35)) +
  coord_sf(
    xlim = st_bbox(riyadh_mask)[c("xmin","xmax")],
    ylim = st_bbox(riyadh_mask)[c("ymin","ymax")],
    expand = FALSE, clip = "on"
  ) +
  theme_void(base_size = 14, base_family = "Arial") +
  theme(
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    legend.position  = "bottom",
    legend.direction = "horizontal",
    legend.title     = element_text(face = "bold", size = 26),
    legend.text      = element_text(size = 22),
    legend.key.width  = unit(2, "cm"),
    legend.key.height = unit(0.5, "cm"),
    strip.text = element_text(size = 30, face = "bold", hjust = 0.5, colour = "black"),
    plot.title = element_text(size = 32, face = "bold", hjust = 0.5)
  ) +
  ggtitle("A. Absolute Density (Shared Scale)")


# ============================================================
# 🌡️ 2) Relative (facet-specific continuous 0–100%)
# ============================================================
p_rel <- ggplot() +
  geom_sf(data = riyadh_merged_2, fill = "black", color = "gray30", linewidth = 0.2) +
  stat_density_2d(
    data = gis_points_filtered,
    aes(x = X, y = Y, fill = after_stat(ndensity)),  # normalized within facet
    contour      = FALSE,
    geom         = "raster",
    n            = 200,
    contour_var  = "ndensity",
    show.legend  = TRUE
  ) +
  geom_sf(data = mask_outside, fill = "white", color = NA) +
  geom_sf(data = riyadh_merged_2, fill = NA, color = "gray40", linewidth = 0.2) +
  geom_sf(data = riyadh_regions, fill = NA, color = "white", linewidth = 0.7) +
  facet_wrap(~ private_or_public, labeller = labeller(private_or_public = facet_labels)) +
  scale_fill_viridis_c(
    option = "magma",
    name   = "Relative density\n(within type)",
    limits = c(0, 1),
    labels = scales::label_percent(accuracy = 1)
  ) +
  coord_sf(
    xlim = st_bbox(riyadh_mask)[c("xmin","xmax")],
    ylim = st_bbox(riyadh_mask)[c("ymin","ymax")],
    expand = FALSE, clip = "on"
  ) +
  theme_void(base_size = 14, base_family = "Arial") +
  theme(
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    legend.position  = "bottom",
    legend.direction = "horizontal",
    legend.title     = element_text(face = "bold", size = 26),
    legend.text      = element_text(size = 22),
    legend.key.width  = unit(2, "cm"),
    legend.key.height = unit(0.5, "cm"),
    strip.text = element_text(size = 30, face = "bold", hjust = 0.5, colour = "black"),
    plot.title = element_text(size = 32, face = "bold", hjust = 0.5)
  ) +
  ggtitle("B. Relative Density (Type-Specific)")


# ============================================================
# 🧩 Combine both plots (vertical layout)
# ============================================================
combined_plot <- p_abs / plot_spacer() / p_rel +
  plot_layout(heights = c(1, 0.08, 1))  # middle = vertical gap between legends/titles

combined_plot

########################################################
########################################################
# Section 9: GIS Private Dental Facility Density by Type
########################################################
########################################################

library(dplyr)
library(sf)
library(ggplot2)
library(viridis)
library(ragg)
library(grid)
library(patchwork)

# --- Filter: Private facilities with dental services only ---
gis_points_private <- gis_points %>%
  filter(
    with_dental_services == "Yes",
    private_or_public == "Private",
    !is.na(type_with_dental)
  )

# --- Riyadh mask (true boundary) ---
riyadh_mask <- riyadh_merged_2 %>%
  st_union() %>%
  st_make_valid()

# --- Bounding box polygon around Riyadh ---
bbox_poly <- st_as_sfc(st_bbox(riyadh_mask))

# --- Outside mask: area outside Riyadh but inside bbox ---
mask_outside <- st_difference(bbox_poly, riyadh_mask)

# --- Region-level polygons (for bold region borders) ---
riyadh_regions <- riyadh_merged_2 %>%
  st_make_valid() %>%
  group_by(new_region) %>%
  summarise(.groups = "drop")

# --- Facet labels with n by type_with_dental ---
group_counts <- gis_points_private %>%
  count(type_with_dental, name = "n")

facet_labels <- setNames(
  paste0(group_counts$type_with_dental, " (n = ", group_counts$n, ")"),
  group_counts$type_with_dental
)

# ============================================================
# 1) ABSOLUTE (shared-scale continuous) across all facets
# ============================================================
p_abs_type <- ggplot() +
  # 1) Base black map (districts)
  geom_sf(
    data = riyadh_merged_2,
    fill = "black",
    color = "gray30",
    linewidth = 0.2
  ) +
  # 2) Continuous absolute density (same scale for all facets)
  stat_density_2d(
    data  = gis_points_private,
    aes(x = X, y = Y, fill = after_stat(density)),
    contour     = FALSE,
    geom        = "raster",   # or "tile"
    n           = 200,
    show.legend = TRUE
  ) +
  # 3) Clip outside Riyadh
  geom_sf(data = mask_outside, fill = "white", color = NA) +
  # 4) District borders
  geom_sf(
    data = riyadh_merged_2,
    fill = NA,
    color = "gray50",
    linewidth = 0.2
  ) +
  # 5) Region borders (highlighted)
  geom_sf(
    data = riyadh_regions,
    fill = NA,
    color = "white",
    linewidth = 0.8
  ) +
  # 6) Facets
  facet_wrap(
    ~ type_with_dental,
    labeller = labeller(type_with_dental = facet_labels)
  ) +
  # 7) Color scale (continuous)
  scale_fill_viridis_c(
    option = "magma",
    name   = "Facility density\n(absolute)",
    limits = c(0, 35)
  ) +
  # 8) Tight extent
  coord_sf(
    xlim   = st_bbox(riyadh_mask)[c("xmin", "xmax")],
    ylim   = st_bbox(riyadh_mask)[c("ymin", "ymax")],
    expand = FALSE,
    clip   = "on"
  ) +
  # 9) Theme + font sizes
  theme_void(base_size = 14, base_family = "Arial") +
  theme(
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    
    legend.position   = "bottom",
    legend.direction  = "horizontal",
    legend.title      = element_text(face = "bold", size = 26, hjust = 0.5, color = "black"),
    legend.text       = element_text(size = 22, color = "black"),
    legend.key.width  = unit(2, "cm"),
    legend.key.height = unit(0.5, "cm"),
    
    strip.text = element_text(size = 30, face = "bold", hjust = 0.5, margin = margin(b = 8), colour = "black"),
    plot.title = element_text(size = 32, face = "bold", hjust = 0.5),
    panel.spacing.x     = unit(4, "cm")   # <-- increases space between facet maps
  ) +
  ggtitle("A. Absolute Density (Shared Scale)")

# ============================================================
# 2) RELATIVE (facet-specific continuous 0–100%)
# ============================================================
p_rel_type <- ggplot() +
  # 1) Base black map (districts)
  geom_sf(
    data = riyadh_merged_2,
    fill = "black",
    color = "gray30",
    linewidth = 0.2
  ) +
  # 2) Continuous relative density (normalized within each facet)
  stat_density_2d(
    data  = gis_points_private,
    aes(x = X, y = Y, fill = after_stat(ndensity)),
    contour      = FALSE,
    geom         = "raster",   # or "tile"
    n            = 200,
    contour_var  = "ndensity", # key for per-facet normalization
    show.legend  = TRUE
  ) +
  # 3) Clip outside Riyadh
  geom_sf(data = mask_outside, fill = "white", color = NA) +
  # 4) District borders
  geom_sf(
    data = riyadh_merged_2,
    fill = NA,
    color = "gray50",
    linewidth = 0.2
  ) +
  # 5) Region borders (highlighted)
  geom_sf(
    data = riyadh_regions,
    fill = NA,
    color = "white",
    linewidth = 0.8
  ) +
  # 6) Facets
  facet_wrap(
    ~ type_with_dental,
    labeller = labeller(type_with_dental = facet_labels)
  ) +
  # 7) Color scale (continuous 0–100% per facet)
  scale_fill_viridis_c(
    option = "magma",
    name   = "Relative density\n(within type)",
    limits = c(0, 1),
    labels = scales::label_percent(accuracy = 1)
  ) +
  # 8) Tight extent
  coord_sf(
    xlim   = st_bbox(riyadh_mask)[c("xmin", "xmax")],
    ylim   = st_bbox(riyadh_mask)[c("ymin", "ymax")],
    expand = FALSE,
    clip   = "on"
  ) +
  # 9) Theme + font sizes
  theme_void(base_size = 14, base_family = "Arial") +
  theme(
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    
    legend.position   = "bottom",
    legend.direction  = "horizontal",
    legend.title      = element_text(face = "bold", size = 26, hjust = 0.5, color = "black"),
    legend.text       = element_text(size = 22, color = "black"),
    legend.key.width  = unit(2, "cm"),
    legend.key.height = unit(0.5, "cm"),
    
    strip.text = element_text(size = 30, face = "bold", hjust = 0.5, margin = margin(b = 8), colour = "black"),
    plot.title = element_text(size = 32, face = "bold", hjust = 0.5),
    panel.spacing.x     = unit(4, "cm")   # <-- increases space between facet maps
  ) +
  ggtitle("B. Relative Density (Type-Specific)")

# ============================================================
# 3) Combine into one figure with a spacer between panels
# ============================================================
combined_type <- p_abs_type / plot_spacer() / p_rel_type +
  plot_layout(heights = c(1, 0.08, 1))

combined_type


################################################################
################################################################
# Section 10: Heatmap of number of dental facilities by district
################################################################
################################################################

# Ensure CRS match
gisdata <- st_transform(gisdata, st_crs(riyadh_merged_2))

# Spatial join to assign each point to a district
gis_with_district <- st_join(gisdata, riyadh_merged_2["district_name_in_english"])

# Count private dental facilities per district
facility_counts <- gis_with_district %>%
  st_drop_geometry() %>%
  group_by(district_name_in_english) %>%
  summarise(
    n_private_dental = sum(with_dental_services == "Yes" & private_or_public == "Private", na.rm = TRUE),
    n_private_total  = sum(private_or_public == "Private", na.rm = TRUE)
  ) %>%
  ungroup()

# Number of facilities in each district
riyadh_counted <- riyadh_merged_2 %>%
  left_join(facility_counts, by = "district_name_in_english") %>%
  mutate(
    n_private_dental = ifelse(is.na(n_private_dental), 0, n_private_dental),
    n_private_total  = ifelse(is.na(n_private_total), 0, n_private_total),
    pct_private_dental = ifelse(
      n_private_total > 0,
      round((n_private_dental / n_private_total) * 100, 0),
      NA_real_
    )
  )

# Reading in population data per district
pop <- read_excel("District_population_street_data.xlsx", sheet = "New Riyadh Regions") %>% 
  clean_names() %>% 
  select(district_name_in_english, population, done)

# Number of facilities in each district
riyadh_counted_2 <- riyadh_counted %>%
  left_join(pop, by = "district_name_in_english") %>% 
  mutate(
    clinics_per_10000 = round((n_private_dental / population) * 10000, 1),
    population_per_facility = population / n_private_dental
  )

# Clinics per 10,000 population + updated data_status rule
riyadh_counted_3 <- riyadh_counted_2 %>%
  mutate(
    data_status = case_when(
      done == "No" ~ "Not surveyed",
      is.na(population) | population == 0 ~ "Surveyed but no population",
      TRUE ~ "Valid"
    )
  )

# --- FONT FIX: Load the 'ragg' library. It is better at handling fonts. ---
# You may need to run install.packages("ragg") in your console once.
library(ragg)

# --- Ensure all required libraries are loaded ---
library(sf)
library(dplyr)
library(ggplot2)
library(ggnewscale) # For multiple color/fill scales
library(viridis) # For Mako palette

# --- Plot 1: Number of dental facilities per district ---
ggplot() +
  # --- 1) VALID districts: numeric fill (n_private_dental) ---
  geom_sf(
    data = riyadh_counted_3 %>% filter(data_status == "Valid"),
    aes(fill = n_private_dental),
    color = "white", # <-- OPTIMIZATION: White borders are cleaner
    linewidth = 0.3 # Thinner white line
  ) +
  # --- OPTIMIZATION: Switched to Mako (Viridis) palette ---
  scale_fill_viridis_c(
    option = "mako", # Mako is a pro-level sequential palette
    direction = -1, # Reverses it so yellow = high, dark blue = low
    name = "Facility Count",
    breaks = seq(0, 35, by = 5),
    guide = guide_colorbar(
      order = 1,
      # --- OPTIMIZATION: Tall, thin vertical bar ---
      barwidth = unit(1.5, "lines"),
      barheight = unit(20, "lines"),
      title.position = "top",
      title.hjust = 0.5
    )
  ) +
  
  # --- Switch to a new fill scale for categorical status ---
  ggnewscale::new_scale_fill() +
  
  # --- 2) NON-VALID: overlay 'No population' districts in grey ---
  geom_sf(
    data = riyadh_counted_3 %>% filter(data_status == "Not surveyed" | data_status == "Surveyed but no population"),
    aes(fill = data_status),
    color = "white", # <-- OPTIMIZATION: White borders
    linewidth = 0.3
  ) +
  scale_fill_manual(
    values = c(
      "Surveyed but no population" = "grey90",
      "Not surveyed"  = "grey85"
    ),
    breaks = c("Surveyed but no population", "Not surveyed"),
    labels = c("Surveyed but no population", "Not surveyed"),
    name   = NULL,
    guide = guide_legend(
      order = 2,
      override.aes = list(linewidth = 0.1)
    )
  ) +
  
  # --- District labels & title removed ---
  
  coord_sf(expand = FALSE) +
  
  # --- PROFESSIONAL PLOT FIX: Use theme_void and set a specific font ---
  theme_void(
    base_size = 14,
    base_family = "Arial" # <-- Explicitly set the font family here
  ) +
  theme(
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    
    # --- AESTHETIC TWEAK: Move legend to right for vertical stack ---
    legend.position = "right",
    legend.direction = "vertical",
    legend.box = "vertical",
    legend.title = element_text(face = "bold", size = 16, hjust = 0.5), # <-- OPTIMIZATION: Proportional font
    legend.text  = element_text(size = 14) # <-- OPTIMIZATION: Proportional font
  ) +
  
  # --- PROFESSIONAL PLOT FIX: Remove title (put in caption) ---
  labs(
    title = NULL,
    fill  = NULL # Titles are set in the scales
  )



# 2) Plot percentage of dental facilities out of all private facilities per district

# Saving as JPG
library(ragg)
library(sf)
library(dplyr)
library(ggplot2)
library(ggnewscale)
library(viridis)

# --- 0) Region-level polygons for clean region borders ---
riyadh_regions <- riyadh_counted_3 %>%
  st_make_valid() %>%
  dplyr::group_by(new_region) %>%
  dplyr::summarise(.groups = "drop")

# --- 1) Plot: % of private facilities providing dental services out of all private facilities ---
p <- ggplot() +
  # --- District-level fill by percentage ---
  geom_sf(
    data = riyadh_counted_3 %>% filter(!is.na(pct_private_dental)),
    aes(fill = pct_private_dental),
    color = "white",
    linewidth = 0.2
  ) +
  scale_fill_viridis_c(
    option     = "mako",
    direction  = -1,   # reversed color order (light = high)
    name       = "% of private facilities\nwith dental services",
    limits     = c(0, 100),
    breaks     = seq(0, 100, by = 20),
    labels     = function(x) paste0(x, "%"),
    guide      = guide_colorbar(
      order         = 1,
      barwidth      = unit(1.5, "lines"),
      barheight     = unit(20, "lines"),
      title.position = "top",
      title.hjust    = 0.5
    )
  ) +
  
  # --- Switch to new fill scale for categorical statuses ---
  ggnewscale::new_scale_fill() +
  
  # --- Surveyed but no population districts (grey fill) ---
  geom_sf(
    data  = riyadh_counted_3 %>% filter(data_status == "Surveyed but no population"),
    aes(fill = "Surveyed but no population"),
    color = "white",
    linewidth = 0.2
  ) +
  
  # --- Surveyed but no private facility found (light grey) ---
  geom_sf(
    data = riyadh_counted_3 %>%
      filter(data_status == "Valid", is.na(pct_private_dental)),
    aes(fill = "Surveyed but no facility found"),
    color = "white",
    linewidth = 0.2
  ) +
  # --- Not surveyed (light grey) ---
  geom_sf(
    data = riyadh_counted_3 %>%
      filter(is.na(pct_private_dental), data_status == "Not surveyed"),
    aes(fill = "Not surveyed"),
    color = "white",
    linewidth = 0.2
  ) +
  
  # --- Legend for data status (no title) ---
  scale_fill_manual(
    values = c(
      "Surveyed but no population"             = "grey90",
      "Surveyed but no facility found" = "grey70",
      "Not surveyed"  = "grey50"
    ),
    breaks = c("Surveyed but no population", "Surveyed but no facility found", "Not surveyed"),
    labels = c("Surveyed but no population", "Surveyed but no facility found", "Not surveyed"),
    name   = NULL,   # 🟢 removed legend title only
    guide  = guide_legend(order = 2)
  ) +
  
  # --- Region borders on top ---
  geom_sf(
    data = riyadh_regions,
    fill = NA,
    color = "white",
    linewidth = 2
  ) +
  
  coord_sf(expand = FALSE) +
  
  # --- Theme (standard publication layout) ---
  theme_void(base_size = 14, base_family = "Arial") +
  theme(
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    
    legend.position  = "right",
    legend.direction = "vertical",
    legend.box       = "vertical",
    
    legend.title = element_text(face = "bold", size = 16, hjust = 0.5),
    legend.text  = element_text(size = 14)
  ) +
  labs(title = NULL, fill = NULL)

# --- Display ---
p


# 3) Plot private dental facilities per population by district

# --- FONT FIX: Load the 'ragg' library. It is better at handling fonts. ---
# You may need to run install.packages("ragg") in your console once.
library(ragg)

# --- Ensure all required libraries are loaded ---
library(sf)
library(dplyr)
library(ggplot2)
library(ggnewscale) # For multiple color/fill scales
library(ggtext) # For markdown in legend
library(viridis) # For Mako palette

# --- Plot 3: Private dental facilities per 10,000 population ---
ggplot() +
  # --- 1) VALID districts: numeric fill (clinics_per_10000) ---
  geom_sf(
    data = riyadh_counted_3 %>% filter(data_status == "Valid"),
    aes(fill = clinics_per_10000),
    color = "white", # <-- OPTIMIZATION: White borders are cleaner
    linewidth = 0.3 # Thinner white line
  ) +
  # --- OPTIMIZATION: Switched to Mako (Viridis) palette ---
  scale_fill_viridis_c(
    option = "mako", # Mako is a pro-level sequential palette
    direction = -1, # Reverses it so yellow = high, dark blue = low
    name = "Dental facilities\nper 10,000\n Population", # <-- OPTIMIZATION: Shortened title
    breaks = scales::pretty_breaks(n = 5), # Use pretty breaks for this scale
    guide = guide_colorbar(
      order = 1,
      # --- OPTIMIZATION: Tall, thin vertical bar ---
      barwidth = unit(1.5, "lines"),
      barheight = unit(20, "lines"),
      title.position = "top",
      title.hjust = 0.5
    )
  ) +
  
  # --- Switch to a new fill scale for categorical status ---
  ggnewscale::new_scale_fill() +
  
  # --- 2) NON-VALID: overlay 'No population' districts in grey ---
  geom_sf(
    data = riyadh_counted_3 %>% filter(data_status == "Surveyed but no population"),
    aes(fill = data_status),
    color = "white", # <-- OPTIMIZATION: White borders
    linewidth = 0.3
  ) +
  # 2) Not surveyed districts
  geom_sf(
    data = riyadh_counted_3 %>% filter(data_status == "Not surveyed"),
    aes(fill = "Not surveyed"),
    color = "white",
    linewidth = 0.3
  ) +
  scale_fill_manual(
    values = c("Surveyed but no population" = "grey90",
               "Not surveyed" = "grey80"
               ),
    breaks = c("Surveyed but no population", "Not surveyed"),
    labels = c("Surveyed but no population", "Not surveyed"),
    name   = NULL, # Removed "Status" title
    guide = guide_legend(
      order = 2, 
      override.aes = list(linewidth = 0.1)
    )
  ) +
  
  # --- District labels & title removed ---
  
  coord_sf(expand = FALSE) +
  
  # --- PROFESSIONAL PLOT FIX: Use theme_void and set a specific font ---
  theme_void(
    base_size = 14,
    base_family = "Arial" # <-- Explicitly set the font family here
  ) +
  theme(
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    
    # --- AESTHETIC TWEAK: Move legend to right for vertical stack ---
    legend.position = "right",
    legend.direction = "vertical",
    legend.box = "vertical",
    legend.title = element_text(face = "bold", size = 16, hjust = 0.5), # <-- OPTIMIZATION: Proportional font
    legend.text  = element_text(size = 14) # <-- OPTIMIZATION: Proportional font
  ) +
  
  # --- PROFESSIONAL PLOT FIX: Remove title (put in caption) ---
  labs(
    title = NULL,
    fill  = NULL # Titles are set in the scales
  )


################################################################
################################################################
# Section 11: Comparison of number of dental facilities, number 
# per 10,000, and their proportion out of all private facilities
# between regions
################################################################
################################################################

riyadh_counted_4 <- riyadh_counted_2 %>%
  mutate(
    data_status = case_when(
      done == "No" ~ "Not surveyed",
      is.na(population) | population == 0 ~ "Surveyed but no population",
      n_private_total == 0 ~ "Surveyed but no facility found",
      TRUE ~ "Valid"
    )
  )

riyadh_counted_4$data_status <- factor(
  riyadh_counted_4$data_status,
  levels = c(
    "Valid",
    "Surveyed but no facility found",
    "Surveyed but no population",
    "Not surveyed"
  )
)

# Calculate the mean number of dental per district for each region
mean_private_dental_by_region <- riyadh_counted_4 %>%
  filter(data_status!="Surveyed but no population" | data_status!="Not surveyed") %>% 
  group_by(region) %>%
  summarise(
    mean_n_private_dental = mean(n_private_dental, na.rm = TRUE)
  )

# Print the resulting table
print(mean_private_dental_by_region)

# Calculate the mean of pct_private_dental for each region, excluding NA/NaN values
mean_pct_by_region <- riyadh_counted_4 %>%
  filter(data_status!="Surveyed but no population" | data_status!="Surveyed but no facility found" | data_status!="Not surveyed") %>% 
  group_by(region) %>%
  summarise(
    mean_pct_private_dental = mean(pct_private_dental, na.rm = TRUE)
  )

# Print the resulting table
print(mean_pct_by_region)



# Mean of clinics per 10,000 population for each region
mean_by_region <- riyadh_counted_4 %>%
  filter(data_status!="Surveyed but no population" | data_status!="Not surveyed") %>% 
  group_by(region) %>%
  summarise(
    mean_clinics_per_10000 = mean(clinics_per_10000, na.rm = TRUE)
  )

# Print the resulting table
print(mean_by_region)



# --- Libraries ---
library(sf)
library(dplyr)
library(ggplot2)
library(viridis)
library(patchwork)
library(ragg)

# Ensure region is a factor with stable levels
riyadh_counted_4 <- riyadh_counted_4 %>%
  mutate(region = factor(region))
region_levels <- levels(riyadh_counted_4$region)

# -----------------------------
# 1) Prep datasets per outcome
# -----------------------------

# A) Number of private dental facilities per district
df_n <- riyadh_counted_4 %>%
  filter(!data_status %in% c("Surveyed but no population", "Not surveyed")) %>%
  filter(!is.na(n_private_dental)) %>%
  mutate(region = factor(region, levels = region_levels))

# B) Private dental facilities per 10,000 population
df_clinics <- riyadh_counted_4 %>%
  filter(!data_status %in% c("Surveyed but no population", "Not surveyed")) %>%
  filter(!is.na(clinics_per_10000)) %>%
  mutate(region = factor(region, levels = region_levels))

# C) % of private facilities with dental services
df_pct <- riyadh_counted_4 %>%
  filter(!data_status %in% c("Surveyed but no population",
                             "Surveyed but no facility found",
                             "Not surveyed")) %>%
  filter(!is.na(pct_private_dental)) %>%
  mutate(region = factor(region, levels = region_levels))

# -----------------------------
# 2) n per region for x-axis labels
# -----------------------------

n_n <- df_n %>%
  st_drop_geometry() %>%
  count(region, name = "n")

n_clinics <- df_clinics %>%
  st_drop_geometry() %>%
  count(region, name = "n")

n_pct <- df_pct %>%
  st_drop_geometry() %>%
  count(region, name = "n")

make_x_labels <- function(x, n_tbl) {
  x_chr <- as.character(x)
  idx   <- match(x_chr, as.character(n_tbl$region))
  n_val <- n_tbl$n[idx]
  paste0(x_chr, "\n(n=", n_val, ")")
}

# -----------------------------
# 3) Common theme and colours
# -----------------------------

fill_scale  <- scale_fill_viridis_d(option = "Turbo", begin = 0.1, end = 0.9, name = "Region")
color_scale <- scale_color_viridis_d(option = "Turbo", begin = 0.1, end = 0.9, guide = "none")

base_theme <- theme_minimal(base_size = 14) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    plot.margin        = margin(10, 10, 10, 10),
    axis.text.x        = element_text(size = 11, lineheight = 1.1, colour = "black"),
    text               = element_text(family = "Arial")
  )

# -----------------------------
# 4) Plot A – n private facilities per district
# -----------------------------

p1 <- ggplot(df_n, aes(x = region, y = n_private_dental, fill = region)) +
  geom_violin(trim = FALSE, alpha = 0.4) +
  geom_boxplot(width = 0.15, outlier.shape = NA, colour = "black", alpha = 1.5, size=1.4) +
  stat_summary(fun = mean,
               geom = "point",
               shape = 18, size = 6, stroke = 1.2, colour = "black") +
  geom_jitter(aes(color = region),
              width = 0.15, alpha = 0.5, size = 3) +
  fill_scale + color_scale +
  scale_x_discrete(labels = ~ make_x_labels(., n_n)) +
  labs(
    x = "Region",
    y = "Number of private dental\nfacilities per district",
    title = "A) Dental facility count per district"
  ) +
  base_theme

# -----------------------------
# 5) Plot B – clinics per 10,000 population
# -----------------------------

p2 <- ggplot(df_clinics, aes(x = region, y = clinics_per_10000, fill = region)) +
  geom_violin(trim = FALSE, alpha = 0.4) +
  geom_boxplot(width = 0.15, outlier.shape = NA, colour = "black", alpha = 1.5, size=1.4) +
  stat_summary(fun = mean,
               geom = "point",
               shape = 18, size = 6, stroke = 1.2, colour = "black") +
  geom_jitter(aes(color = region),
              width = 0.15, alpha = 0.5, size = 3) +
  fill_scale + color_scale +
  scale_x_discrete(labels = ~ make_x_labels(., n_clinics)) +
  labs(
    x = "Region",
    y = "Private dental facilities\nper 10,000 population",
    title = "B) Dental facilities per 10,000 population per district"
  ) +
  base_theme

# -----------------------------
# 6) Plot C – % with dental services
# -----------------------------

p3 <- ggplot(df_pct, aes(x = region, y = pct_private_dental, fill = region)) +
  geom_violin(trim = FALSE, alpha = 0.4) +
  geom_boxplot(width = 0.15, outlier.shape = NA, colour = "black", alpha = 1.5, size=1.4) +
  stat_summary(fun = mean,
               geom = "point",
               shape = 18, size = 6, stroke = 1.2, colour = "black") +
  geom_jitter(aes(color = region),
              width = 0.15, alpha = 0.5, size = 3) +
  fill_scale + color_scale +
  scale_x_discrete(labels = ~ make_x_labels(., n_pct)) +
  labs(
    x = "Region",
    y = "% of private facilities\nwith dental services",
    title = "C) Proportion of private facilities with dental services per district"
  ) +
  base_theme

# -----------------------------
# 7) Combine vertically
# -----------------------------

p1 <- p1 + labs(x = NULL)
p2 <- p2 + labs(x = NULL)

combined_vertical <- (p1 / p2 / p3) +
  plot_layout(guides = "collect") &
  theme(
    legend.position = "none",   # <-- put here, not nested
    
    # ---- Font sizes ----
    plot.title      = element_text(size = 30, face = "bold"),
    axis.title.x    = element_text(size = 28, face = "bold"),
    axis.title.y    = element_text(size = 28, face = "bold"),
    axis.text.x     = element_text(size = 26),
    axis.text.y     = element_text(size = 26)
  )

combined_vertical

