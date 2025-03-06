#Intall and import necessary packages
install.packages("librarian")
library(librarian)
shelf(tidyverse, tidycensus, tidyr, scales, mapview, knitr, emmeans, nlme, lmerTest, kableExtra, onewaytests, mediation, car, tigris, sf, broom, ggplot2, reshape2, viridis, readr, purrr)

setwd("~/Desktop/Srish/!MaCSS/Term 2/COMPSS224A")
census_api_key("63217a192c5803bfc72aab537fe4bf19f6058326", overwrite=TRUE, install = TRUE)

## Load in and clean the enrollment data
enroll_201416 <- read_delim("enr201416-v2.csv", delim="\t") #2014-2016 enrollment data by school
enroll_201719 <- read_delim("enr201719-v2.csv", delim="\t") #2017-2019 enrollment data by school
enroll_202022 <- read_delim("enr202022-v2.csv", delim="\t") #2020-2022 enrollment data by school

# List of enrollment datasets
enrollment <- list(enroll_201416, enroll_201719, enroll_202022)

# Apply filter to each dataset in the list
filtered_enrollment <- lapply(enrollment, function(data) {
  data %>% filter(DISTRICT == "Ravenswood City Elementary")
})

# Assign names to the filtered list (optional, for clarity)
names(filtered_enrollment) <- c("enroll_201416", "enroll_201719", "enroll_202022")

enroll_201416 <- filtered_enrollment$enroll_201416
enroll_201719 <- filtered_enrollment$enroll_201719
enroll_202022 <- filtered_enrollment$enroll_202022

unique(enroll_201416["SCHOOL"])
unique(enroll_201719["SCHOOL"])
unique(enroll_202022["SCHOOL"])

#the public schools we are tracking in Ravenswood City Elementary District
#belle haven elementary school (current)
#costano school of the arts (current)
#los robles ronald mcnair academy (current)
#cesar chavez ravenswood middle school (current)
#willow oaks (which closed in 2020, merged with belle haven)
#edison brentwood merged with costano in 2020
#costano elementary (merged)
#cesar chavez elementary closed/merged 2019

#for enroll_201416
#Los Robles Magnet Academy
#Belle Haven Elementary
#Ronald McNair Academy
#Costano Elementary
#Brentwood Academy
#Green Oaks Academy
#Cesar Chavez Elementary
#Willow Oaks Elementary
enroll_201416 <- enroll_201416 %>%
  filter(SCHOOL %in% c("Los Robles Magnet Academy", "Belle Haven Elementary",
                       "Ronald McNair Academy", "Costano Elementary", "Brentwood Academy", "Green Oaks Academy",
                       "Cesar Chavez Elementary", "Willow Oaks Elementary")) #filter the data frame just to our district for analysis 
glimpse(enroll_201416)

#for enroll_201719
#Los Robles Magnet Academy
#KIPP Valiant Community Prep
#Ravenswood Comprehensive Middle (now cesar chavez ravenwood in 2022)
#Belle Haven Elementary
#Ronald McNair Academy
#Costano Elementary
#Brentwood Academy
#Green Oaks Academy
#Cesar Chavez Elementary
#Willow Oaks Elementary
#Los Robles-Ronald McNair Academy
#Ravenswood Middle
enroll_201719 <- enroll_201719 %>%
  filter(SCHOOL %in% c("Los Robles Magnet Academy", "KIPP Valiant Community Prep",
                       "Belle Haven Elementary", "Ravenswood Comprehensive Middle",
                       "Ronald McNair Academy", "Costano Elementary", "Brentwood Academy", "Green Oaks Academy",
                       "Cesar Chavez Elementary", "Willow Oaks Elementary", "Los Robles-Ronald McNair Academy", "Ravenswood Middle")) #filter the data frame just to our district for analysis 
glimpse(enroll_201416)

#for enroll_202022
#District Office
#Nonpublic, Nonsectarian Schools
#Los Robles-Ronald McNair Academy
#Aspire East Palo Alto Charter
#KIPP Valiant Community Prep
#Cesar Chavez Ravenswood Middle
#Belle Haven Elementary
#Costano Elementary
enroll_202022 <- enroll_202022 %>%
  filter(SCHOOL %in% c("Los Robles-Ronald McNair Academy",
                       "KIPP Valiant Community Prep", "Cesar Chavez Ravenswood Middle", "Belle Haven Elementary",
                       "Costano Elementary")) #filter the data frame just to our district for analysis 
glimpse(enroll_202022)

# merge all enrollment data
enrollment_data <- bind_rows(enroll_201416, enroll_201719, enroll_202022)

glimpse(enrollment_data)

# make sure ACADEMIC_YEAR is numeric
enrollment_data <- enrollment_data %>%
  mutate(ACADEMIC_YEAR = as.numeric(substr(ACADEMIC_YEAR, 1, 4)))  

# check missing values
sum(is.na(enrollment_data$ENR_TOTAL)) 

#fix enrollment data to aggregate 
enrollment_data <- enrollment_data %>%
  group_by(SCHOOL, ACADEMIC_YEAR) %>%
  summarise(ENR_TOTAL = sum(ENR_TOTAL, na.rm = TRUE), .groups = "drop")

# View the result
head(enrollment_data)

#Import ACS tables

#define the years for our analysis
years <- 2018:2022 #years for analysis, policy stabilization (LCFF in 2013), closures in 2018

#geographic mobility in the past year B07003_001
acs_geomobility <- map_dfr(years, ~{
  get_acs(
    geography = "tract",
    variables = "B07003_001",  # Geographic Mobility in the Past Year
    state = "CA",
    county = "San Mateo",
    year = .x,
    survey = "acs5"
  ) %>%
    mutate(year = as.numeric(.x))  # Explicitly add the correct year
})

# Check results
head(acs_geomobility)

#Estimate total Subfamily Type by Presence of Own Children Under 18 Years
acs_famwkid <- map_dfr(years, ~{
  get_acs(
    geography = "tract",
    variables = "B11013_001",  # Subfamily Type by Presence of Own Children Under 18 Years
    state = "CA",
    county = "San Mateo",
    year = .x,
    survey = "acs5"
  ) %>%
    mutate(year = as.numeric(.x))  # Explicitly add the correct year
})

head(acs_famwkid)

#estimate total owner occupied (owner tenure) B25003_002
acs_ownerten <- map_dfr(years, ~{
  get_acs(
    geography = "tract",
    variables = "B25003_002",  #Estimate Total Owner Occupied
    state = "CA",
    county = "San Mateo",
    year = .x,
    survey = "acs5"
  )%>%
    mutate(year = as.numeric(.x))  # Explicitly add the correct year
})

head(acs_ownerten)

#estimate total renter occupied (renter tenure) B25003_003
acs_renterten <- map_dfr(years, ~{
  get_acs(
    geography = "tract",
    variables = "B25003_003",  #Estimate Total Renter Occupied
    state = "CA",
    county = "San Mateo",
    year = .x,
    survey = "acs5"
  ) %>%
    mutate(year = as.numeric(.x))  # Explicitly add the correct year
})

head(acs_renterten)

#estimate Median Family Income in the Past 12 Months (in 2022 Inflation-Adjusted Dollars) by Presence of Own Children Under 18 Years
#B19125_001 (acs_fammedinc)
acs_fammedinc <- map_dfr(years, ~{
  get_acs(
    geography = "tract",
    variables = "B19126_001",  #Median Family Income in the Past 12 Months (in 2022 Inflation-Adjusted Dollars) by Presence of Own Children Under 18 Years
    county = "San Mateo",
    state = "CA",
    year = .x,
    survey = "acs5"
  ) %>%
    mutate(year = as.numeric(.x))  # Explicitly add the correct year
})

head(acs_fammedinc)

#estimate Total Median Value by Year Structure Built
#B19125_001 (acs_homemedval)
acs_homemedval <- map_dfr(years, ~{
  get_acs(
    geography = "tract",
    variables = "B25107_001",  #Median Value by Year Structure Built
    county = "San Mateo",
    state = "CA",
    year = .x,
    survey = "acs5"
  ) %>%
    mutate(year = as.numeric(.x))  # Explicitly add the correct year
})

head(acs_homemedval)

#Retaining tracts which are under the Ravenswood School District
tracts_to_keep <- c("06081611700", "06081613900", "06081611800", 
                    "06081611900", "06081612000", "06081612100", 
                    "06081611901", "06081611902", "06081612001", 
                    "06081612002", "06081612101", "06081612102")

filter_acs_table <- function(df) {
  df %>% filter(GEOID %in% tracts_to_keep)
}

acs_tables <- list(acs_geomobility, acs_renterten, acs_fammedinc, acs_famwkid, acs_homemedval, acs_ownerten)
acs_filtered_tables <- map(acs_tables, filter_acs_table)
list2env(setNames(acs_filtered_tables, c("acs_geomobility", "acs_renterten", "acs_fammedinc", "acs_famwkid", "acs_homemedval", "acs_ownerten")), envir = .GlobalEnv)


## Load Financial (Funding) Data for Ravenswood School District
# Define function to clean and rename columns
clean_financial_data <- function(file_path, year) {
  df <- suppressMessages(read_csv(file_path, skip = 1, show_col_types = FALSE)) %>%
    dplyr::select(where(~ !all(is.na(.))))  # Remove empty columns if present
  
  # Rename columns with clear prefixes
  colnames(df) <- c("Object_Codes", "Type_of_Revenue", 
                    "Revenues_Unrestricted", "Revenues_Restricted", "Revenues_Total",
                    "Dollar_Per_Student_District_ADA", "Dollar_Per_Student_Pct_Avg_Elementary", 
                    "Dollar_Per_Student_State_Avg_Elem", "Dollar_Per_Student_State_Avg_All")
  
  # Identify the last row (Total Revenues) and rename first two columns as "Total"
  df[nrow(df), 1:2] <- "Total"
  
  # Add Year column (extract first 4 characters to keep the starting year)
  df <- df %>%
    mutate(Year = as.numeric(str_extract(year, "^\\d{4}")))
  
  return(df)  # Keep data in wide format with renamed columns
}

# List of CSV filenames corresponding to the years 2018-2022 only
file_names <- c(
  "Financial Data for Ravenswood City Elementary - 2014-15.csv",
  "Financial Data for Ravenswood City Elementary - 2015-16.csv",
  "Financial Data for Ravenswood City Elementary - 2016-17.csv",
  "Financial Data for Ravenswood City Elementary - 2017-18.csv",
  "Financial Data for Ravenswood City Elementary - 2018-19.csv",
  "Financial Data for Ravenswood City Elementary - 2019-20.csv",
  "Financial Data for Ravenswood City Elementary - 2020-21.csv",
  "Financial Data for Ravenswood City Elementary - 2021-22.csv",
  "Financial Data for Ravenswood City Elementary - 2022-23.csv"
)

# Corresponding years for filtering (extracting the first year)
years <- c("2014-15", "2015-16", "2016-17", "2017-18", "2018-19", "2019-20", "2020-21", "2021-22", "2022-23")

# Apply function to each file and store in a single dataframe
fin_data <- map2_dfr(file_names, years, clean_financial_data)

# Print last few rows to confirm "Total" row is retained and "Year" is in correct format
tail(fin_data)

#Summarize data
# Summarize count-based variables using SUM
acs_geomobility_summary <- acs_geomobility %>%
  filter(str_detect(NAME, "East Palo Alto|San Mateo")) %>%
  group_by(year) %>%
  summarise(total_geomobility = sum(estimate, na.rm = TRUE))

acs_ownerten_summary <- acs_ownerten %>%
  filter(str_detect(NAME, "East Palo Alto|San Mateo")) %>%
  group_by(year) %>%
  summarise(total_owner_tenure = sum(estimate, na.rm = TRUE))

acs_renterten_summary <- acs_renterten %>%
  filter(str_detect(NAME, "East Palo Alto|San Mateo")) %>%
  group_by(year) %>%
  summarise(total_renter_tenure = sum(estimate, na.rm = TRUE))

acs_famwkid_summary <- acs_famwkid %>%
  filter(str_detect(NAME, "East Palo Alto|San Mateo")) %>%
  group_by(year) %>%
  summarise(total_families_with_kids = sum(estimate, na.rm = TRUE))

# Summarize monetary variables using MEAN
acs_fammedinc_summary <- acs_fammedinc %>%
  filter(str_detect(NAME, "East Palo Alto|San Mateo")) %>%
  group_by(year) %>%
  summarise(avg_median_income = mean(estimate, na.rm = TRUE))

acs_homemedval_summary <- acs_homemedval %>%
  filter(str_detect(NAME, "East Palo Alto|San Mateo")) %>%
  group_by(year) %>%
  summarise(avg_median_home_value = mean(estimate, na.rm = TRUE))

#Exploratory Data Anlysis (EDA)
# Plot the enrollment trend
enrollment_summary <- enrollment_data %>%
  group_by(ACADEMIC_YEAR, SCHOOL) %>%
  summarise(total_enrollment = sum(ENR_TOTAL), .groups = "drop")

overall_enrollment <- enrollment_summary %>%
  group_by(ACADEMIC_YEAR) %>%
  summarise(total_enrollment = sum(total_enrollment), .groups = "drop") %>%
  mutate(SCHOOL = "Total")

plot_data <- bind_rows(enrollment_summary, overall_enrollment)

ggplot(plot_data, aes(x = as.factor(ACADEMIC_YEAR), y = total_enrollment, group = SCHOOL, color = SCHOOL)) +
  geom_line(size = 1) +  
  geom_point(size = 2) +
  geom_line(data = overall_enrollment, aes(x = as.factor(ACADEMIC_YEAR), y = total_enrollment, group = 1), 
            color = "red", size = 1.5) +  # Overlay total enrollment in red
  ggtitle("Enrollment Trend in Ravenswood City Elementary") +
  xlab("Academic Year") + 
  ylab("Total Enrollment") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

# Ensure all summary datasets contain the same years and are structured properly
acs_summary <- list(
  acs_geomobility_summary %>% rename(Value = total_geomobility) %>% mutate(Type = "Geographic Mobility"),
  acs_ownerten_summary %>% rename(Value = total_owner_tenure) %>% mutate(Type = "Owner Tenure"),
  acs_renterten_summary %>% rename(Value = total_renter_tenure) %>% mutate(Type = "Renter Tenure"),
  acs_fammedinc_summary %>% rename(Value = avg_median_income) %>% mutate(Type = "Median Family Income"),
  acs_famwkid_summary %>% rename(Value = total_families_with_kids) %>% mutate(Type = "Families with Kids"),
  acs_homemedval_summary %>% rename(Value = avg_median_home_value) %>% mutate(Type = "Median Home Value")
)

# Combine all summaries into one dataframe
acs_summary_df <- bind_rows(acs_summary)

# Combine count-based variables into a long format
acs_count_vars <- list(
  acs_ownerten_summary %>% rename(Value = total_owner_tenure) %>% mutate(Type = "Owner Tenure"),
  acs_renterten_summary %>% rename(Value = total_renter_tenure) %>% mutate(Type = "Renter Tenure"),
  acs_famwkid_summary %>% rename(Value = total_families_with_kids) %>% mutate(Type = "Families with Kids")
)

acs_count_long <- bind_rows(acs_count_vars) %>%
  rename(Year = year) %>%
  filter(Year >= 2018 & Year <= 2022)

# Plot count variables
ggplot(acs_count_long, aes(x = Year, y = Value, color = Type, group = Type)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Trends in Tenure, and Families with Kids (2018-2022)",
    x = "Year",
    y = "Count",
    color = "Variable"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5)
  )

# Combine monetary variables into a long format
acs_money_vars <- list(
  acs_fammedinc_summary %>% rename(Value = avg_median_income) %>% mutate(Type = "Median Family Income"),
  acs_homemedval_summary %>% rename(Value = avg_median_home_value) %>% mutate(Type = "Median Home Value")
)

acs_money_long <- bind_rows(acs_money_vars) %>%
  rename(Year = year) %>%
  filter(Year >= 2018 & Year <= 2022)

acs_money_long <- acs_money_long %>%
  mutate(Type = as.factor(Type))

# Plot monetary variables
ggplot(acs_money_long, aes(x = Year, y = Value, color = Type, group = Type)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  scale_y_log10(labels = scales::dollar) +  # Apply log scale
  labs(
    title = "Median Family Income and Median Home Value (2018-2022)",
    x = "Year",
    y = "Log Scale of Dollar Amount ($)",
    color = "Variable"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5)
  )

# Filter only the 'Total' row for each year
total_revenues <- fin_data %>%
  filter(Object_Codes == "Total") %>%
  mutate(Revenues_Total = as.numeric(gsub("[$,]", "", Revenues_Total)))  # Convert to numeric

# Plot total revenue by year with proper y-axis formatting
ggplot(total_revenues, aes(x = Year, y = Revenues_Total, group = 1)) +
  geom_line(color = "#00BFC4", size = 1) +
  geom_point(color = "#F8766D", size = 2) +
  scale_y_continuous(labels = scales::comma) +  # Format y-axis with commas
  labs(title = "Total Revenues Over Time",
       x = "Year",
       y = "Total Revenue ($)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for readability

# Filter only the 'LCFF Sources' row for each year
lcff_revenues <- fin_data %>%
  filter(Type_of_Revenue == "LCFF Sources") %>%
  mutate(Revenues_Unrestricted = as.numeric(gsub("[$,]", "", Revenues_Unrestricted)),
         Revenues_Restricted = as.numeric(gsub("[$,]", "", Revenues_Restricted)),
         Revenues_Total = as.numeric(gsub("[$,]", "", Revenues_Total)))  # Convert to numeric

# Plot LCFF Total Revenue by Year
ggplot(lcff_revenues, aes(x = Year, y = Revenues_Total, group = 1)) +
  geom_line(color = "#00BFC4", size = 1) +
  geom_point(color = "#F8766D", size = 2) +
  scale_y_continuous(labels = scales::comma) +  # Format y-axis with commas
  labs(title = "LCFF Revenues Over Time",
       x = "Year",
       y = "LCFF Revenue ($)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for readability

district_tracts <- c("06081611800", "06081611900", "06081611901", "06081611902",
                     "06081612000", "06081612001", "06081612002",
                     "06081612100", "06081612101", "06081612102", "06081611700", "06081613900"
)

tracts_sf <- tigris::tracts(state = "CA", county = "San Mateo", year = 2022, class = "sf")

# Filter for the district tracts
filtered_tracts <- tracts_sf %>%
  filter(GEOID %in% district_tracts)

# Plot the static map
ggplot() +
  geom_sf(data = filtered_tracts, aes(fill = GEOID), color = "black", size = 0.3) +  # Census tracts with outlines
  geom_sf_text(data = filtered_tracts, aes(label = GEOID), size = 3) +  # Label tracts with GEOID
  labs(title = "Census Tracts in Ravenswood City Elementary Area",
       subtitle = "San Mateo County, CA",
       fill = "Census Tract") +
  theme_minimal()

#37.47398933393889, -122.145722017199 (Cesar Chavez Elementary School, ) A
#37.46259324964851, -122.13108515092696 Los Robles-Ronald McNair Academy and Los Robles Magnet Academy and Ronald McNair Academy B
#37.462199995618064, -122.1343507220907 Brentwood Academy C
#37.47277945482856, -122.14607955648873 Cesar Chavez Ravenswood Middle D
#37.462738130210504, -122.1576352220907 KIPP Valiant Community Prep E
#37.47717434524049, -122.16123826441813 Belle Haven Elementary School F
#37.4778444722474, -122.13867439043322 Costano Elementary School G
#37.4736913517679, -122.14561769896501 Green Oaks Academy H
#37.46255795948895, -122.1577271407187 Willow Oaks Elementary I
#37.47277945482856, -122.14607955648873 Ravenswood Comprehensive Middle J
#37.47277945482856, -122.14607955648873 Ravenswood Middle K

#dataframe for long and lat
long_lat <- data.frame(
  longitude = c(-122.145722017199, -122.13108515092696, -122.1343507220907, -122.14607955648873, -122.1576352220907, -122.16123826441813,-122.13867439043322, 
                -122.14561769896501, -122.1577271407187, -122.14607955648873, -122.14607955648873),
  latitude = c(37.47398933393889, 37.46259324964851, 37.462199995618064, 37.47277945482856, 37.462738130210504, 37.47717434524049, 37.4778444722474,
               37.4736913517679, 37.46255795948895, 37.47277945482856, 37.47277945482856),
  label = c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K")
)

# Convert points to an sf object
points_sf <- st_as_sf(long_lat, coords = c("longitude", "latitude"), crs = 4326)

ggplot() +
  # Plot census tracts
  geom_sf(data = filtered_tracts, aes(fill = GEOID), color = "black", size = 0.3, alpha = 0.5) +
  # Plot points
  geom_sf(data = points_sf, aes(color = label), size = 3) +
  # Add text labels for points
  geom_sf_text(data = points_sf, aes(label = label), size = 4, fontface = "bold", nudge_y = 0.001) +
  # Labels and theme
  labs(title = "Census Tracts with School Locations",
       subtitle = "Ravenswood City Elementary School District",
       fill = "Census Tract GEOID",
       color = "School Point Label") +
  theme_minimal()

#set to wgs84 crs
filtered_tracts <- st_transform(filtered_tracts, crs = 4326)  #tracts to wgs84
points_sf <- st_transform(points_sf, crs = 4326)  #points to same crs

#spatial join, match each point (school loc) to the corresponding tract
points_with_tracts <- st_join(points_sf, filtered_tracts, left = TRUE)

# Select relevant columns (keeping only label and GEOID)
points_with_tracts <- points_with_tracts %>%
  select(label, GEOID)

# View results
points_table <- as.data.frame(points_with_tracts) %>%
  dplyr::select(label, GEOID)

points_table <- points_table %>%
  mutate(label = case_when(
    label == "A" ~ "Cesar Chavez Elementary",
    label == "B" ~ "Los Robles-Ronald McNair Academy", #and Ronald McNair Academy and Los Robles Magnet Academy
    label == "C" ~ "Brentwood Academy",
    label == "D" ~ "Cesar Chavez Elementary",
    label == "E" ~ "KIPP Valiant Community Prep",
    label == "F" ~ "Belle Haven Elementary",
    label == "G" ~ "Costano Elementary",
    label == "H" ~ "Green Oaks Academy",
    label == "I" ~ "Willow Oaks Elementary",
    label == "J" ~ "Ravenswood Comprehensive Middle",
    label == "K" ~ "Ravenswood Middle",
    TRUE ~ label 
  ))

new_rows <- data.frame(label = c("Ronald McNair Academy", "Los Robles Magnet Academy", "Cesar Chavez Ravenswood Middle"),
                       GEOID = c('06081611901', '06081611901', '06081611800'))

# Append new rows
points_table <- bind_rows(points_table, new_rows)

points_table <- points_table %>% 
  rename(
    SCHOOL = label
  )

enrollment_data <- enrollment_data %>%
  filter(SCHOOL %in% c("Cesar Chavez Elementary","Brentwood Academy", "Belle Haven Elementary", "Green Oaks Academy", "Ravenswood Comprehensive Middle", "Ronald McNair Academy",
                       "Cesar Chavez Ravenswood Middle", "Los Robles-Ronald McNair Academy", "KIPP Valiant Community Prep", "Costano Elementary", "Willow Oaks Elementary", "Ravenswood Middle", "Los Robles Magnet Academy"))

head(enrollment_data)

enrollment_data_grouped <- enrollment_data %>%
  group_by(SCHOOL, ACADEMIC_YEAR)

# make sure ACADEMIC_YEAR is numeric
enrollment_data_grouped <- enrollment_data_grouped %>%
  mutate(ACADEMIC_YEAR = as.numeric(substr(ACADEMIC_YEAR, 1, 4)))  

enrollment_data_grouped <- enrollment_data_grouped %>%
  filter(as.numeric(substr(ACADEMIC_YEAR, 1, 4)) >= 2018)

head(enrollment_data_grouped)

# merge GEOID from points_table into enrollment_data based on SCHOOL
enrollment_data_merged <- enrollment_data_grouped %>%
  left_join(points_table, by = "SCHOOL", relationship = "many-to-many")

#mutate(GEOID = as.character(GEOID))
head(enrollment_data_merged)

missing_geoids <- enrollment_data_merged %>%
  filter(is.na(GEOID))

missing_geoids  # These schools did not get a GEOID

#function to rename estimate column based on acs variable in the table
rename_acs <- function(df) {
  df %>%
    rename(!!paste0(unique(df$variable), "_estimate") := estimate) %>%
    dplyr::select(-variable, -moe)  # Remove redundant columns if not needed
}

#apply the renaming function to all acs tables
acs_tables <- map(acs_tables, rename_acs)

#combine acs tables
acs_combined_1 <- reduce(acs_tables, full_join, by = c("GEOID", "NAME", "year"))

#rename column so it matches enrollment_data tables
acs_combined_1 <- acs_combined_1 %>%
  rename(ACADEMIC_YEAR = year)

head(acs_combined_1)
#check data types before merging
str(enrollment_data_merged$GEOID)
str(acs_combined_1$GEOID)
str(enrollment_data_merged$ACADEMIC_YEAR)
str(acs_combined_1$ACADEMIC_YEAR)

#[1] "06081611901" "06081613900" "06081611800" "06081611700"

acs_combined_1 <- acs_combined_1 %>%
  filter(GEOID %in% c("06081611901", "06081613900", "06081611800", "06081611700", "06081611900"))

#verify that only the geoids with schools are in the data
unique(acs_combined_1$GEOID)

#check num pairs in each df
common_pairs <- inner_join(enrollment_data_merged, acs_combined_1, by = c("GEOID", "ACADEMIC_YEAR"))
nrow(common_pairs)

enrollment_acs_1 <- enrollment_data_merged %>%
  left_join(acs_combined_1, by = c("GEOID", "ACADEMIC_YEAR"))

summary(enrollment_acs_1)  #check for na values
colnames(enrollment_acs_1)  #confirm that ACS variable columns were added

head(enrollment_acs_1)
tail(enrollment_acs_1)

missing_acs_schools <- enrollment_acs_1 %>%
  filter(if_any(starts_with("B"), is.na)) %>%  #check the ACS variable columns for NA values
  select(SCHOOL, ACADEMIC_YEAR, GEOID)

print(unique(missing_acs_schools$ACADEMIC_YEAR))
print(unique(missing_acs_schools$SCHOOL))
print(unique(missing_acs_schools$GEOID))

#fix GEOID for the specific schools and years that had their census tract changed in 2020 (06081611900 became split and they are now in 06081611901)
enrollment_data_merged <- enrollment_data_merged %>%
  mutate(GEOID = case_when(
    SCHOOL %in% c("Los Robles Magnet Academy", "Brentwood Academy", "Los Robles-Ronald McNair Academy") &
      ACADEMIC_YEAR %in% c(2018, 2019) ~ "06081611900",  #assign correct GEOID
    
    TRUE ~ GEOID  #keep all other GEOIDs unchanged
  ))

#check that changes worked
updated_rows <- enrollment_data_merged %>%
  filter(SCHOOL %in% c("Los Robles Magnet Academy", "Brentwood Academy", "Los Robles-Ronald McNair Academy"),
         ACADEMIC_YEAR %in% c(2018, 2019))

print(updated_rows)

enrollment_acs_1 <- enrollment_data_merged %>%
  left_join(acs_combined_1, by = c("GEOID", "ACADEMIC_YEAR"))

summary(enrollment_acs_1)  #check for na values
colnames(enrollment_acs_1)  #confirm that ACS variable columns were added

head(enrollment_acs_1)

#Merging Financial Data and wnrollment_acs_1
#check data types
str(fin_data$Year)
str(enrollment_acs_1$ACADEMIC_YEAR)

fin_data$ACADEMIC_YEAR <- fin_data$Year
str(fin_data$ACADEMIC_YEAR)

enrollment_acs_fin_1 <- enrollment_acs_1 %>%
  left_join(fin_data, by = "ACADEMIC_YEAR", relationship="many-to-many")

head(enrollment_acs_fin_1)

enrollment_acs_fin <- enrollment_acs_fin_1 %>%
  filter(Type_of_Revenue %in% c("LCFF Sources", "State Aid/Principal Apportionment", "Local Property Taxes/Misc. Funds/LCFF Transfers", "Total"))

head(enrollment_acs_fin)

# Convert financial columns to numeric safely by removing $, %, and commas
enrollment_acs_fin <- enrollment_acs_fin %>%
  mutate(across(c(Revenues_Unrestricted, Revenues_Restricted, Revenues_Total, 
                  Dollar_Per_Student_District_ADA, Dollar_Per_Student_Pct_Avg_Elementary, 
                  Dollar_Per_Student_State_Avg_Elem, Dollar_Per_Student_State_Avg_All), 
                ~ as.numeric(gsub("[^0-9.-]", "", .)),  # Remove $, %, and commas
                .names = "clean_{.col}"))  # Store cleaned values in new columns

colnames(enrollment_acs_fin)

# Summarizing financial data by Type_of_Revenue
financial_summary <- enrollment_acs_fin %>%
  group_by(Type_of_Revenue) %>%
  summarise(
    avg_clean_Revenues_Total = mean(clean_Revenues_Total, na.rm = TRUE),
    avg_clean_Revenues_Unrestricted = mean(clean_Revenues_Unrestricted, na.rm = TRUE),
    avg_clean_Revenues_Restricted = mean(clean_Revenues_Restricted, na.rm = TRUE),
    avg_clean_Dollar_Per_Student_District_ADA = mean(clean_Dollar_Per_Student_District_ADA, na.rm = TRUE),
    avg_clean_Dollar_Per_Student_State_Avg_All = mean(clean_Dollar_Per_Student_State_Avg_All, na.rm = TRUE)
  )

# View the summary
print(financial_summary)

# Compare Ravenswood vs. State Averages
ggplot(financial_summary, aes(x = Type_of_Revenue)) +
  geom_col(aes(y = avg_clean_Dollar_Per_Student_District_ADA, fill = "Ravenswood District"), position = "dodge") +
  geom_col(aes(y = avg_clean_Dollar_Per_Student_State_Avg_All, fill = "State Average"), position = "dodge") +
  labs(title = "Comparison of Per-Student Revenue: Ravenswood vs. State",
       x = "Type of Revenue",
       y = "Dollar Per Student",
       fill = "Source") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(labels = scales::comma)

revenue_trend <- enrollment_acs_fin %>%
  ungroup() %>%  # Ensure no grouping
  group_by(ACADEMIC_YEAR) %>%  # Group by year
  summarise(
    avg_per_student_ravenswood = mean(clean_Dollar_Per_Student_District_ADA, na.rm = TRUE),
    avg_per_student_state = mean(clean_Dollar_Per_Student_State_Avg_All, na.rm = TRUE)
  ) %>%
  pivot_longer(cols = c(avg_per_student_ravenswood, avg_per_student_state),
               names_to = "Revenue_Source",
               values_to = "Per_Student_Revenue")

# Rename factor labels for readability
revenue_trend <- revenue_trend %>%
  mutate(Revenue_Source = case_when(
    Revenue_Source == "avg_per_student_ravenswood" ~ "Ravenswood District",
    Revenue_Source == "avg_per_student_state" ~ "State Average"
  ))

# Plot the trend over time
ggplot(revenue_trend, aes(x = as.factor(ACADEMIC_YEAR), y = Per_Student_Revenue, group = Revenue_Source, color = Revenue_Source)) +
  geom_line(size = 1.5) +
  geom_point(size = 3) +
  scale_y_continuous(labels = scales::dollar) +  # Format Y-axis as currency
  labs(
    title = "Revenue Per Student Over Time: Ravenswood vs. State Average",
    x = "Academic Year",
    y = "Per Student Revenue ($)",
    color = "Source"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for readability

#Correlation Analysis
# Select relevant columns
analysis_data <- enrollment_acs_fin %>%
  dplyr::select(
    SCHOOL, 
    ACADEMIC_YEAR, 
    ENR_TOTAL, 
    GEOID, 
    NAME, 
    starts_with("B"),  # Keep all ACS estimate columns
    Type_of_Revenue, 
    clean_Revenues_Total, 
    clean_Dollar_Per_Student_District_ADA  # Revenue per student
  )

# View the structure to confirm
glimpse(analysis_data)

# Define a named vector with readable labels
variable_labels <- c(
  "ENR_TOTAL" = "Total Enrollment",
  "clean_Revenues_Total" = "Total Revenue",
  "clean_Dollar_Per_Student_District_ADA" = "Revenue per Student",
  "B07003_001_estimate" = "Geographic Mobility",
  "B25003_003_estimate" = "Renter Households",
  "B19125_001_estimate" = "Median Family Income",
  "B11013_001_estimate" = "Families with Kids",
  "B25107_001_estimate" = "Median Home Value",
  "B25003_002_estimate" = "Owner Households"
)

# Apply renaming only to existing columns
analysis_data_renamed <- analysis_data %>%
  rename_with(~ variable_labels[.x], .cols = names(variable_labels)[names(variable_labels) %in% names(analysis_data)])

# Select only numeric columns
numeric_data <- analysis_data_renamed %>%
  ungroup() %>%
  dplyr::select(where(is.numeric)) %>%
  drop_na()  # Remove missing values

# Compute correlation matrix
cor_matrix <- cor(numeric_data, use = "complete.obs")
cor_matrix %>%
  round(2) %>%
  kable("html", caption = "Correlation Matrix of Numeric Variables") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

cor_long <- as.data.frame(as.table(cor_matrix)) %>%
  rename(Variable1 = Var1, Variable2 = Var2, Correlation = Freq)

ggplot(cor_long, aes(x = Variable1, y = Variable2, fill = Correlation)) +
  geom_tile(color = "white") +  # White grid lines
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1, 1), space = "Lab",
                       name = "Correlation") +  # Color scale
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid = element_blank()) +
  ggtitle("Correlation Heatmap of Numeric Variables")

unique(analysis_data$GEOID)

## ANOVA
# Hypothesis 1
analysis_data_anova <- analysis_data %>%
  ungroup() %>%
  mutate(ACADEMIC_YEAR = as.factor(ACADEMIC_YEAR))  # Convert year to categorical factor

# Re-run the model with p-values
mixed_model_p <- lmer(B07003_001_estimate ~ ACADEMIC_YEAR + (1 | GEOID), data = analysis_data)
summary(mixed_model_p)

mixed_model_fam <- lmer(B11013_001_estimate ~ ACADEMIC_YEAR + (1 | GEOID), data = analysis_data)
summary(mixed_model_fam)

#Plotting count of families with children 
# Ensure GEOID is a character for merging
analysis_data$GEOID <- as.character(analysis_data$GEOID)
filtered_tracts$GEOID <- as.character(filtered_tracts$GEOID)

# Compute the change in families with kids (difference between first and last year for each tract)
family_change <- analysis_data %>%
  group_by(GEOID) %>%
  summarize(family_change = last(B11013_001_estimate) - first(B11013_001_estimate))

# Merge with spatial data
tracts_families_change <- left_join(filtered_tracts, family_change, by = "GEOID")

ggplot(tracts_families_change) +
  geom_sf(aes(fill = family_change), color = "black", size = 0.3) +  # Color by change in family count
  scale_fill_viridis_c(option = "magma", name = "Change in Families") +  # Use color scale
  labs(
    title = "Change in Families with Kids by Census Tract",
    subtitle = "Ravenswood School District, East Palo Alto (2018-2022)",
    fill = "Family Count Change"
  ) +
  theme_minimal()

#Hypothesis 2
mixed_model_enrollment <- lmer(ENR_TOTAL ~ ACADEMIC_YEAR + (1 | SCHOOL), data = analysis_data)
summary(mixed_model_enrollment)

enrollment_change <- analysis_data %>%
  group_by(SCHOOL) %>%
  summarize(enrollment_change = last(ENR_TOTAL) - first(ENR_TOTAL))

print(enrollment_change)

#Removing schools that were closed or had 0 enrollment in any of the years
# Define the schools that closed
closed_schools <- c("Brentwood Academy", "Willow Oaks Elementary")

# Remove them from the dataset
filtered_data <- analysis_data %>%
  filter(!SCHOOL %in% closed_schools)

mixed_model_enrollment_filtered <- lmer(ENR_TOTAL ~ ACADEMIC_YEAR + (1 | SCHOOL), data = filtered_data)
summary(mixed_model_enrollment_filtered)

enrollment_change_filtered <- filtered_data %>%
  group_by(SCHOOL) %>%
  summarize(enrollment_change = last(ENR_TOTAL) - first(ENR_TOTAL))

enrollment_pct_change <- filtered_data %>%
  group_by(SCHOOL) %>%
  summarize(
    start_enrollment = first(ENR_TOTAL),
    end_enrollment = last(ENR_TOTAL),
    pct_change = ((end_enrollment - start_enrollment) / start_enrollment) * 100
  )

print(enrollment_pct_change)


enrollment_trend <- filtered_data %>%
  group_by(SCHOOL) %>%
  summarize(
    start_year = min(ACADEMIC_YEAR),
    end_year = max(ACADEMIC_YEAR),
    start_enrollment = first(ENR_TOTAL),
    end_enrollment = last(ENR_TOTAL),
    years_active = end_year - start_year,
    annual_change = (end_enrollment - start_enrollment) / years_active  # Change per year
  )

print(enrollment_trend)

# Home values and LCFF funding
mixed_model_home_values <- lmer(B25107_001_estimate ~ ACADEMIC_YEAR + (1 | GEOID), data = analysis_data)
summary(mixed_model_home_values)

lcff_data <- analysis_data %>%
  filter(Type_of_Revenue == "LCFF Sources")  # Keep only LCFF funding

# Run the LMM for LCFF funding over time
repeated_lcff <- lmer(clean_Revenues_Total ~ ACADEMIC_YEAR + (1 | SCHOOL), data = lcff_data)

# View the model summary
summary(repeated_lcff)

## Home Values effect on funding

#Spearman's Ran Correlation (Non-Parametric)
#the data is too sparse for regression, but we still want to check for a relationship, so
# Spearman’s correlation can measure whether home values and LCFF funding move together (monotonic relationship).
# This is better than Pearson correlation when dealing with small sample sizes and nonlinear trends.

cor.test(analysis_data$B25107_001_estimate, analysis_data$clean_Revenues_Total, method = "spearman")

# Normalize values separately to prevent artifacts
analysis_data_scaled <- analysis_data %>%
  group_by(ACADEMIC_YEAR) %>%
  summarise(
    Home_Values_Avg = mean(B25107_001_estimate, na.rm = TRUE),
    LCFF_Funding_Avg = mean(clean_Revenues_Total, na.rm = TRUE)
  ) %>%
  mutate(
    Home_Values_Scaled = (Home_Values_Avg - min(Home_Values_Avg, na.rm = TRUE)) / 
      (max(Home_Values_Avg, na.rm = TRUE) - min(Home_Values_Avg, na.rm = TRUE)),
    
    LCFF_Funding_Scaled = (LCFF_Funding_Avg - min(LCFF_Funding_Avg, na.rm = TRUE)) / 
      (max(LCFF_Funding_Avg, na.rm = TRUE) - min(LCFF_Funding_Avg, na.rm = TRUE))
  ) %>%
  dplyr::select(ACADEMIC_YEAR, Home_Values_Scaled, LCFF_Funding_Scaled) %>%
  pivot_longer(cols = c(Home_Values_Scaled, LCFF_Funding_Scaled), 
               names_to = "Variable", 
               values_to = "Value")

# Re-plot with proper scaling
ggplot(analysis_data_scaled, aes(x = as.factor(ACADEMIC_YEAR), y = Value, color = Variable, group = Variable)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_color_manual(values = c("blue", "red"), labels = c("Home Values", "LCFF Funding")) +
  labs(
    title = "Home Values vs. LCFF Funding Over Time",
    x = "Academic Year",
    y = "Scaled Values (0-1)",
    color = "Variable"
  ) +
  theme_minimal()

# Create Enrollment Categories (Tertiles)
analysis_data <- analysis_data %>%
  mutate(Enrollment_Category = ntile(ENR_TOTAL, 3)) %>%
  mutate(Enrollment_Category = case_when(
    Enrollment_Category == 1 ~ "Low",
    Enrollment_Category == 2 ~ "Medium",
    Enrollment_Category == 3 ~ "High"
  ))

# Check distribution
table(analysis_data$Enrollment_Category)

#To control for differences between Tracts, we include GEOID as a factor in the model.
anova_enrollment_families <- aov(B11013_001_estimate ~ Enrollment_Category + as.factor(GEOID), data = analysis_data)
summary(anova_enrollment_families)
