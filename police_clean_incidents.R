
# Get incidents near library
# Look at parking violations near Richmond Street


# El Cerrito Police Incident Data
# Start: August 5, 2025

setwd("D:/Documents/Employment/2025 job search/Project 2025/el-cerrito-police-report/data/combined/")

# Load necessary libraries
library(readr)       # for reading CSV files
library(dplyr)       # for data wrangling
library(stringr)     # for string manipulation
library(lubridate)   # for date-time parsing

# Read data
df <- read_csv("el-cerrito-police-incidents.csv")  # update if you already loaded this earlier

# Standardize column names
df <- df %>%
  rename_with(~ str_trim(.) %>% 
                str_replace_all(" ", "_") %>% 
                tolower())

nrow(df) # 175495

# 🔁 Remove completely duplicated rows
df <- df %>%
  distinct()  # Keeps only one copy of exact duplicates

nrow(df) # 142986

# ❌ Remove rows where event_number is missing
df <- df %>%
  filter(!is.na(event_number))
nrow(df) # 137194

# Check structure after deduplication
# glimpse(df)

# 🕒 Parse datetime columns
df <- df %>%
  mutate(
    received_date = parse_date_time(received_date, orders = c("mdy HM")),
    first_unit_arrived_time = parse_date_time(first_unit_arrived_time, orders = c("mdy HM")),
    cleared_date = parse_date_time(cleared_date, orders = c("mdy HM"))
  )

# Confirm parsing results
sum(is.na(df$received_date)) # 1
sum(is.na(df$first_unit_arrived_time)) # 21375
sum(is.na(df$cleared_date)) # 1758

# 🧹 Clean 'call_for_service' text field
df <- df %>%
  mutate(call_for_service = str_trim(tolower(call_for_service)))

# 💾 Save cleaned file
# write_csv(df, "el-cerrito-police-cleaned-r.csv")

####################
# Basic Summary Stats
####################

n_total <- nrow(df)
n_unique <- n_distinct(df$event_number)

cat("Total rows:", n_total, "\n") # Total rows: 137194 
cat("Unique event numbers:", n_unique, "\n") # Unique event numbers: 137194 


################

# 1. Total number of incidents (rows)
n_incidents <- nrow(df)
cat("Total number of incidents:", n_incidents, "\n") # 137194 

# 2. Number of incidents by year (based on received_date)
df_yearly <- df %>%
  filter(!is.na(received_date)) %>%  # Remove missing dates
  mutate(year = year(received_date)) %>%
  count(year, name = "incident_count") %>%
  arrange(year)

# print(df_yearly)

# 3. Number of incidents by month (aggregated across all years)
df_monthly <- df %>%
  filter(!is.na(received_date)) %>%
  mutate(month = month(received_date, label = TRUE, abbr = FALSE)) %>%
  count(month, name = "incident_count") %>%
  arrange(match(month, month.name))  # Ensure correct calendar order

print(df_monthly)


# 4. Frequency table of incident types (Call for Service)
df_type_summary <- df %>%
  filter(!is.na(call_for_service)) %>%
  count(call_for_service, name = "count") %>%
  arrange(desc(count)) %>%
  mutate(percent = round((count / sum(count)) * 100, 1))  # % of total

print(df_type_summary, n = 50)
# write_csv(df_type_summary, "el-cerrito-police-counts.csv")

types_df <- function(data = df, call_type) {
  data %>%
    filter(call_for_service == call_type)



df_2438_assault =
  df |> filter(call_for_service == "243a - assault / battery")

df_10851_mv_theft = 
  df |> filter(call_for_service == "10851 - motor vehicle theft")

df_459a_auto_burglary = 
  df |> filter(call_for_service == "459a - auto burglary")

df_parker = 
  df |> filter(call_for_service == "parker - parking violation")

df_unwanted_person = 
  df |> filter(call_for_service == "unwant - unwanted person")


df_487_grand_theft= 
  df |> filter(call_for_service == "487 - grand theft")

df_459r_res_burglary = 
  df |> filter(call_for_service == "459r - residential burglary")

df_314_indecent = 
  df |> filter(call_for_service == "314 - indecent exposure")
print(df_314_indecent, n = 100)



df_10852_vehic_parts_theft = 
  df |> filter(call_for_service == "10852 - vehcile parts theft")


df_shots_fired = 
  df |> filter(call_for_service == "sfrmc - shots fired")


df_person_with_a_gun = 
  df |> filter(call_for_service == "mgun - person with a gun")

df_stalking = 
  df |> filter(call_for_service == "stalk - stalking")


df_rape = 
  df |> filter(call_for_service == "261 - rape")


}


types_df(call_type = "261 - rape")

########################
########################

# ------------------------------
# Address cleanup for geocoding
# ------------------------------
# ------------------------------
# Address cleanup for geocoding
# ------------------------------

df <- df %>%
  mutate(
    # Step 1: Trim, lowercase, and remove extra spaces
    cleaned_address = str_trim(address),                         # trim leading/trailing whitespace
    cleaned_address = str_to_lower(cleaned_address),            # convert to lowercase
    cleaned_address = str_squish(cleaned_address),              # collapse multiple spaces
    
    # Step 2: Expand abbreviations
    cleaned_address = str_replace_all(cleaned_address, c(
      "\\bave\\b" = "avenue",
      "\\bst\\b" = "street",
      "\\brd\\b" = "road",
      "\\bblvd\\b" = "boulevard",
      "\\bdr\\b" = "drive",
      "\\bct\\b" = "court",
      "\\bln\\b" = "lane",
      "\\bplz\\b" = "plaza",
      "\\bhwy\\b" = "highway"
    )),
    
    # Step 3: Remove apartment/unit numbers (e.g., "apt 4B", "#2")
    cleaned_address = str_remove(cleaned_address, "\\b(apt|unit|#)\\s*\\w+\\b"),
    
    # Step 4: Remove trailing commas that might remain
    cleaned_address = str_replace(cleaned_address, ",\\s*$", ""),
    
    # Step 5: Normalize cross-street separators
    cleaned_address = str_replace_all(cleaned_address, "\\s*/\\s*", " / "),
    
    # Step 6: Flag potential cross-street entries
    cross_street_flag = str_detect(cleaned_address, " / "),
    
    # Step 7: Create a full address for geocoding (adds city/state)
    full_address = paste(cleaned_address, "el cerrito, ca", sep = ", ")
  )

# Sample before-and-after
df %>%
  select(address, cleaned_address, full_address) %>%
  head(10)

# Check rows flagged as cross-streets
df %>%
  filter(cross_street_flag) %>%
  select(cleaned_address) %>%
  distinct() %>%
  head(20)

###############

# --------------------------
# Geocode cleaned addresses
# --------------------------

# Load library for geocoding
library(tidygeocoder)  # If not installed: install.packages("tidygeocoder")

# Step 1: Extract unique geocodable addresses
unique_addresses <- df %>%
  filter(!is.na(full_address)) %>%
  distinct(full_address)

# Step 2: Geocode using ArcGIS (fast + good for U.S. locations)
# geocoded_results <- unique_addresses %>%
#   geocode(address = full_address, method = "arcgis", lat = latitude, long = longitude)

names(geocoded_results)
names(df)

# Step 3: Join lat/lon back into main data frame
df <- df %>%
  left_join(geocoded_results, by = "full_address") %>%
  relocate(latitude, longitude, .before = everything())  # move coords to front

# Step 4: Create flags to diagnose geocoding
df <- df %>%
  mutate(
    BAD_LATLON_FLAG = is.na(latitude) | is.na(longitude),
    NO_STREET_NUMBER_FLAG = !str_detect(cleaned_address, "\\d{3,}"),     # no 3+ digit street #
    FREEWAY_FLAG = str_detect(cleaned_address, "(i[- ]?80|interstate|freeway|on[- ]?ramp)")
  )

# Step 5: Sample review
df %>%
  filter(BAD_LATLON_FLAG) %>%
  select(cleaned_address, full_address, latitude, longitude, FREEWAY_FLAG, NO_STREET_NUMBER_FLAG) %>%
  head(20)

# Step 6: Save for reuse
write_csv(df, "el-cerrito-police-with-coords.csv")

# Quick stats
cat("Total incidents:", nrow(df), "\n")
cat("Missing coordinates:", sum(is.na(df$latitude)), "\n")

# Optional: % geocoded
round(mean(!is.na(df$latitude)) * 100, 1)  # % with coordinates

# Keep only good rows for mapping and spatial analysis
df_usable <- df %>%
  filter(!BAD_LATLON_FLAG, !FREEWAY_FLAG)  # optionally also filter NO_STREET_NUMBER_FLAG

# Save subset if needed
write_csv(df_usable, "el-cerrito-police-usable-coords.csv")

df_near_bart <- df_usable %>%
  filter(str_detect(cleaned_address, "cutting|del norte|bart")) %>%
  select(received_date, call_for_service, cleaned_address) %>%
  arrange(received_date)

print(df_near_bart, n = 20)

df %>%
  filter(!is.na(received_date)) %>%
  mutate(year = year(received_date)) %>%
  count(year, call_for_service) %>%
  arrange(desc(n)) %>%
  head(50)


library(ggplot2)
ggplot(df_usable, aes(x = longitude, y = latitude)) +
  geom_point(alpha = 0.3, size = 0.5) +
  coord_fixed() +
  theme_minimal() +
  ggtitle("El Cerrito Police Incidents with Coordinates")

####################
# geocode analysis

# Required libraries
library(geosphere)
library(dplyr)
library(tibble)

# Define function
count_incidents_near_locations <- function(incident_df, location_df, distance_ft = 500) {
  # Convert distance to meters
  distance_m <- distance_ft * 0.3048
  
  # Ensure required columns exist
  if (!all(c("latitude", "longitude") %in% names(incident_df))) {
    stop("Incident data must contain 'latitude' and 'longitude' columns.")
  }
  if (!all(c("name", "lat", "lon") %in% names(location_df))) {
    stop("Location data must contain 'name', 'lat', and 'lon' columns.")
  }
  
  # Filter out invalid coordinates
  incidents_valid <- incident_df %>%
    filter(!is.na(latitude), !is.na(longitude))
  
  # Loop through locations and count nearby incidents
  results <- location_df %>%
    rowwise() %>%
    mutate(
      Incident_Count = {
        landmark_coords <- c(lon, lat)
        incident_coords <- incidents_valid %>%
          select(longitude, latitude) %>%
          as.matrix()
        distances <- distHaversine(incident_coords, landmark_coords)
        sum(distances <= distance_m)
      }
    ) %>%
    ungroup()
  
  return(results)
}

# Define landmarks
landmarks <- tibble::tribble(
  ~name,                 ~lat,     ~lon,
  "Plaza BART",         37.90295, -122.29923,
  "Del Norte BART",     37.92505, -122.31676,
  "Castro Park",        37.91878, -122.30319,
  "Community Center",   37.91560, -122.30181
)

# Call the function
count_results <- count_incidents_near_locations(df_usable, landmarks, distance_ft = 500)

# View results
print(count_results)

# Step 1: Define El Cerrito landmarks with coordinates
landmarks_el_cerrito <- tibble::tribble(
  ~name,                      ~lat,      ~lon,
  "El Cerrito Plaza BART",    37.90295, -122.29923,
  "El Cerrito Del Norte BART",37.92505, -122.31676,
  "Castro Park Pickleball",   37.91878, -122.30319,
  "El Cerrito Community Center", 37.91560, -122.30181,
  "El Cerrito High School",   37.91591, -122.30514,
  "Korematsu Middle School",  37.91599, -122.29687,
  "Harding Elementary School",37.90164, -122.30161,
  "Madera Elementary School", 37.92216, -122.28563,
  "El Cerrito Library",       37.90258, -122.30623
)



# Step 2: Run the proximity function using your filtered incident data
results_500ft <- count_incidents_near_locations(
  incident_df = df_usable,
  location_df = landmarks_el_cerrito,
  distance_ft = 500
)

# Step 3: View the results
print(results_500ft)


# Step 2: Run the proximity function using your filtered incident data
results_300ft <- count_incidents_near_locations(
  incident_df = df_usable,
  location_df = landmarks_el_cerrito,
  distance_ft = 300
)

# Step 3: View the results
print(results_300ft)

############
incident_types <- c(
  "243a - assault / battery",
  "10851 - motor vehicle theft",
  "459a - auto burglary",
  "parker - parking violation",
  "unwant - unwanted person",
  "487 - grand theft",
  "459r - residential burglary",
  "314 - indecent exposure",
  "10852 - vehcile parts theft",
  "sfrmc - shots fired",
  "mgun - person with a gun",
  "stalk - stalking",
  "261 - rape"
)

# Initialize a list to store results
incident_results <- list()

# Loop through each incident type
for (type in incident_types) {
  filtered_df <- df %>% 
    filter(call_for_service == type)
  
  result <- count_incidents_near_locations(
    incident_df = filtered_df,
    location_df = landmarks_el_cerrito,
    distance_ft = 500
  ) %>%
    mutate(call_for_service = type)
  
  incident_results[[type]] <- result
}

# Combine all results into a single data frame
all_counts_by_type <- bind_rows(incident_results)

all_counts_by_type

# Step 6: Save for reuse
write_csv(all_counts_by_type, "all_counts_by_type.csv")

getwd()
#################
#################

# Chunks

library(tidygeocoder)
library(dplyr)
library(readr)

# Load or define your full data frame (example: df)
df <- read_csv("your_data.csv")

# Filter out already-geocoded rows if resuming
df_to_geocode <- df %>% filter(is.na(latitude) | is.na(longitude))

# Split into chunks (example: 500 rows at a time)
chunk_size <- 500
n_chunks <- ceiling(nrow(df_to_geocode) / chunk_size)

geocoded_all <- list()

for (i in 1:n_chunks) {
  message("Processing chunk ", i, " of ", n_chunks)
  
  df_chunk <- df_to_geocode %>%
    slice((1 + (i - 1) * chunk_size):(min(i * chunk_size, nrow(df_to_geocode))))
  
  geo_chunk <- df_chunk %>%
    geocode(address = full_address, method = "arcgis", lat = latitude, long = longitude)
  
  geocoded_all[[i]] <- geo_chunk
  
  # Optional: write intermediate result to disk
  write_csv(bind_rows(geocoded_all), paste0("geo_progress_chunk_", i, ".csv"))
  
  # Respect server — slight pause
  Sys.sleep(1)
}

# Combine all chunks
geocoded_final <- bind_rows(geocoded_all)

# Merge back with original (if needed)
df <- df %>%
  left_join(geocoded_final %>% select(event_number, latitude, longitude), by = "event_number")
#################################

# Filter list

df_unique_call_for_service = data.frame
dim(df_unique_call_for_service) # 127   1
write_csv(df_unique_call_for_service, "unique_calls.csv")


# Step 1: Read the list of desired incident types from CSV
incident_types <- read_csv("types of incidents filtered.csv") %>%
  pull(`unique.df_usable.call_for_service.`)

# Step 2: Filter your main data to include only these incident types
df_filtered <- df %>%
  filter(call_for_service %in% incident_types)

dim(df_filtered) # 45326    14
write_csv(df_filtered, "df_filtered.csv")


##########################

#############################
# Working with filtered set #
#############################

# Reuse the landmarks
landmarks_el_cerrito <- tibble::tribble(
  ~name,                      ~lat,      ~lon,
  "El Cerrito Plaza BART",    37.90295, -122.29923,
  "El Cerrito Del Norte BART",37.92505, -122.31676,
  "Castro Park Pickleball",   37.91878, -122.30319,
  "El Cerrito Community Center", 37.91560, -122.30181,
  "El Cerrito High School",   37.91591, -122.30514,
  "Korematsu Middle School",  37.91599, -122.29687,
  "Harding Elementary School",37.90164, -122.30161,
  "Madera Elementary School", 37.92216, -122.28563,
  "El Cerrito Library",       37.90258, -122.30623
)

# Call the function for 500 ft
results_500ft_filtered <- count_incidents_near_locations(
  incident_df = df_filtered,
  location_df = landmarks_el_cerrito,
  distance_ft = 500  # You can vary this later
)

# View results
print(results_500ft_filtered)

# Call the function for 500 ft
results_300ft_filtered <- count_incidents_near_locations(
  incident_df = df_filtered,
  location_df = landmarks_el_cerrito,
  distance_ft = 300  # You can vary this later
)

# View results
print(results_300ft_filtered)
#############

df %>%
  filter(latitude > 37.923, latitude < 37.927,
         longitude > -122.319, longitude < -122.314) %>%
  count(call_for_service, sort = TRUE)


count_incidents_near_locations(
  incident_df = df,
  location_df = landmarks_el_cerrito,
  distance_ft = 600)  # You can vary this later)


count_incidents_near_locations(
  incident_df = df_filtered,
  location_df = landmarks_el_cerrito,
  distance_ft = 600)  # You can vary this later)


# Top 25 addresses by incident count (using df_filtered)

df_filtered_top25 <- df_filtered %>%
  filter(!is.na(address)) %>%                         # remove missing addresses
  count(address, sort = TRUE) %>%                     # count occurrences
  slice_head(n = 25)                                  # take top 25

# View result
print(df_filtered_top25, n = 25)

# Top 25 cleaned addresses by incident count

df_filtered_top25_cleaned <- df_filtered %>%
  filter(!is.na(cleaned_address)) %>%       # ensure cleaned address exists
  count(cleaned_address, sort = TRUE) %>%   # group and count
  slice_head(n = 25)                        # take top 25

# View result
print(df_filtered_top25_cleaned, n = 100)

###3
# Define the regular expression pattern (outside the pipe)
regex_pattern <- "(?i)el cerrito plaza"  # (?i) makes it case-insensitive
# Use the regex to filter cleaned addresses and count matches
df_plaza_group <- df_filtered %>%
  filter(str_detect(cleaned_address, regex_pattern)) %>%   # regex match
  count(cleaned_address, sort = TRUE)                      # group and count

# View the result
print(df_plaza_group, n = 25)


# Define your regex pattern
regex_pattern <- "(?i)el cerrito plaza"  # case-insensitive match

# Step 1: Get counts by cleaned address
df_plaza_group <- df_filtered %>%
  filter(str_detect(cleaned_address, regex_pattern)) %>%
  count(cleaned_address, sort = TRUE)

# Step 2: Calculate total across all matching addresses
total_plaza_count <- sum(df_plaza_group$n)

# Step 3: Display results
print(df_plaza_group, n = 25)
cat("\nTotal incidents matching El Cerrito Plaza pattern:", total_plaza_count, "\n")



# Step 1: Define Plaza and San Pablo regex patterns
regex_plaza <- "(?i)el cerrito plaza"
regex_sanpablo <- "(?i)san pablo ave(nue)?"

# Step 2A: Count incident types at El Cerrito Plaza
df_plaza_type_counts <- df_filtered %>%
  filter(str_detect(cleaned_address, regex_plaza)) %>%         # match address
  count(call_for_service, sort = TRUE)                          # count types

# Step 2B: Count incident types on San Pablo Avenue
df_sanpablo_type_counts <- df_filtered %>%
  filter(str_detect(cleaned_address, regex_sanpablo)) %>%       # match address
  count(call_for_service, sort = TRUE)                          # count types

# Step 3: Display both results
cat("🅰️ Incident types at El Cerrito Plaza:\n")
print(df_plaza_type_counts, n = 100)

cat("\n🅱️ Incident types on San Pablo Avenue:\n")
print(df_sanpablo_type_counts, n = 100)

# This code counts all filtered incident types that occurred within 500 feet of the El Cerrito Library

# Load required libraries
library(dplyr)
library(geosphere)
library(stringr)

# Step 1: Define coordinates for El Cerrito Library
library_lat <- 37.90258
library_lon <- -122.30623
radius_ft <- 500
radius_m <- radius_ft * 0.3048  # convert feet to meters

# Step 2: Filter to rows with valid coordinates and calculate distance to library
df_near_library <- df_filtered %>%
  filter(!is.na(latitude), !is.na(longitude)) %>%
  rowwise() %>%
  mutate(
    dist_to_library = distHaversine(c(longitude, latitude), c(library_lon, library_lat))
  ) %>%
  ungroup() %>%
  filter(dist_to_library <= radius_m)

# Step 3: Count the number of incidents by type within 500 feet of the library
df_library_type_counts <- df_near_library %>%
  count(call_for_service, sort = TRUE)

# Step 4: Display the results
cat("📚 Incident types within 500 feet of El Cerrito Library:\n")
print(df_library_type_counts, n = 25)
#################

#############
# Key Table #
#############

# This code creates a data frame with incident type counts near El Cerrito Library (distance-based)
# and all incidents matching "el cerrito plaza" via regular expression

# Load required libraries
library(dplyr)
library(geosphere)
library(stringr)
library(tidyr)

# Step 1: Define constants
regex_plaza <- "(?i)el cerrito plaza"        # regex to match cleaned_address
library_lat <- 37.90258
library_lon <- -122.30623
library_radius_ft <- 600                     # customizable radius
library_radius_m <- library_radius_ft * 0.3048

# Step 2: Filter to valid lat/lon rows
df_coords <- df_filtered %>%
  filter(!is.na(latitude), !is.na(longitude))

# Step 3: Get incidents that match regex for El Cerrito Plaza
df_plaza <- df_filtered %>%
  filter(str_detect(cleaned_address, regex_plaza))

# Step 4: Get incidents within X feet of the library
df_library <- df_coords %>%
  rowwise() %>%
  mutate(
    dist_to_library = distHaversine(c(longitude, latitude), c(library_lon, library_lat))
  ) %>%
  ungroup() %>%
  filter(dist_to_library <= library_radius_m)

# Step 5: Count by call_for_service
counts_plaza <- df_plaza %>%
  count(call_for_service, name = "Plaza_Count")

counts_library <- df_library %>%
  count(call_for_service, name = "Library_Count")

# Step 6: Combine into final table
incident_summary <- full_join(counts_plaza, counts_library, by = "call_for_service") %>%
  replace_na(list(Plaza_Count = 0, Library_Count = 0)) %>%
  arrange(desc(Plaza_Count + Library_Count))

# Step 7: View results
print(incident_summary, n = 50)
########################
########################

#############
# heat maps #
#############

# Load required libraries
library(ggplot2)
library(dplyr)
library(tibble)

# Step 1 – Create landmark positions for police map
landmarks_police <- tibble::tribble(
  ~name,                       ~lat,     ~lon,        ~hjust, ~vjust,
  "Plaza BART",               37.90295, -122.29923,  -0.1,   -0.5,
  "Del Norte BART",           37.92505, -122.31676,  -0.1,   -0.5,
  "Castro Park",              37.91878, -122.30319,  -0.1,   -0.5,
  "Community Center",         37.91560, -122.30181,   1.1,    1.2,
  "El Cerrito High School",   37.91500, -122.29500,  -0.1,   -0.5,
  "Korematsu Middle School",  37.91960, -122.29560,  -0.1,   -0.5,
  "Harding Elementary",       37.90490, -122.30010,  -0.1,   -0.5,
  "Madera Elementary",        37.91810, -122.27830,  -0.1,   -0.5,
  "El Cerrito Library",       37.90580, -122.30130,  -0.1,   -0.5
)

# Step 2 – Filter to valid coordinates for heatmap data
heat_data <- df_filtered %>%
  filter(!BAD_LATLON_FLAG)

# Step 3 – Create the heat map using stat_density_2d and label landmarks
ggplot() +
  stat_density_2d(
    data = heat_data,
    aes(x = longitude, y = latitude, fill = after_stat(level)),
    geom = "polygon", contour = TRUE, alpha = 0.8
  ) +
  scale_fill_gradient(
    low = "lightgreen", high = "red", name = "Incident Density"
  ) +
  geom_point(
    data = landmarks_police,
    aes(x = lon, y = lat),
    color = "black", size = 3, shape = 21, fill = "white"
  ) +
  geom_text(
    data = landmarks_police,
    aes(x = lon, y = lat, label = name, hjust = hjust, vjust = vjust),
    size = 2.5, fontface = "bold"
  ) +
  labs(
    title = "El Cerrito Police Incidents – Heat Map",
    subtitle = "Filtered Incident Types (April 2023 – March 2025)"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 10)
  )


# Quick summary to confirm data coverage
df_filtered %>%
  filter(!BAD_LATLON_FLAG) %>%
  summarize(
    min_lat = min(latitude, na.rm = TRUE),
    max_lat = max(latitude, na.rm = TRUE),
    min_lon = min(longitude, na.rm = TRUE),
    max_lon = max(longitude, na.rm = TRUE),
    n_valid = n()
  )

# Step 1: Identify points clearly outside El Cerrito
df_outliers <- df_filtered %>%
  filter(!BAD_LATLON_FLAG) %>%
  filter(latitude < 37.85 | latitude > 38.00 |
           longitude < -122.35 | longitude > -122.25)

# View number of bad points and some samples
cat("Number of out-of-bounds points:", nrow(df_outliers), "\n")
print(df_outliers %>% select(event_number, latitude, longitude, address) %>% head(1000), n = 1000)
##############################
##############################

# Step 1: Filter to valid El Cerrito coordinates
df_filtered_clean <- df_filtered %>%
  filter(!BAD_LATLON_FLAG) %>%                                      # remove flagged bad coords
  filter(latitude >= 37.85, latitude <= 38.00,                      # restrict to El Cerrito area
         longitude >= -122.35, longitude <= -122.25)

# Step 2: Print summary to verify clean data coverage
df_filtered_clean %>%
  summarize(
    min_lat = min(latitude),
    max_lat = max(latitude),
    min_lon = min(longitude),
    max_lon = max(longitude),
    n_cleaned = n()
  )

# Step 3: Create a heat map of incident density using cleaned police data

# Define landmarks including El Cerrito Plaza and the Library
landmarks <- tibble::tribble(
  ~name,                 ~lat,     ~lon,        ~hjust, ~vjust,
  "El Cerrito Plaza",    37.90295, -122.29923,  -0.1,   -0.5,
  "Del Norte BART",      37.92505, -122.31676,  -0.1,   -0.5,
  "Castro Park",         37.91878, -122.30319,  -0.1,   -0.5,
  "Community Center",    37.91560, -122.30181,   1.1,    1.2,
  "El Cerrito Library",  37.90783, -122.29345,  -0.1,   -0.5
)

# Create the heat map plot
ggplot() +
  stat_density_2d(
    data = df_filtered_clean,
    aes(x = longitude, y = latitude, fill = after_stat(level)),
    geom = "polygon", contour = TRUE, alpha = 0.8, bins = 40
  ) +
  scale_fill_gradient(
    low = "lightgreen", high = "red", name = "Incident Density"
  ) +
  geom_point(
    data = landmarks,
    aes(x = lon, y = lat),
    color = "black", size = 3, shape = 21, fill = "white"
  ) +
  geom_text(
    data = landmarks,
    aes(x = lon, y = lat, label = name, hjust = hjust, vjust = vjust),
    size = 2.5, fontface = "bold"
  ) +
  labs(
    title = "El Cerrito Police Incidents",
    subtitle = "Heat Map of Incident Density"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 10)
  )

################################
################################

# Step: Create table of top 50 incident locations by cleaned address
# Step: Create table of top 50 incident locations by cleaned address
# Step: Top 50 incident locations with highest counts, descending order

top_locations <- df_filtered_clean %>%
  group_by(cleaned_address, latitude, longitude) %>%   # Group by address and coordinates
  tally(name = "count") %>%                            # Count incidents per location
  ungroup() %>%
  arrange(desc(count)) %>%                             # Sort by count, descending
  slice_max(order_by = count, n = 50)                  # Keep top 50

# View result
top_locations
write_csv(top_locations, "top_locations.csv")

################################
###############################

# Step 1: Read the top locations file
top_locations <- readr::read_csv("top_locations.csv")

# Step 2: Extract addresses to remove
addresses_to_remove <- top_locations %>%
  filter(Delete == "yes") %>%                 # only rows marked "yes"
  pull(cleaned_address) %>%                   # get the list of addresses
  unique()                                    # ensure no duplicates

# Step 3: Filter the dataset
df_filtered_clean_updated <- df_filtered_clean %>%
  filter(!(cleaned_address %in% addresses_to_remove))  # remove rows with flagged addresses

dim(df_filtered_clean)
dim(df_filtered_clean_updated)

# Load required libraries
library(readxl)
library(dplyr)
library(ggplot2)

# Step 1: Read cleaned landmark coordinates from Excel
landmarks <- read_excel("landmark lat and long.xlsx") %>%
  rename(name = Landmark, lat = Latitude, lon = Longitude) %>%
  mutate(
    hjust = -0.1,  # adjust as needed
    vjust = -0.5
  )

# Step 2: Plot heat map
ggplot() +
  stat_density_2d(
    data = df_filtered_clean_updated,
    aes(x = longitude, y = latitude, fill = after_stat(level)),
    geom = "polygon", contour = TRUE, alpha = 0.8, bins = 40  # increase bins for resolution
  ) +
  scale_fill_gradient(
    low = "lightgreen", high = "red", name = "Incident Density"
  ) +
  geom_point(
    data = landmarks,
    aes(x = lon, y = lat),
    color = "black", size = 3, shape = 21, fill = "white"
  ) +
  geom_text(
    data = landmarks,
    aes(x = lon, y = lat, label = name, hjust = hjust, vjust = vjust),
    size = 2.5, fontface = "bold"
  ) +
  labs(
    title = "El Cerrito Police Incidents (Cleaned)",
    subtitle = "Heat Map of Incident Density After Removing Invalid Locations"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 10)
  )

########
dim(df) # 137194     14

df_filtered <- df %>%
  filter(call_for_service %in% incident_types)

dim(df_filtered) # 45326    14

df_filtered_clean <- df_filtered %>%
  filter(!BAD_LATLON_FLAG) %>%                                      # remove flagged bad coords
  filter(latitude >= 37.85, latitude <= 38.00,                      # restrict to El Cerrito area
         longitude >= -122.35, longitude <= -122.25)
dim(df_filtered_clean) # 45244    14
dim(df_filtered_clean_updated) # 39967    14

##############################
##############################

# STEP 1: Load necessary libraries
library(readxl)     # for reading landmark Excel file
library(dplyr)      # for data wrangling
library(ggplot2)    # for plotting
library(tibble)     # for tribble (if needed, but not used here)

# STEP 2: Read landmark coordinates using your actual column names
landmarks <- read_excel("landmark lat and long.xlsx") %>%
  rename(
    name = `Landmark Name`,
    lat = Latitude,
    lon = Longitude
  ) %>%
  mutate(
    hjust = -0.1,   # horizontal text adjustment
    vjust = -0.5    # vertical text adjustment
  )


# STEP 3: Create the heat map using final cleaned dataset
ggplot() +
  stat_density_2d(
    data = df_filtered_clean_updated,
    aes(x = longitude, y = latitude, fill = after_stat(level)),
    geom = "polygon", contour = TRUE, alpha = 0.8, bins = 40
  ) +
  scale_fill_gradient(
    low = "lightgreen", high = "red", name = "Incident Density"
  ) +
  geom_point(
    data = landmarks,
    aes(x = lon, y = lat),
    color = "black", size = 3, shape = 21, fill = "white"
  ) +
  geom_text(
    data = landmarks,
    aes(x = lon, y = lat, label = name, hjust = hjust, vjust = vjust),
    size = 2.5, fontface = "bold"
  ) +
  labs(
    title = "El Cerrito Police Incidents",
    subtitle = "Heat Map of Incident Density"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 10)
  )
################################$###

###############
# Grid Search #
###############

# Step 1: Load necessary library
library(geosphere)   # for distance calculations
library(dplyr)
library(ggplot2)
library(ggrepel)
library(scales)

# Step 2: Set parameters
lat_range <- seq(37.88, 37.94, by = 0.001)       # approx every 360 ft north–south
lon_range <- seq(-122.33, -122.27, by = 0.001)   # approx every 360 ft east–west
radius_ft <- 500                                 # search radius in feet
radius_m <- radius_ft * 0.3048                   # convert to meters

# Step 3: Create grid of lat/lon points
grid_points <- expand.grid(lat = lat_range, lon = lon_range)

# Step 4: Define function to count nearby incidents
count_nearby <- function(grid_lat, grid_lon, data_latlon) {
  distances <- distHaversine(matrix(c(grid_lon, grid_lat), ncol = 2),
                             data_latlon)
  sum(distances <= radius_m)
}

# Step 5: Apply function to all grid points
incident_coords <- df_filtered_clean_updated %>%
  select(longitude, latitude) %>%
  as.matrix()

grid_points$count <- apply(grid_points, 1, function(row) {
  count_nearby(row["lat"], row["lon"], incident_coords)
})

# Step 6: Get top 20 hotspots
top_hotspots <- grid_points %>%
  arrange(desc(count)) %>%
  slice_head(n = 20) %>%
  mutate(rank = row_number())

# Step 7: Plot map with heat layer and hotspot labels
ggplot() +
  stat_density_2d(
    data = df_filtered_clean_updated,
    aes(x = longitude, y = latitude, fill = after_stat(level)),
    geom = "polygon", contour = TRUE, alpha = 0.7, bins = 40
  ) +
  scale_fill_gradient(low = "lightgreen", high = "red", name = "Incident Density") +
  geom_point(
    data = top_hotspots,
    aes(x = lon, y = lat), color = "black", size = 3, shape = 21, fill = "white"
  ) +
  geom_text_repel(
    data = top_hotspots,
    aes(x = lon, y = lat, label = rank),
    size = 3, fontface = "bold", color = "black", box.padding = 0.3
  ) +
  labs(
    title = "El Cerrito Police Incident Hotspots",
    subtitle = paste("Top 20 centers within", radius_ft, "feet radius"),
    fill = "Incident Density"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 10)
  )
####################

# Reverse Engineer to find cross streets
# Show lat/lon for points 7, 9, and 15 to identify cross streets manually
names(top_hotspots)

# Show lat/lon for hotspot ranks 7, 9, and 15
top_hotspots %>%
  filter(rank %in% c(7, 9, 11, 15)) %>%
  select(rank, lat, lon) %>%
  arrange(rank)
########################

# Updated 13 hotspots

# Step 1: Read landmark locations from your cleaned Excel file
landmarks <- readxl::read_excel("landmark lat and long.xlsx") %>%
  rename(name = `Landmark Name`, lat = Latitude, lon = Longitude) %>%
  mutate(
    hjust = -0.1,  # label position fine-tuning
    vjust = -0.5
  )

# Step 2: Add 3 new hotspot centers manually
hotspot_labels <- tibble::tribble(
  ~name,                            ~lat,     ~lon,        ~hjust, ~vjust,
  "Liberty & Portola",              37.914,   -122.306,    1.2,    -0.5,
  "San Pablo & Blake",              37.922,   -122.316,    -0.1,   -0.5,
  "Fairmount & Liberty",            37.901,   -122.299,    -0.1,   1.2
)

# Step 3: Combine original landmarks and hotspot labels
all_points <- bind_rows(landmarks, hotspot_labels)

# Step 4: Generate the heat map using updated dataset
ggplot() +
  stat_density_2d(
    data = df_filtered_clean_updated,  # your final cleaned incident dataset
    aes(x = longitude, y = latitude, fill = after_stat(level)),
    geom = "polygon", contour = TRUE, alpha = 0.8, bins = 40
  ) +
  scale_fill_gradient(
    low = "lightgreen", high = "red", name = "Incident Density"
  ) +
  geom_point(
    data = all_points,
    aes(x = lon, y = lat),
    color = "black", size = 3, shape = 21, fill = "white"
  ) +
  geom_text(
    data = all_points,
    aes(x = lon, y = lat, label = name, hjust = hjust, vjust = vjust),
    size = 2.5, fontface = "bold"
  ) +
  labs(
    title = "El Cerrito Police Incident Hotspots and Landmarks",
    subtitle = "Heat Map of Incident Density (Filtered Categories)"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 10)
  )
####################
# Use above graph but filter the data

parking_df = 
  df_filtered_clean_updated |> filter(call_for_service == "parker - parking violation")


# Step 4: Generate the heat map using updated dataset
ggplot() +
  stat_density_2d(
    data = parking_df,  # your final cleaned incident dataset
    aes(x = longitude, y = latitude, fill = after_stat(level)),
    geom = "polygon", contour = TRUE, alpha = 0.8, bins = 40
  ) +
  scale_fill_gradient(
    low = "lightgreen", high = "red", name = "Incident Density"
  ) +
  geom_point(
    data = all_points,
    aes(x = lon, y = lat),
    color = "black", size = 3, shape = 21, fill = "white"
  ) +
  geom_text(
    data = all_points,
    aes(x = lon, y = lat, label = name, hjust = hjust, vjust = vjust),
    size = 2.5, fontface = "bold"
  ) +
  labs(
    title = "El Cerrito Police Incident Hotspots and Landmarks",
    subtitle = "Heat Map of Incident Density (Filtered Categories)"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 10)
  )

##############################

# save work

# Save as CSV files
write.csv(df, "df.csv", row.names = FALSE)
write.csv(df_filtered, "df_filtered.csv", row.names = FALSE)
write.csv(df_filtered_clean, "df_filtered_clean.csv", row.names = FALSE)
write.csv(df_filtered_clean_updated, "df_filtered_clean_updated.csv", row.names = FALSE)

# Save as RDS files
saveRDS(df, "df.rds")
saveRDS(df_filtered, "df_filtered.rds")
saveRDS(df_filtered_clean, "df_filtered_clean.rds")
saveRDS(df_filtered_clean_updated, "df_filtered_clean_updated.rds")

