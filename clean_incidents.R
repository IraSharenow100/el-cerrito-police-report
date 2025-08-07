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
write_csv(df, "el-cerrito-police-cleaned-r.csv")

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

print(df_yearly)

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
write_csv(df_type_summary, "el-cerrito-police-counts.csv")

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
