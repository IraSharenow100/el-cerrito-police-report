# -*- coding: utf-8 -*-
"""
Created on Wed Aug  6 06:08:29 2025

@author: irash
"""

# Python translation of the R code using pandas, numpy, and datetime parsing
# Run this in Spyder (Anaconda). Ensure pandas is installed.

import pandas as pd
import numpy as np
from datetime import datetime

# 1. 📁 Set working directory and load the CSV
# Equivalent to setwd() in R
# You can use an absolute path or relative one
file_path = "D:/Documents/Employment/2025 job search/Project 2025/el-cerrito-police-report/data/combined/el-cerrito-police-incidents.csv"
df = pd.read_csv(file_path)

# 2. 🧹 Standardize column names
# - Trim whitespace
# - Replace spaces with underscores
# - Convert to lowercase
df.columns = df.columns.str.strip().str.replace(" ", "_").str.lower()

# 3. 📊 Show initial number of rows
print("Initial number of rows:", len(df))  # Expected: 175495

# 4. 🔁 Remove exact duplicate rows
df = df.drop_duplicates()
print("After dropping full duplicates:", len(df))  # Expected: 142986

# 5. ❌ Drop rows with missing event_number
df = df[df['event_number'].notna()]
print("After removing rows with missing event_number:", len(df))  # Expected: 137194

# 6. 🕒 Parse datetime columns
# Define a function that safely parses mdy HM format
def parse_mdy_hm(series):
    return pd.to_datetime(series, format="%m/%d/%Y %H:%M", errors='coerce')

df['received_date'] = parse_mdy_hm(df['received_date'])
df['first_unit_arrived_time'] = parse_mdy_hm(df['first_unit_arrived_time'])
df['cleared_date'] = parse_mdy_hm(df['cleared_date'])

# 7. ✅ Confirm parsing
print("Missing received_date:", df['received_date'].isna().sum())         # Expected: 1
print("Missing first_unit_arrived_time:", df['first_unit_arrived_time'].isna().sum())  # Expected: ~21,375
print("Missing cleared_date:", df['cleared_date'].isna().sum())           # Expected: ~1,758

# 8. 🧹 Clean 'call_for_service' column: lowercase and trim
df['call_for_service'] = df['call_for_service'].str.strip().str.lower()

# 9. 💾 Save cleaned file
df.to_csv("el-cerrito-police-cleaned-python.csv", index=False)

# -----------------------------
# Summary Statistics
# -----------------------------

# 1. 🔢 Total number of incidents
print("Total incidents (rows):", len(df))  # Expected: 137194

# 2. 📅 Number of incidents by year (from received_date)
df['year'] = df['received_date'].dt.year
df_yearly = df[df['year'].notna()].groupby('year').size().reset_index(name='incident_count')
print("\nIncidents by Year:\n", df_yearly)

# 3. 📆 Number of incidents by month (all years combined)
df['month'] = df['received_date'].dt.month
month_lookup = {
    1: "January", 2: "February", 3: "March", 4: "April",
    5: "May", 6: "June", 7: "July", 8: "August",
    9: "September", 10: "October", 11: "November", 12: "December"
}
df['month_name'] = df['month'].map(month_lookup)
df_monthly = df[df['month'].notna()].groupby('month_name').size().reset_index(name='incident_count')
# Sort by calendar order
df_monthly['month_number'] = df_monthly['month_name'].map({v: k for k, v in month_lookup.items()})
df_monthly = df_monthly.sort_values(by='month_number').drop(columns='month_number')
print("\nIncidents by Month:\n", df_monthly)


# 4. 🧾 Frequency table of incident types
df_type_summary = df[df['call_for_service'].notna()] \
    .groupby('call_for_service') \
    .size().reset_index(name='count') \
    .sort_values(by='count', ascending=False)

# Add percent of total
df_type_summary['percent'] = round(df_type_summary['count'] / df_type_summary['count'].sum() * 100, 1)

# Show top 50 incident types
print("\nTop Incident Types:\n", df_type_summary.head(50))

# Save to CSV
df_type_summary.to_csv("el-cerrito-police-counts-python.csv", index=False)

# -----------------------------
# Helper Function (Like types_df in R)
# -----------------------------

def filter_by_call_type(data, call_type):
    """
    Returns a DataFrame with incidents of the specified call_for_service type.
    
    Parameters:
    - data: pandas DataFrame (default: df)
    - call_type: string to match in 'call_for_service' column
    
    Returns:
    - Filtered DataFrame
    """
    return data[data['call_for_service'] == call_type]

# Example usage:
df_rape = filter_by_call_type(df, "261 - rape")
print("\nSample Rape Calls:\n", df_rape.head())
