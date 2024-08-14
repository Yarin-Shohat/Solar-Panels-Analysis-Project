Data Anlysis Project - Solar Panels
================
Team 2

- [Preprocessing](#preprocessing)
  - [Load Library For Preprocessing](#load-library-for-preprocessing)
  - [Read Data](#read-data)
    - [Read Altitudes Data](#read-altitudes-data)
    - [Read Electric Data](#read-electric-data)
    - [Read Weather Data from Meteorological
      Service](#read-weather-data-from-meteorological-service)
    - [Read Weather Data from Visual
      Crossing](#read-weather-data-from-visual-crossing)
  - [Preprocess Electric Data and Merge to one
    DF](#preprocess-electric-data-and-merge-to-one-df)
    - [Preprocess Electric Data](#preprocess-electric-data)
    - [Merge Electric Data to one DF](#merge-electric-data-to-one-df)
  - [Preprocess Weather Data from Meteorological Service and Merge to
    one
    DF](#preprocess-weather-data-from-meteorological-service-and-merge-to-one-df)
    - [Merge Weather Data from Meteorological Service to one
      DF](#merge-weather-data-from-meteorological-service-to-one-df)
    - [Preprocess Weather Data from Meteorological Service to one
      DF](#preprocess-weather-data-from-meteorological-service-to-one-df)
    - [Join Weather Data from Meteorological Service With Electric
      Data](#join-weather-data-from-meteorological-service-with-electric-data)
  - [Preprocess Weather Data from Visual Crossing and Merge to one
    DF](#preprocess-weather-data-from-visual-crossing-and-merge-to-one-df)
    - [Merge and Preprocess Weather Data from Visual Crossing to one
      DF](#merge-and-preprocess-weather-data-from-visual-crossing-to-one-df)
    - [Join Weather Data from Meteorological Service and Electric Data
      with Visual
      Crossing](#join-weather-data-from-meteorological-service-and-electric-data-with-visual-crossing)
  - [Missing Values Handelling](#missing-values-handelling)
  - [Feature Engineering](#feature-engineering)
    - [Season Column](#season-column)
    - [Altitude Column](#altitude-column)
    - [Area Column](#area-column)
    - [Temperature Range](#temperature-range)
- [Data Analysis](#data-analysis)
  - [Visualization](#visualization)
  - [Linear Regression](#linear-regression)
    - [Regular Linear Regression for Weather
      Features](#regular-linear-regression-for-weather-features)
    - [Fixed Model Linear Regression for Weather
      Features](#fixed-model-linear-regression-for-weather-features)
    - [Fixed Model Linear Regression for Location
      Features](#fixed-model-linear-regression-for-location-features)
    - [Combined Linear Regression for Weather Features & Location
      Features](#combined-linear-regression-for-weather-features--location-features)
  - [Bootstrap](#bootstrap)

# Preprocessing

## Load Library For Preprocessing

``` r
library(tidyverse)
library(readxl)
```

## Read Data

Link to Drive of the Data files is in the Final Report

### Read Altitudes Data

``` r
# Read the Data for Altitudes for the Locations
Altitudes <- read_excel("../data/Altitudes.xlsx")
```

### Read Electric Data

**Note: This is Private data**

``` r
# Read the Data of Solar Panels
suppressMessages({
  suppressWarnings({ Location1 <- read_excel("../data/Location1.xlsx") })
  suppressWarnings({ Location2 <- read_excel("../data/Location2.xlsx") })
  suppressWarnings({ Location3 <- read_excel("../data/Location3.xlsx") })
  suppressWarnings({ Location4 <- read_excel("../data/Location4.xlsx") })
  suppressWarnings({ Location5 <- read_excel("../data/Location5.xlsx") })
  suppressWarnings({ Location6 <- read_excel("../data/Location6.xlsx") })
  suppressWarnings({ Location7 <- read_excel("../data/Location7.xlsx") })
  suppressWarnings({ Location8 <- read_excel("../data/Location8.xlsx") })
  suppressWarnings({ Location9 <- read_excel("../data/Location9.xlsx") })
})
```

### Read Weather Data from Meteorological Service

``` r
# Read Weather Data from Meteorological Service
Location3_Weather <- suppressWarnings(read_excel("../data/Location3_Weather.xlsx"))
Location4_Weather <- suppressWarnings(read_excel("../data/Location4_Weather.xlsx"))
Location5_Weather <- suppressWarnings(read_excel("../data/Location5_Weather.xlsx"))
Location6_Weather <- suppressWarnings(read_excel("../data/Location6_Weather.xlsx"))
Location7_Weather <- suppressWarnings(read_excel("../data/Location7_Weather.xlsx"))
Location8_Weather <- suppressWarnings(read_excel("../data/Location8_Weather.xlsx"))
Location9_Weather <- suppressWarnings(read_excel("../data/Location9_Weather.xlsx"))
Location2_Weather <- suppressWarnings(read_excel("../data/Location2_Weather.xlsx"))
Location1_Weather <- suppressWarnings(read_excel("../data/Location1_Weather.xlsx"))
```

### Read Weather Data from Visual Crossing

``` r
# Read Weather Data from Visual Crossing
Location1_Weather2 <- read.csv("../data/Location1 2023-01-01 to 2023-12-31.csv")
Location2_Weather2 <- read.csv("../data/Location2 2023-01-01 to 2023-12-31.csv")
Location3_Weather2 <- read.csv("../data/Location3 2023-01-01 to 2023-12-31.csv")
Location4_Weather2 <- read.csv("../data/Location4 2023-01-01 to 2023-12-31.csv")
Location5_Weather2 <- read.csv("../data/Location5 2023-01-01 to 2023-12-31.csv")
Location6_Weather2 <- read.csv("../data/Location6 2023-01-01 to 2023-12-31.csv")
Location7_Weather2 <- read.csv("../data/Location7 2023-01-01 to 2023-12-31.csv")
Location8_Weather2 <- read.csv("../data/Location8 2023-01-01 to 2023-12-31.csv")
Location9_Weather2 <- read.csv("../data/Location9 2023-01-01 to 2023-12-31.csv")
```

## Preprocess Electric Data and Merge to one DF

### Preprocess Electric Data

``` r
# Select the columns we need from Solar Data
Location3 <- Location3 %>% 
  select(Timestamp,
         "Location3 - inverter 1 south -  [] - E_INT_N", 
         "Location3 - inverter 1 south - Current AC [A] - I_AC", 
         "Location3 - inverter 1 south - Direct Current [A] - I_DC", 
         "Location3 - inverter 1 south - AC Voltage [V] - U_AC", 
         "Location3 - inverter 1 south - Voltage DC [V] - U_DC")

Location1 <- Location1 %>% 
  select(Timestamp,
          "Location1 - INV 1 1234 - Daily energy yield [kWh] - E_INT_N", 
          "Location1 - INV 1 1234 - Current AC [A] - I_AC", 
          "Location1 - INV 1 1234 - Direct Current [A] - I_DC", 
          "Location1 - INV 1 1234 - AC Voltage [V] - U_AC", 
          "Location1 - INV 1 1234 - Voltage DC [V] - U_DC")

Location2 <- Location2 %>% select(Timestamp,
                                  "Location2 - INV 1 -  [] - E_INT_N", 
                                  "Location2 - INV 1 - Current AC [A] - I_AC", 
                                  "Location2 - INV 1 - Direct Current [A] - I_DC", 
                                  "Location2 - INV 1 - AC Voltage [V] - U_AC", 
                                  "Location2 - INV 1 - Voltage DC [V] - U_DC")

Location4 <- Location4 %>% select(Timestamp,
                                 "Location4 - INV 1 2345 -  [] - E_INT_N", 
                                 "Location4 - INV 1 2345 - Current AC [A] - I_AC", 
                                 "Location4 - INV 1 2345 - Direct Current [A] - I_DC", 
                                 "Location4 - INV 1 2345 - AC Voltage [V] - U_AC", 
                                 "Location4 - INV 1 2345 - Voltage DC [V] - U_DC")

Location5 <- Location5 %>% select(Timestamp,
                                 "Location5 - INV 1 2345 -  [] - E_INT_N", 
                                 "Location5 - INV 1 2345 - Current AC [A] - I_AC", 
                                 "Location5 - INV 1 2345 - Direct Current [A] - I_DC", 
                                 "Location5 - INV 1 2345 - AC Voltage [V] - U_AC", 
                                 "Location5 - INV 1 2345 - Voltage DC [V] - U_DC")

Location6 <- Location6 %>% select(Timestamp,
                                 "Location6 -  INV 1 4567 - Daily energy yield [kWh] - E_INT_N", 
                                 "Location6 -  INV 1 4567 - Current AC [A] - I_AC", 
                                 "Location6 -  INV 1 4567 - Direct Current [A] - I_DC", 
                                 "Location6 -  INV 1 4567 - AC Voltage [V] - U_AC", 
                                 "Location6 -  INV 1 4567 - Voltage DC [V] - U_DC")


Location7 <- Location7 %>% select(Timestamp,
                                  "Location7 - INV 1 7896 -  [] - E_INT_N", 
                                  "Location7 - INV 1 7896 - Current AC [A] - I_AC", 
                                  "Location7 - INV 1 7896 - Direct Current [A] - I_DC", 
                                  "Location7 - INV 1 7896 - AC Voltage [V] - U_AC", 
                                  "Location7 - INV 1 7896 - Voltage DC [V] - U_DC")


Location8 <- Location8 %>% select(Timestamp,
                                 "Location8  - Station 1 inverter 1 -  [] - E_INT_N", 
                                 "Location8  - Station 1 inverter 1 - Current AC [A] - I_AC", 
                                 "Location8  - Station 1 inverter 1 - Direct Current [A] - I_DC", 
                                 "Location8  - Station 1 inverter 1 - AC Voltage [V] - U_AC", 
                                 "Location8  - Station 1 inverter 1 - Voltage DC [V] - U_DC")

Location9 <- Location9 %>% select(Timestamp,
                            "Location9 - Inv 1 -  [] - E_INT_N",
                            "Location9 - Inv 1 - Current AC [A] - I_AC",
                            "Location9 - Inv 1 - Direct Current [A] - I_DC",
                            "Location9 - Inv 1 - AC Voltage [V] - U_AC",
                            "Location9 - Inv 1 - Voltage DC [V] - U_DC")

# Function to process each data frame
# The Function is change the columns name to uniform names and change the Types
#
# @param df - The Data Frame
# @param location - The Location Name
# @param e_int_n_col - Columns for E_INT_N
# @param i_ac_col -  Current AC Column
# @param i_dc_col - Current DC Column
# @param u_ac_col - AC Voltage Column
# @param u_dc_col - DC Voltage Column
process_df <- function(df, location, e_int_n_col, i_ac_col, i_dc_col, u_ac_col, u_dc_col) {
  df %>%
    select(Timestamp,
           Daily_energy_yield_kWh = !!sym(e_int_n_col),
           Current_AC_A = !!sym(i_ac_col),
           Direct_Current_A = !!sym(i_dc_col),
           AC_Voltage_V = !!sym(u_ac_col),
           Voltage_DC_V = !!sym(u_dc_col)) %>%
    mutate(Location = location,
           Daily_energy_yield_kWh = as.numeric(Daily_energy_yield_kWh),
           Current_AC_A = as.numeric(Current_AC_A),
           Direct_Current_A = as.numeric(Direct_Current_A),
           AC_Voltage_V = as.numeric(AC_Voltage_V),
           Voltage_DC_V = as.numeric(Voltage_DC_V))
}

# Process each Data Frame
Location3 <- process_df(Location3, "Location3", 
                       "Location3 - inverter 1 south -  [] - E_INT_N", 
                       "Location3 - inverter 1 south - Current AC [A] - I_AC", 
                       "Location3 - inverter 1 south - Direct Current [A] - I_DC", 
                       "Location3 - inverter 1 south - AC Voltage [V] - U_AC", 
                       "Location3 - inverter 1 south - Voltage DC [V] - U_DC")

Location1 <- process_df(Location1, "Location1", 
                      "Location1 - INV 1 1234 - Daily energy yield [kWh] - E_INT_N", 
                      "Location1 - INV 1 1234 - Current AC [A] - I_AC", 
                      "Location1 - INV 1 1234 - Direct Current [A] - I_DC", 
                      "Location1 - INV 1 1234 - AC Voltage [V] - U_AC", 
                      "Location1 - INV 1 1234 - Voltage DC [V] - U_DC")

Location2 <- process_df(Location2, "Location2", 
                        "Location2 - INV 1 -  [] - E_INT_N", 
                        "Location2 - INV 1 - Current AC [A] - I_AC", 
                        "Location2 - INV 1 - Direct Current [A] - I_DC", 
                        "Location2 - INV 1 - AC Voltage [V] - U_AC", 
                        "Location2 - INV 1 - Voltage DC [V] - U_DC")

Location4 <- process_df(Location4, "Location4", 
                         "Location4 - INV 1 2345 -  [] - E_INT_N", 
                         "Location4 - INV 1 2345 - Current AC [A] - I_AC", 
                         "Location4 - INV 1 2345 - Direct Current [A] - I_DC", 
                         "Location4 - INV 1 2345 - AC Voltage [V] - U_AC", 
                         "Location4 - INV 1 2345 - Voltage DC [V] - U_DC")

Location5 <- process_df(Location5, "Location5", 
                     "Location5 - INV 1 2345 -  [] - E_INT_N", 
                     "Location5 - INV 1 2345 - Current AC [A] - I_AC", 
                     "Location5 - INV 1 2345 - Direct Current [A] - I_DC", 
                     "Location5 - INV 1 2345 - AC Voltage [V] - U_AC", 
                     "Location5 - INV 1 2345 - Voltage DC [V] - U_DC")

Location6 <- process_df(Location6, "Location6", 
                         "Location6 -  INV 1 4567 - Daily energy yield [kWh] - E_INT_N", 
                         "Location6 -  INV 1 4567 - Current AC [A] - I_AC", 
                         "Location6 -  INV 1 4567 - Direct Current [A] - I_DC", 
                         "Location6 -  INV 1 4567 - AC Voltage [V] - U_AC", 
                         "Location6 -  INV 1 4567 - Voltage DC [V] - U_DC")

Location7 <- process_df(Location7, "Location7", 
                        "Location7 - INV 1 7896 -  [] - E_INT_N", 
                        "Location7 - INV 1 7896 - Current AC [A] - I_AC", 
                        "Location7 - INV 1 7896 - Direct Current [A] - I_DC", 
                        "Location7 - INV 1 7896 - AC Voltage [V] - U_AC", 
                        "Location7 - INV 1 7896 - Voltage DC [V] - U_DC")

Location8 <- process_df(Location8, "Location8", 
                         "Location8  - Station 1 inverter 1 -  [] - E_INT_N", 
                         "Location8  - Station 1 inverter 1 - Current AC [A] - I_AC", 
                         "Location8  - Station 1 inverter 1 - Direct Current [A] - I_DC", 
                         "Location8  - Station 1 inverter 1 - AC Voltage [V] - U_AC", 
                         "Location8  - Station 1 inverter 1 - Voltage DC [V] - U_DC")

Location9 <- process_df(Location9, "Location9", 
                      "Location9 - Inv 1 -  [] - E_INT_N", 
                      "Location9 - Inv 1 - Current AC [A] - I_AC", 
                      "Location9 - Inv 1 - Direct Current [A] - I_DC", 
                      "Location9 - Inv 1 - AC Voltage [V] - U_AC", 
                      "Location9 - Inv 1 - Voltage DC [V] - U_DC")
```

### Merge Electric Data to one DF

``` r
# Merge all data frames
Solar_df <- bind_rows(Location3, Location1, Location2, Location4, Location5, Location6, Location7, Location8, Location9)
```

## Preprocess Weather Data from Meteorological Service and Merge to one DF

### Merge Weather Data from Meteorological Service to one DF

``` r
# Create a function to rename columns
# The Function is change the columns name to uniform names
#
# @param df - The Data Frame
# @param location - The Location Name
rename_columns <- function(df, location) {
  df %>%
    select(
      Timestamp = `תאריך ושעה (שעון קיץ)`,
      Relative_Humidity_Percent = `לחות יחסית (%)`,
      Temperature_C = `טמפרטורה (C°)`,
      Max_Temperature_C = `טמפרטורת מקסימום (C°)`,
      Min_Temperature_C = `טמפרטורת מינימום (C°)`,
      Rain_mm = `כמות גשם (מ"מ)`
    ) %>%
    mutate(Location = location)
}

# Process each Data Frame
Location3_Weather <- rename_columns(Location3_Weather, "Location3")
Location1_Weather <- rename_columns(Location1_Weather, "Location1")
Location2_Weather <- rename_columns(Location2_Weather, "Location2")
Location4_Weather <- rename_columns(Location4_Weather, "Location4")
Location5_Weather <- rename_columns(Location5_Weather, "Location5")
Location6_Weather <- rename_columns(Location6_Weather, "Location6")
Location7_Weather <- rename_columns(Location7_Weather, "Location7")
Location8_Weather <- rename_columns(Location8_Weather, "Location8")
Location9_Weather <- rename_columns(Location9_Weather, "Location9")

# Merge all Weather Data frames
Weather_df <- bind_rows(
  Location3_Weather, 
  Location1_Weather, 
  Location2_Weather, 
  Location4_Weather, 
  Location5_Weather, 
  Location6_Weather, 
  Location7_Weather, 
  Location8_Weather, 
  Location9_Weather
)
```

### Preprocess Weather Data from Meteorological Service to one DF

``` r
# Remove the Time from the Timestamp column - now we having only Date
Weather_df <- Weather_df %>%
  mutate(Timestamp = dmy_hm(Timestamp) %>% # Parse the date-time string
           as_date() %>%                    # Convert to date format
           format("%Y-%m-%d"))             # Format the date as desired

# Filter only the data from 2023
Weather_df <- Weather_df %>%
  filter(year(Timestamp) == 2023)

# Covert the Date to Date Type
Weather_df <- Weather_df %>%
  mutate(Timestamp = as.Date(Timestamp))

# Get statistics for every day - now we have one row for every day for every location
Weather_df <- Weather_df %>%
  group_by(Location, Timestamp) %>%
  summarize(
    Relative_Humidity_Percent = mean(Relative_Humidity_Percent),
    Temperature_C = mean(Temperature_C),
    Max_Temperature_C = max(Max_Temperature_C),
    Min_Temperature_C = min(Min_Temperature_C),
    Rain_mm = mean(Rain_mm),
    .groups = "drop"
  )
```

### Join Weather Data from Meteorological Service With Electric Data

``` r
# Join the data frames
merged_df <- inner_join(Weather_df, Solar_df, by = c("Location", "Timestamp"))
```

## Preprocess Weather Data from Visual Crossing and Merge to one DF

### Merge and Preprocess Weather Data from Visual Crossing to one DF

``` r
# The Function is change date column do regular format of date
#
# @param df - The Data Frame
# @param date_col - The Date Column
convert_date_format <- function(df, date_col) {
  df %>%
    mutate({{ date_col }} := dmy({{ date_col }})) %>%
    mutate({{ date_col }} := format({{ date_col }}, "%Y-%m-%d"))
}

# Assign the modified data frames back to their original names
Location2_Weather2 <- convert_date_format(Location2_Weather2, datetime)
Location4_Weather2 <- convert_date_format(Location4_Weather2, datetime)
Location5_Weather2 <- convert_date_format(Location5_Weather2, datetime)
Location6_Weather2 <- convert_date_format(Location6_Weather2, datetime)
Location8_Weather2 <- convert_date_format(Location8_Weather2, datetime)
Location7_Weather2 <- convert_date_format(Location7_Weather2, datetime)
Location9_Weather2 <- convert_date_format(Location9_Weather2, datetime)

# Merge all Weather Data frames
Weather_df2 <- bind_rows(
  Location3_Weather2, 
  Location1_Weather2, 
  Location2_Weather2, 
  Location4_Weather2, 
  Location5_Weather2, 
  Location6_Weather2, 
  Location7_Weather2, 
  Location8_Weather2, 
  Location9_Weather2
)

# Covert the Date to Date Type
Weather_df2 <- Weather_df2 %>%
  mutate(datetime = as.Date(datetime))


Weather_df2 <- Weather_df2 %>%
  rename(Location = name,
         Timestamp = datetime)
```

### Join Weather Data from Meteorological Service and Electric Data with Visual Crossing

``` r
# Join the data frames
merged_df <- inner_join(Weather_df2, merged_df, by = c("Location", "Timestamp"))

merged_df <- merged_df %>%
  mutate(
    sunrise = ymd_hms(sunrise),
    sunset = ymd_hms(sunset),
    daylight = as.numeric(difftime(sunset, sunrise, units = "hours"))
  )

merged_df <- merged_df %>% select(Location,
                                  Timestamp,
                                  Daily_energy_yield_kWh,
                                  Temperature_C,
                                  Max_Temperature_C,
                                  Min_Temperature_C,
                                  Relative_Humidity_Percent,
                                  Rain_mm,
                                  Current_AC_A,
                                  Direct_Current_A,
                                  AC_Voltage_V,
                                  Voltage_DC_V,
                                  Daylight_Hours = daylight,
                                  Temperature_C_Feel_Like = feelslike,
                                  Max_Temperature_C_Fell_Like = feelslikemax,
                                  Min_Temperature_C_Feel_Like = feelslikemin,
                                  Solar_Radiation = solarradiation,
                                  UV_Index = uvindex,
                                  Wind_Speed = windspeed,
                                  Wind_Gust = windgust,
                                  Visibility = visibility,
                                  Cloud_Cover = cloudcover,
                                  Description = conditions
                                  )
```

## Missing Values Handelling

Check for NA and NULL Values

``` r
# Check for NULL values
null_values <- sapply(merged_df, function(x) sum(is.null(x)))

# Check for NA values
na_values <- sapply(merged_df, function(x) sum(is.na(x)))

# Check for complete cases
complete_cases <- sum(complete.cases(merged_df))

# Print the results:

# Null values
print(null_values)
```

    ##                    Location                   Timestamp 
    ##                           0                           0 
    ##      Daily_energy_yield_kWh               Temperature_C 
    ##                           0                           0 
    ##           Max_Temperature_C           Min_Temperature_C 
    ##                           0                           0 
    ##   Relative_Humidity_Percent                     Rain_mm 
    ##                           0                           0 
    ##                Current_AC_A            Direct_Current_A 
    ##                           0                           0 
    ##                AC_Voltage_V                Voltage_DC_V 
    ##                           0                           0 
    ##              Daylight_Hours     Temperature_C_Feel_Like 
    ##                           0                           0 
    ## Max_Temperature_C_Fell_Like Min_Temperature_C_Feel_Like 
    ##                           0                           0 
    ##             Solar_Radiation                    UV_Index 
    ##                           0                           0 
    ##                  Wind_Speed                   Wind_Gust 
    ##                           0                           0 
    ##                  Visibility                 Cloud_Cover 
    ##                           0                           0 
    ##                 Description 
    ##                           0

``` r
# NA values
print(na_values)
```

    ##                    Location                   Timestamp 
    ##                           0                           0 
    ##      Daily_energy_yield_kWh               Temperature_C 
    ##                           2                          50 
    ##           Max_Temperature_C           Min_Temperature_C 
    ##                          44                          45 
    ##   Relative_Humidity_Percent                     Rain_mm 
    ##                          53                          11 
    ##                Current_AC_A            Direct_Current_A 
    ##                           2                           2 
    ##                AC_Voltage_V                Voltage_DC_V 
    ##                           2                           2 
    ##              Daylight_Hours     Temperature_C_Feel_Like 
    ##                           0                           0 
    ## Max_Temperature_C_Fell_Like Min_Temperature_C_Feel_Like 
    ##                           0                           0 
    ##             Solar_Radiation                    UV_Index 
    ##                           0                           0 
    ##                  Wind_Speed                   Wind_Gust 
    ##                           0                           0 
    ##                  Visibility                 Cloud_Cover 
    ##                           0                           0 
    ##                 Description 
    ##                           0

``` r
# Complete cases
print(complete_cases)
```

    ## [1] 3208

Handelling Missing Values Step:

``` r
# Handelling Missing Values Step:

# Function to use the formula of the interpolation
# @param x - the var we wan to fill with interpolation
# @param y - the var we use to predict
linear_interpolation <- function(x, y) {
  na_indices <- is.na(x)
  interpolated <- approx(x = y[!na_indices], y = x[!na_indices], xout = y, rule = 2)$y
  return(interpolated)
}

# Function to interpolate missing values
# @param df - The Data Frame
interpolate_na <- function(df) {
  df %>%
    arrange(Timestamp) %>%
    mutate(
      Relative_Humidity_Percent = linear_interpolation(Relative_Humidity_Percent, Timestamp),
      Temperature_C = linear_interpolation(Temperature_C, Timestamp),
      Max_Temperature_C = linear_interpolation(Max_Temperature_C, Timestamp),
      Min_Temperature_C = linear_interpolation(Min_Temperature_C, Timestamp),
      Rain_mm = linear_interpolation(Rain_mm, Timestamp),
      Daily_energy_yield_kWh = linear_interpolation(Daily_energy_yield_kWh, Timestamp),
      Current_AC_A = linear_interpolation(Current_AC_A, Timestamp),
      Direct_Current_A = linear_interpolation(Direct_Current_A, Timestamp),
      AC_Voltage_V = linear_interpolation(AC_Voltage_V, Timestamp),
      Voltage_DC_V = linear_interpolation(Voltage_DC_V, Timestamp)
    ) %>%
    fill(Relative_Humidity_Percent, .direction = "downup") %>%
    fill(Temperature_C, .direction = "downup") %>%
    fill(Max_Temperature_C, .direction = "downup") %>%
    fill(Min_Temperature_C, .direction = "downup") %>%
    fill(Rain_mm, .direction = "downup") %>%
    fill(Daily_energy_yield_kWh, .direction = "downup")
}

# Apply interpolation function to each group (Location)
df <- merged_df %>%
  group_by(Location) %>%
  group_modify(~ interpolate_na(.x)) %>%
  ungroup()
```

Check for NA and NULL Values

``` r
# Check for NULL values
null_values <- sapply(df, function(x) sum(is.null(x)))

# Check for NA values
na_values <- sapply(df, function(x) sum(is.na(x)))

# Check for complete cases
complete_cases <- sum(complete.cases(df))

# Print the results

# Null values
print(null_values)
```

    ##                    Location                   Timestamp 
    ##                           0                           0 
    ##      Daily_energy_yield_kWh               Temperature_C 
    ##                           0                           0 
    ##           Max_Temperature_C           Min_Temperature_C 
    ##                           0                           0 
    ##   Relative_Humidity_Percent                     Rain_mm 
    ##                           0                           0 
    ##                Current_AC_A            Direct_Current_A 
    ##                           0                           0 
    ##                AC_Voltage_V                Voltage_DC_V 
    ##                           0                           0 
    ##              Daylight_Hours     Temperature_C_Feel_Like 
    ##                           0                           0 
    ## Max_Temperature_C_Fell_Like Min_Temperature_C_Feel_Like 
    ##                           0                           0 
    ##             Solar_Radiation                    UV_Index 
    ##                           0                           0 
    ##                  Wind_Speed                   Wind_Gust 
    ##                           0                           0 
    ##                  Visibility                 Cloud_Cover 
    ##                           0                           0 
    ##                 Description 
    ##                           0

``` r
# NA values
print(na_values)
```

    ##                    Location                   Timestamp 
    ##                           0                           0 
    ##      Daily_energy_yield_kWh               Temperature_C 
    ##                           0                           0 
    ##           Max_Temperature_C           Min_Temperature_C 
    ##                           0                           0 
    ##   Relative_Humidity_Percent                     Rain_mm 
    ##                           0                           0 
    ##                Current_AC_A            Direct_Current_A 
    ##                           0                           0 
    ##                AC_Voltage_V                Voltage_DC_V 
    ##                           0                           0 
    ##              Daylight_Hours     Temperature_C_Feel_Like 
    ##                           0                           0 
    ## Max_Temperature_C_Fell_Like Min_Temperature_C_Feel_Like 
    ##                           0                           0 
    ##             Solar_Radiation                    UV_Index 
    ##                           0                           0 
    ##                  Wind_Speed                   Wind_Gust 
    ##                           0                           0 
    ##                  Visibility                 Cloud_Cover 
    ##                           0                           0 
    ##                 Description 
    ##                           0

``` r
# Complete cases
print(complete_cases)
```

    ## [1] 3281

## Feature Engineering

### Season Column

Create Season Column and add Dummy variables

``` r
# Create Season column by the number of the month - From Wikipedia
df <- df %>%
  mutate(Timestamp = as.Date(Timestamp),
         Month = month(Timestamp),
         Season = case_when(
           Month %in% 3:5 ~ "Spring",
           Month %in% 6:8 ~ "Summer",
           Month %in% 9:11 ~ "Autumn",
           TRUE ~ "Winter"
         ))

# Create a function to Make dummy variables
#
# @param df - The Data Frame
# @param column_name - the column names to be dummy
create_dummy_variables <- function(df, column_name) {
  # Ensure the specified column is a factor
  df[[column_name]] <- factor(df[[column_name]])
  
  # Create dummy variables using model.matrix
  dummies <- model.matrix(~ . - 1, data = df[, column_name, drop = FALSE])
  
  # Convert the matrix to a data frame and rename columns appropriately
  dummy_df <- as.data.frame(dummies)
  colnames(dummy_df) <- gsub("^\\.", paste0(column_name, "_"), colnames(dummy_df))
  
  # Combine the original dataframe with the dummy variables
  df <- cbind(df, dummy_df)
  
  return(df)
}

# Example usage for the "Season" column
df <- create_dummy_variables(df, "Season")
```

### Altitude Column

Create Altitudes Column

``` r
# Merge altitude data with df based on Location
df <- merge(df, Altitudes, by = "Location", all.x = TRUE)
```

### Area Column

Create Area Column and add Dummy variables

``` r
# Create Area column based on Location - From Google Maps
df$Area <- ifelse(df$Location %in% c("Location9", "Location8", "Location2"), "North",
                         ifelse(df$Location %in% c("Location6", "Location7"), "Center",
                                ifelse(df$Location %in% c("Location3", "Location4", "Location5", "Location1"), "South", NA)))

# Create a function to Make dummy variables

df <- create_dummy_variables(df, "Area")
```

### Temperature Range

Create Temperature Range Column

``` r
# Create a new variable for temperature ranges
df$Temperature_Range <- cut(df$Temperature_C, breaks = 5, labels = c("Very Low", "Low", "Medium", "High", "Very High"))
```

# Data Analysis

## Visualization

``` r
# Temperature vs. Daily Energy Yield
  ggplot(df, aes(x = Temperature_C, y = Daily_energy_yield_kWh)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +
    labs(title = "Correlation between Daily Energy Yield and Temperature",
         x = "Temperature (°C)",
         y = "Daily Energy Yield (kWh)")
```

![](README_files/figure-gfm/Correlation%20between%20Daily%20Energy%20Yield%20and%20Temperature-1.png)<!-- -->

``` r
# Rain vs. Daily Energy Yield
ggplot(df, aes(x = Rain_mm, y = Daily_energy_yield_kWh)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(
    x = "Rain (mm)",
    y = "Daily Energy Yield (kWh)",
    title = "Linear Regression: Energy Yield vs Rain",
    subtitle = "Assessing the Impact of Rain on Energy Yield"
  ) +
  theme_minimal() +
  xlim(0, 0.4) +
  ylim(0, 8)
```

![](README_files/figure-gfm/Linear%20Regression%20Energy%20Yield%20vs%20Rain-1.png)<!-- -->

``` r
# Cloud Cover vs. Daily Energy Yield
suppressWarnings(suppressMessages(ggplot(df, aes(x = Cloud_Cover, y = Daily_energy_yield_kWh)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Effect of Cloud Cover on Daily Energy Yield",
       x = "Cloud Cover (%)",
       y = "Daily Energy Yield (kWh)")))
```

![](README_files/figure-gfm/Effect%20of%20Cloud%20Cover%20on%20Daily%20Energy%20Yield-1.png)<!-- -->

``` r
# Wind Speed vs. Daily Energy Yield
ggplot(df, aes(x = Wind_Speed, y = Daily_energy_yield_kWh)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Wind Speed vs. Daily Energy Yield",
       x = "Wind Speed (m/s)",
       y = "Daily Energy Yield (kWh)")
```

![](README_files/figure-gfm/Wind%20Speed%20vs%20Daily%20Energy%20Yield-1.png)<!-- -->

``` r
# Solar Radiation vs. Daily Energy Yield
suppressWarnings(suppressMessages(ggplot(df, aes(x = Solar_Radiation, y = Daily_energy_yield_kWh)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Solar Radiation vs. Daily Energy Yield",
       x = "Solar Radiation",
       y = "Daily Energy Yield (kWh)")))
```

![](README_files/figure-gfm/Solar%20Radiation%20vs.%20Daily%20Energy%20Yield-1.png)<!-- -->

``` r
# Impact of Altitude on Daily Energy Yield
ggplot(df, aes(x = Altitude, y = Daily_energy_yield_kWh)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Impact of Altitude on Daily Energy Yield",
       x = "Altitude",
       y = "Daily Energy Yield (kWh)")
```

![](README_files/figure-gfm/Impact%20of%20Altitude%20on%20Daily%20Energy%20Yield-1.png)<!-- -->

``` r
# Relationship between UV Index and Daily Energy Yield
ggplot(df, aes(x = UV_Index, y = Daily_energy_yield_kWh)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Relationship between UV Index and Daily Energy Yield",
       x = "UV Index",
       y = "Daily Energy Yield (kWh)")
```

![](README_files/figure-gfm/Relationship%20between%20UV%20Index%20and%20Daily%20Energy%20Yield-1.png)<!-- -->

``` r
# Histogram for Daily Energy Yield with respect to Temperature Ranges
ggplot(df, aes(x = Temperature_Range, y = Daily_energy_yield_kWh)) +
  geom_bar(stat = "identity", fill = "blue", color = df$Temperature_Range, alpha = 0.7) +
  labs(
    x = "Temperature Range",
    y = "Daily Energy Yield (kWh)",
    title = "Histogram of Daily Energy Yield",
    subtitle = "across Temperature Ranges"
  ) +
  theme_minimal()
```

![](README_files/figure-gfm/Histogram%20of%20Energy%20Yield-1.png)<!-- -->

``` r
# Filter the dataset for specific weather descriptions
filtered_df <- df %>%
  filter(Description %in% c("Clear", "Partially cloudy", "Rain, Partially cloudy"))


# Create the violin plot with custom colors
ggplot(filtered_df, aes(x = Description, y = Daily_energy_yield_kWh, fill = Description)) +
  geom_violin(trim = FALSE) +
  scale_fill_manual(values = c("Clear" = "#9ACD32", "Partially cloudy" = "red", "Rain, Partially cloudy" = "#87CEEB")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Energy Yield Distribution by Weather Description", x = NULL, y = "Daily Energy Yield (kWh)") +
    theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
```

![](README_files/figure-gfm/Energy%20Yield%20Distribution%20by%20Weather%20Description-1.png)<!-- -->

``` r
library(corrplot)
# Calculate the correlation matrix
cor_matrix <- cor(df[, c("Daily_energy_yield_kWh", "Temperature_C", "Relative_Humidity_Percent", "Solar_Radiation", "UV_Index", "Rain_mm", "Wind_Speed", "Cloud_Cover")])

corrplot(cor_matrix, method = "color", type = "full", addrect = 4, 
         tl.col = "black")
```

![](README_files/figure-gfm/Correlation%20Matrix-1.png)<!-- -->

``` r
library(ggridges)

# Ridgeline Plot
ggplot(df, aes(x = Daily_energy_yield_kWh, y = Season, fill = Season)) +
  geom_density_ridges(alpha = 0.8) +
  labs(title = "Ridgeline Plot of Energy Yield by Season", x = "Daily Energy Yield (kWh)", y = "Season")
```

![](README_files/figure-gfm/Ridgeline%20Plot%20of%20Energy%20Yield%20by%20Season-1.png)<!-- -->

``` r
# Scatter plot with regression line
ggplot(df, aes(x = Temperature_C, y = Daily_energy_yield_kWh, color = Area)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  labs(title = "Energy Yield vs Temperature", x = "Temperature (C)", y = "Daily Energy Yield (kWh)") + 
  facet_wrap(~ Season)
```

![](README_files/figure-gfm/Energy%20Yield%20vs%20Temperature%20With%20Season-1.png)<!-- -->

## Linear Regression

``` r
# Load necessary libraries
suppressMessages(library(tidymodels))
library(broom)

# Fix random numbers by setting the seed 
# Enables analysis to be reproducible when random numbers are used 
set.seed(93)

# Put 75% of the data into the training set 
df_split <- initial_split(df, prop = 0.75)

# Create data frames for the two sets:
train_data <- training(df_split)
test_data  <- testing(df_split)
```

### Regular Linear Regression for Weather Features

``` r
# Fit the linear regression
df_fit <- linear_reg() %>% 
  set_engine("lm") %>% 
  fit(Daily_energy_yield_kWh ~ Temperature_C + Relative_Humidity_Percent + Rain_mm + Solar_Radiation + UV_Index + Wind_Speed + Wind_Gust + Cloud_Cover, data = train_data)

# Extract the fitted model object
lm_fit <- df_fit$fit

# Get the tidy coefficients
tidy_coefs <- tidy(lm_fit)
print(tidy_coefs)
```

    ## # A tibble: 9 × 5
    ##   term                      estimate std.error statistic  p.value
    ##   <chr>                        <dbl>     <dbl>     <dbl>    <dbl>
    ## 1 (Intercept)                1.75     0.208        8.42  6.09e-17
    ## 2 Temperature_C              0.00531  0.00489      1.09  2.77e- 1
    ## 3 Relative_Humidity_Percent -0.00159  0.00176     -0.904 3.66e- 1
    ## 4 Rain_mm                   -3.10     0.756       -4.10  4.20e- 5
    ## 5 Solar_Radiation            0.00688  0.000820     8.39  8.41e-17
    ## 6 UV_Index                   0.124    0.0325       3.81  1.42e- 4
    ## 7 Wind_Speed                -0.0105   0.00458     -2.29  2.20e- 2
    ## 8 Wind_Gust                  0.0132   0.00234      5.63  1.99e- 8
    ## 9 Cloud_Cover               -0.0148   0.00132    -11.3   9.33e-29

``` r
# Get the model summary statistics
model_summary <- glance(lm_fit)

# Print the model summary statistics
print(model_summary)
```

    ## # A tibble: 1 × 12
    ##   r.squared adj.r.squared sigma statistic   p.value    df logLik   AIC   BIC
    ##       <dbl>         <dbl> <dbl>     <dbl>     <dbl> <dbl>  <dbl> <dbl> <dbl>
    ## 1     0.460         0.458  1.15      261. 3.29e-321     8 -3837. 7694. 7753.
    ## # ℹ 3 more variables: deviance <dbl>, df.residual <int>, nobs <int>

``` r
# Extract RMSE from residuals
rmse <- sqrt(mean(lm_fit$residuals^2))

# Print RMSE
cat("RMSE: ", rmse, "\n")
```

    ## RMSE:  1.151319

### Fixed Model Linear Regression for Weather Features

``` r
# Load necessary libraries
library(plm)

# Convert the data to a tibble
df_tibble <- as_tibble(df)

# Estimate fixed effects model using plm
fixed_model_weather <- plm(Daily_energy_yield_kWh ~ Temperature_C + Relative_Humidity_Percent + Rain_mm + Solar_Radiation + UV_Index + Wind_Speed + Wind_Gust + Cloud_Cover + factor(Location),
                   data = df_tibble,
                   model = "within")

# Summary of the model
summary(fixed_model_weather)
```

    ## Oneway (individual) effect Within Model
    ## 
    ## Call:
    ## plm(formula = Daily_energy_yield_kWh ~ Temperature_C + Relative_Humidity_Percent + 
    ##     Rain_mm + Solar_Radiation + UV_Index + Wind_Speed + Wind_Gust + 
    ##     Cloud_Cover + factor(Location), data = df_tibble, model = "within")
    ## 
    ## Unbalanced Panel: n = 9, T = 361-365, N = 3281
    ## 
    ## Residuals:
    ##      Min.   1st Qu.    Median   3rd Qu.      Max. 
    ## -3.581618 -0.316978  0.052145  0.345381  2.584073 
    ## 
    ## Coefficients:
    ##                              Estimate  Std. Error  t-value  Pr(>|t|)    
    ## Temperature_C             -0.02356385  0.00226916 -10.3844 < 2.2e-16 ***
    ## Relative_Humidity_Percent -0.00447790  0.00094271  -4.7500 2.122e-06 ***
    ## Rain_mm                   -3.30848247  0.35362008  -9.3560 < 2.2e-16 ***
    ## Solar_Radiation            0.01078620  0.00035393  30.4754 < 2.2e-16 ***
    ## UV_Index                   0.08952409  0.01384091   6.4681 1.141e-10 ***
    ## Wind_Speed                -0.01450594  0.00228327  -6.3532 2.402e-10 ***
    ## Wind_Gust                  0.00759537  0.00130118   5.8373 5.825e-09 ***
    ## Cloud_Cover               -0.00857176  0.00057556 -14.8929 < 2.2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Total Sum of Squares:    5078.6
    ## Residual Sum of Squares: 1049.3
    ## R-Squared:      0.79339
    ## Adj. R-Squared: 0.79237
    ## F-statistic: 1566.7 on 8 and 3264 DF, p-value: < 2.22e-16

``` r
# Extract residuals
residuals <- residuals(fixed_model_weather)

# Calculate RMSE
rmse <- sqrt(mean(residuals^2))

# Print RMSE
cat("RMSE: ", rmse)
```

    ## RMSE:  0.5655196

### Fixed Model Linear Regression for Location Features

``` r
# Load necessary libraries
library(plm)


# Convert the data to a tibble
df_tibble <- as_tibble(df)

# Estimate fixed effects model using plm
fixed_model_area <- plm(Daily_energy_yield_kWh ~ Altitude + AreaSouth + AreaCenter + AreaNorth + Daylight_Hours + factor(Location),
                   data = df_tibble,
                   model = "within")

# Summary of the model
summary(fixed_model_area)
```

    ## Oneway (individual) effect Within Model
    ## 
    ## Call:
    ## plm(formula = Daily_energy_yield_kWh ~ Altitude + AreaSouth + 
    ##     AreaCenter + AreaNorth + Daylight_Hours + factor(Location), 
    ##     data = df_tibble, model = "within")
    ## 
    ## Unbalanced Panel: n = 9, T = 361-365, N = 3281
    ## 
    ## Residuals:
    ##     Min.  1st Qu.   Median  3rd Qu.     Max. 
    ## -3.96679 -0.36329  0.20833  0.55547  1.61216 
    ## 
    ## Coefficients:
    ##                Estimate Std. Error t-value  Pr(>|t|)    
    ## Daylight_Hours  0.63858    0.01033  61.819 < 2.2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Total Sum of Squares:    5078.6
    ## Residual Sum of Squares: 2342.2
    ## R-Squared:      0.53881
    ## Adj. R-Squared: 0.53754
    ## F-statistic: 3821.52 on 1 and 3271 DF, p-value: < 2.22e-16

``` r
# Extract residuals
residuals <- residuals(fixed_model_area)

# Calculate RMSE
rmse <- sqrt(mean(residuals^2))

# Print RMSE
cat("RMSE: ", rmse)
```

    ## RMSE:  0.8449048

### Combined Linear Regression for Weather Features & Location Features

Check the proportions for the 2 Linear Regression Models

``` r
# Load necessary libraries
library(caret) # For RMSE calculation

# Assuming you have a dataset df with predictors and response variable 'Daily_energy_yield_kWh'
# Ensure the new data frame new_df is properly structured
new_df <- df  # Here, replace with your actual new data

# Fit the two models
model1 <- lm(Daily_energy_yield_kWh ~ Temperature_C + Relative_Humidity_Percent + Rain_mm + Daylight_Hours + Solar_Radiation + UV_Index + Wind_Speed + Wind_Gust + Cloud_Cover, data = df)
model2 <- lm(Daily_energy_yield_kWh ~ Location + Area + Altitude, data = df)

# Make predictions using both models
pred1 <- predict(model1, newdata = new_df)
pred2 <- predict(model2, newdata = new_df)

# Initialize a data frame to store the results
results <- data.frame(
  w1 = numeric(),
  w2 = numeric(),
  RMSE = numeric(),
  R_squared = numeric(),
  Adjusted_R_squared = numeric(),
  P_value = numeric()
)

# Loop over possible values of w1 and w2
for (w1 in seq(0, 1, by = 0.05)) {
  w2 <- 1 - w1
  combined_pred <- w1 * pred1 + w2 * pred2
  
  # Calculate residuals
  residuals_combined <- df$Daily_energy_yield_kWh - combined_pred
  
  # Calculate RMSE
  rmse_combined <- sqrt(mean(residuals_combined^2))
  
  # Calculate R-squared
  sst <- sum((df$Daily_energy_yield_kWh - mean(df$Daily_energy_yield_kWh))^2)
  sse <- sum(residuals_combined^2)
  r_squared_combined <- 1 - (sse / sst)
  
  # Calculate adjusted R-squared
  n <- nrow(df)
  p <- length(coef(model1)) + length(coef(model2)) - 2 # subtract 2 because we combine two intercepts into one
  adj_r_squared_combined <- 1 - ((1 - r_squared_combined) * (n - 1) / (n - p - 1))
  
  # Calculate p-value using an F-test
  f_statistic <- (sst - sse) / p / (sse / (n - p - 1))
  p_value_combined <- pf(f_statistic, p, n - p - 1, lower.tail = FALSE)
  
  # Store the results
  results <- rbind(results, data.frame(
    w1_Weather = w1,
    w2_Area = w2,
    RMSE = rmse_combined,
    R_squared = r_squared_combined,
    Adjusted_R_squared = adj_r_squared_combined,
    P_value = p_value_combined
  ))
}

# Print results
print(results)
```

    ##    w1_Weather w2_Area      RMSE R_squared Adjusted_R_squared       P_value
    ## 1        0.00    1.00 1.2441360 0.3582153          0.3542780 2.501085e-295
    ## 2        0.05    0.95 1.1987182 0.4042174          0.4005623  0.000000e+00
    ## 3        0.10    0.90 1.1558552 0.4460629          0.4426645  0.000000e+00
    ## 4        0.15    0.85 1.1158414 0.4837518          0.4805846  0.000000e+00
    ## 5        0.20    0.80 1.0789940 0.5172840          0.5143226  0.000000e+00
    ## 6        0.25    0.75 1.0456476 0.5466597          0.5438785  0.000000e+00
    ## 7        0.30    0.70 1.0161470 0.5718788          0.5692523  0.000000e+00
    ## 8        0.35    0.65 0.9908358 0.5929413          0.5904440  0.000000e+00
    ## 9        0.40    0.60 0.9700421 0.6098472          0.6074536  0.000000e+00
    ## 10       0.45    0.55 0.9540611 0.6225965          0.6202811  0.000000e+00
    ## 11       0.50    0.50 0.9431375 0.6311891          0.6289265  0.000000e+00
    ## 12       0.55    0.45 0.9374483 0.6356252          0.6333898  0.000000e+00
    ## 13       0.60    0.40 0.9370887 0.6359047          0.6336710  0.000000e+00
    ## 14       0.65    0.35 0.9420649 0.6320276          0.6297701  0.000000e+00
    ## 15       0.70    0.30 0.9522931 0.6239939          0.6216871  0.000000e+00
    ## 16       0.75    0.25 0.9676069 0.6118036          0.6094220  0.000000e+00
    ## 17       0.80    0.20 0.9877697 0.5954567          0.5929748  0.000000e+00
    ## 18       0.85    0.15 1.0124920 0.5749532          0.5723455  0.000000e+00
    ## 19       0.90    0.10 1.0414490 0.5502930          0.5475341  0.000000e+00
    ## 20       0.95    0.05 1.0742983 0.5214763          0.5185406  0.000000e+00
    ## 21       1.00    0.00 1.1106947 0.4885030          0.4853650  0.000000e+00

The selected relations are: 0.6 to the Weather Regression and 0.4 to the
Area Regression

``` r
library(caret) # For RMSE calculation

# Assuming you have a dataset df with predictors and response variable 'y'
# Fit the two models
model1 <- lm(Daily_energy_yield_kWh ~ Temperature_C + Relative_Humidity_Percent + Rain_mm + Daylight_Hours + Solar_Radiation + UV_Index + Wind_Speed + Wind_Gust + Cloud_Cover, data = df)
model2 <- lm(Daily_energy_yield_kWh ~ Location + Area + Altitude, data = df)

# Make predictions using both models
pred1 <- predict(model1, newdata = new_df)
pred2 <- predict(model2, newdata = new_df)

# Combine predictions using weights (w1 and w2)
w1 <- 0.6
w2 <- 0.4
combined_pred <- w1 * pred1 + w2 * pred2

# Calculate residuals
residuals_combined <- df$Daily_energy_yield_kWh - combined_pred

# Calculate RMSE
rmse_combined <- sqrt(mean(residuals_combined^2))

# Calculate R-squared
sst <- sum((df$Daily_energy_yield_kWh - mean(df$Daily_energy_yield_kWh))^2)
sse <- sum(residuals_combined^2)
r_squared_combined <- 1 - (sse / sst)

# Calculate adjusted R-squared
n <- nrow(df)
p <- length(coef(model1)) + length(coef(model2)) - 2 # subtract 2 because we combine two intercepts into one
adj_r_squared_combined <- 1 - ((1 - r_squared_combined) * (n - 1) / (n - p - 1))

# Calculate p-value using an F-test
f_statistic <- (sst - sse) / p / (sse / (n - p - 1))
p_value_combined <- pf(f_statistic, p, n - p - 1, lower.tail = FALSE)

# Print results with high precision for p-value
cat("RMSE:", rmse_combined, "\n")
```

    ## RMSE: 0.9370887

``` r
cat("R-squared:", r_squared_combined, "\n")
```

    ## R-squared: 0.6359047

``` r
cat("Adjusted R-squared:", adj_r_squared_combined, "\n")
```

    ## Adjusted R-squared: 0.633671

``` r
cat(sprintf("P-value: %.10f\n", p_value_combined))  # Print p-value with high precision
```

    ## P-value: 0.0000000000

## Bootstrap

``` r
# Load necessary libraries
library(tidyverse)
library(broom)
library(rsample)

# Function to fit model and extract coefficients and metrics
fit_model <- function(split) {
  model <- lm(Daily_energy_yield_kWh ~ Temperature_C + Relative_Humidity_Percent + Rain_mm + Solar_Radiation + UV_Index + Wind_Speed + Wind_Gust + Cloud_Cover, data = analysis(split))
  coefs <- tidy(model)
  metrics <- data.frame(
    term = c("R2", "adj_R2", "RMSE"),
    estimate = c(summary(model)$r.squared,
                 summary(model)$adj.r.squared,
                 sqrt(mean(residuals(model)^2)))
  )
  list(coefs = coefs, metrics = metrics)
}

# Perform bootstrapping
set.seed(123)  # for reproducibility
boot_samples <- bootstraps(df, times = 15000)

# Fit model to each bootstrap sample and collect results
boot_results <- boot_samples %>%
  mutate(results = map(splits, fit_model))

# Extract coefficients and metrics
boot_coefs <- boot_results %>%
  unnest_wider(results) %>%
  unnest(coefs)

boot_metrics <- boot_results %>%
  unnest_wider(results) %>%
  unnest(metrics)
```

``` r
# Plot distribution of coefficients
ggplot(boot_coefs, aes(x = estimate)) +
  geom_histogram(bins = 30) +
  facet_wrap(~ term, scales = "free", ncol = 3) +
  labs(title = "Bootstrap distributions of regression coefficients")
```

![](README_files/figure-gfm/Bootstrap%20distributions%20of%20regression%20coefficients-1.png)<!-- -->

``` r
# Plot distribution of metrics
ggplot(boot_metrics, aes(x = estimate)) +
  geom_histogram(bins = 30) +
  facet_wrap(~ term, scales = "free") +
  labs(title = "Bootstrap distributions of model metrics")
```

![](README_files/figure-gfm/Bootstrap%20distributions%20of%20model%20metrics-1.png)<!-- -->

``` r
# Calculate confidence intervals for coefficients
ci_coefs <- boot_coefs %>%
  group_by(term) %>%
  summarize(
    mean_estimate = mean(estimate, na.rm = TRUE),
    lower_ci = quantile(estimate, 0.025, na.rm = TRUE),
    upper_ci = quantile(estimate, 0.975, na.rm = TRUE)
  )

# Calculate confidence intervals for metrics
ci_metrics <- boot_metrics %>%
  group_by(term) %>%
  summarize(
    mean_estimate = mean(estimate, na.rm = TRUE),
    lower_ci = quantile(estimate, 0.025, na.rm = TRUE),
    upper_ci = quantile(estimate, 0.975, na.rm = TRUE)
  )

print(ci_coefs)
```

    ## # A tibble: 9 × 4
    ##   term                      mean_estimate lower_ci upper_ci
    ##   <chr>                             <dbl>    <dbl>    <dbl>
    ## 1 (Intercept)                     1.74     1.40     2.06   
    ## 2 Cloud_Cover                    -0.0146  -0.0168  -0.0123 
    ## 3 Rain_mm                        -3.65    -5.78    -1.97   
    ## 4 Relative_Humidity_Percent      -0.00121 -0.00421  0.00176
    ## 5 Solar_Radiation                 0.00686  0.00549  0.00821
    ## 6 Temperature_C                   0.00664 -0.00121  0.0147 
    ## 7 UV_Index                        0.114    0.0652   0.163  
    ## 8 Wind_Gust                       0.0141   0.0110   0.0171 
    ## 9 Wind_Speed                     -0.00972 -0.0165  -0.00271

``` r
print(ci_metrics)
```

    ## # A tibble: 3 × 4
    ##   term   mean_estimate lower_ci upper_ci
    ##   <chr>          <dbl>    <dbl>    <dbl>
    ## 1 R2             0.456    0.429    0.482
    ## 2 RMSE           1.15     1.12     1.17 
    ## 3 adj_R2         0.454    0.428    0.481
