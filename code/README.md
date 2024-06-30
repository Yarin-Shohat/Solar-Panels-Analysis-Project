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
  - [Feature Engineering](#feature-engineering)
    - [Season Column](#season-column)
    - [Altitude Column](#altitude-column)
    - [Area Column](#area-column)
    - [Temperature Range](#temperature-range)
  - [Missing Values Handelling](#missing-values-handelling)
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
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ## ✔ ggplot2   3.5.1     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.3     ✔ tidyr     1.3.1
    ## ✔ purrr     1.0.2     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(readxl)
```

## Read Data

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
  suppressWarnings({ Revivim <- read_excel("../data/Revivim.xlsx") })
  suppressWarnings({ Nir_David <- read_excel("../data/Nir David.xlsx") })
  suppressWarnings({ Ashkelon <- read_excel("../data/Ashkelon.xlsx") })
  suppressWarnings({ Beer_Sheva <- read_excel("../data/Beer Sheva.xlsx") })
  suppressWarnings({ Elifaz <- read_excel("../data/Elifaz.xlsx") })
  suppressWarnings({ Emek_Hefer <- read_excel("../data/Emek Hefer.xlsx") })
  suppressWarnings({ Jerusalem <- read_excel("../data/Jerusalem.xlsx") })
  suppressWarnings({ Kfar_Rupin <- read_excel("../data/Kfar Rupin.xlsx") })
  suppressWarnings({ Naharia <- read_excel("../data/Naharia.xlsx") })
})
```

### Read Weather Data from Meteorological Service

``` r
# Read Weather Data from Meteorological Service
Ashkelon_Weather <- suppressWarnings(read_excel("../data/Ashkelon_Weather.xlsx"))
Beer_Sheva_Weather <- suppressWarnings(read_excel("../data/Beer_Sheva_Weather.xlsx"))
Elifaz_Weather <- suppressWarnings(read_excel("../data/Elifaz_Weather.xlsx"))
Emek_Hefer_Weather <- suppressWarnings(read_excel("../data/Emek_Hefer_Weather.xlsx"))
Jerusalem_Weather <- suppressWarnings(read_excel("../data/Jerusalem_Weather.xlsx"))
Kfar_Rupin_Weather <- suppressWarnings(read_excel("../data/Kfar_Rupin_Weather.xlsx"))
Naharia_Weather <- suppressWarnings(read_excel("../data/Naharia_Weather.xlsx"))
Nir_David_Weather <- suppressWarnings(read_excel("../data/Nir_David_Weather.xlsx"))
Revivim_Weather <- suppressWarnings(read_excel("../data/Revivim_Weather.xlsx"))
```

### Read Weather Data from Visual Crossing

``` r
# Read Weather Data from Visual Crossing
Revivim_Weather2 <- read.csv("../data/Revivim 2023-01-01 to 2023-12-31.csv")
Nir_David_Weather2 <- read.csv("../data/Nir David 2023-01-01 to 2023-12-31.csv")
Ashkelon_Weather2 <- read.csv("../data/Ashkelon 2023-01-01 to 2023-12-31.csv")
Beer_Sheva_Weather2 <- read.csv("../data/Beer Sheva 2023-01-01 to 2023-12-31.csv")
Elifaz_Weather2 <- read.csv("../data/Elifaz 2023-01-01 to 2023-12-31.csv")
Emek_Hefer_Weather2 <- read.csv("../data/Emek Hefer 2023-01-01 to 2023-12-31.csv")
Jerusalem_Weather2 <- read.csv("../data/Jerusalem 2023-01-01 to 2023-12-31.csv")
Kfar_Rupin_Weather2 <- read.csv("../data/Kfar Rupin 2023-01-01 to 2023-12-31.csv")
Naharia_Weather2 <- read.csv("../data/Naharia 2023-01-01 to 2023-12-31.csv")
```

## Preprocess Electric Data and Merge to one DF

### Preprocess Electric Data

``` r
# Select the columns we need from Solar Data
Ashkelon <- Ashkelon %>% 
  select(Timestamp,
         "Ashkelon - inverter 2 south -  [] - E_INT_N",
         "Ashkelon - inverter 2 south - Current AC [A] - I_AC",
         "Ashkelon - inverter 2 south - Direct Current [A] - I_DC",
         "Ashkelon - inverter 2 south - AC Voltage [V] - U_AC",
         "Ashkelon - inverter 2 south - Voltage DC [V] - U_DC")

Revivim <- Revivim %>% 
  select(Timestamp,
         "Revivim - INV 4 2823 - Daily energy yield [kWh] - E_INT_N",
         "Revivim - INV 4 2823 - Current AC [A] - I_AC",
         "Revivim - INV 4 2823 - Direct Current [A] - I_DC",
         "Revivim - INV 4 2823 - AC Voltage [V] - U_AC",
         "Revivim - INV 4 2823 - Voltage DC [V] - U_DC")

Nir_David <- Nir_David %>% select(Timestamp,
                            "Nir David - INV 2 -  [] - E_INT_N",
                            "Nir David - INV 2 - Current AC [A] - I_AC",
                            "Nir David - INV 2 - Direct Current [A] - I_DC",
                            "Nir David - INV 2 - AC Voltage [V] - U_AC",
                            "Nir David - INV 2 - Voltage DC [V] - U_DC")

Beer_Sheva <- Beer_Sheva %>% select(Timestamp,
                            "Beer Sheva - SMA STP50-41 3020126427 -  [] - E_INT_N",
                            "Beer Sheva - SMA STP50-41 3020126427 - Current AC [A] - I_AC",
                            "Beer Sheva - SMA STP50-41 3020126427 - Direct Current [A] - I_DC",
                            "Beer Sheva - SMA STP50-41 3020126427 - AC Voltage [V] - U_AC",
                            "Beer Sheva - SMA STP50-41 3020126427 - Voltage DC [V] - U_DC")

Elifaz <- Elifaz %>% select(Timestamp,
                            "Elifaz - 1900733075 -  [] - E_INT_N",
                            "Elifaz - 1900733075 - Current AC [A] - I_AC",
                            "Elifaz - 1900733075 - Direct Current [A] - I_DC",
                            "Elifaz - 1900733075 - AC Voltage [V] - U_AC",
                            "Elifaz - 1900733075 - Voltage DC [V] - U_DC")

Emek_Hefer <- Emek_Hefer %>% select(Timestamp,
                            "Emek Hefer -  inv 2 - Daily energy yield [kWh] - E_INT_N",
                            "Emek Hefer -  inv 2 - Current AC [A] - I_AC",
                            "Emek Hefer -  inv 2 - Direct Current [A] - I_DC",
                            "Emek Hefer -  inv 2 - AC Voltage [V] - U_AC",
                            "Emek Hefer -  inv 2 - Voltage DC [V] - U_DC")


Jerusalem <- Jerusalem %>% select(Timestamp,
                            "Jerusalem - SolarEdge SE50K-IL00IBNQ4 7E04A712 1 -  [] - E_INT_N",
                            "Jerusalem - SolarEdge SE50K-IL00IBNQ4 7E04A712 1 - Current AC [A] - I_AC",
                            "Jerusalem - SolarEdge SE50K-IL00IBNQ4 7E04A712 1 - Direct Current [A] - I_DC",
                            "Jerusalem - SolarEdge SE50K-IL00IBNQ4 7E04A712 1 - AC Voltage [V] - U_AC",
                            "Jerusalem - SolarEdge SE50K-IL00IBNQ4 7E04A712 1 - Voltage DC [V] - U_DC")


Kfar_Rupin <- Kfar_Rupin %>% select(Timestamp,
                            "Kfar Ruppin - Floating  - station 1 inverter 14 -  [] - E_INT_N",
                            "Kfar Ruppin - Floating  - station 1 inverter 14 - Current AC [A] - I_AC",
                            "Kfar Ruppin - Floating  - station 1 inverter 14 - Direct Current [A] - I_DC",
                            "Kfar Ruppin - Floating  - station 1 inverter 14 - AC Voltage [V] - U_AC",
                            "Kfar Ruppin - Floating  - station 1 inverter 14 - Voltage DC [V] - U_DC")

Naharia <- Naharia %>% select(Timestamp,
                            "Naharia - Inv 1 -  [] - E_INT_N",
                            "Naharia - Inv 1 - Current AC [A] - I_AC",
                            "Naharia - Inv 1 - Direct Current [A] - I_DC",
                            "Naharia - Inv 1 - AC Voltage [V] - U_AC",
                            "Naharia - Inv 1 - Voltage DC [V] - U_DC")

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
ashkelon <- process_df(Ashkelon, "Ashkelon", 
                       "Ashkelon - inverter 2 south -  [] - E_INT_N", 
                       "Ashkelon - inverter 2 south - Current AC [A] - I_AC", 
                       "Ashkelon - inverter 2 south - Direct Current [A] - I_DC", 
                       "Ashkelon - inverter 2 south - AC Voltage [V] - U_AC", 
                       "Ashkelon - inverter 2 south - Voltage DC [V] - U_DC")

revivim <- process_df(Revivim, "Revivim", 
                      "Revivim - INV 4 2823 - Daily energy yield [kWh] - E_INT_N", 
                      "Revivim - INV 4 2823 - Current AC [A] - I_AC", 
                      "Revivim - INV 4 2823 - Direct Current [A] - I_DC", 
                      "Revivim - INV 4 2823 - AC Voltage [V] - U_AC", 
                      "Revivim - INV 4 2823 - Voltage DC [V] - U_DC")

nir_david <- process_df(Nir_David, "Nir_David", 
                        "Nir David - INV 2 -  [] - E_INT_N", 
                        "Nir David - INV 2 - Current AC [A] - I_AC", 
                        "Nir David - INV 2 - Direct Current [A] - I_DC", 
                        "Nir David - INV 2 - AC Voltage [V] - U_AC", 
                        "Nir David - INV 2 - Voltage DC [V] - U_DC")

beer_sheva <- process_df(Beer_Sheva, "Beer_Sheva", 
                         "Beer Sheva - SMA STP50-41 3020126427 -  [] - E_INT_N", 
                         "Beer Sheva - SMA STP50-41 3020126427 - Current AC [A] - I_AC", 
                         "Beer Sheva - SMA STP50-41 3020126427 - Direct Current [A] - I_DC", 
                         "Beer Sheva - SMA STP50-41 3020126427 - AC Voltage [V] - U_AC", 
                         "Beer Sheva - SMA STP50-41 3020126427 - Voltage DC [V] - U_DC")

elifaz <- process_df(Elifaz, "Elifaz", 
                     "Elifaz - 1900733075 -  [] - E_INT_N", 
                     "Elifaz - 1900733075 - Current AC [A] - I_AC", 
                     "Elifaz - 1900733075 - Direct Current [A] - I_DC", 
                     "Elifaz - 1900733075 - AC Voltage [V] - U_AC", 
                     "Elifaz - 1900733075 - Voltage DC [V] - U_DC")

emek_hefer <- process_df(Emek_Hefer, "Emek_Hefer", 
                         "Emek Hefer -  inv 2 - Daily energy yield [kWh] - E_INT_N", 
                         "Emek Hefer -  inv 2 - Current AC [A] - I_AC", 
                         "Emek Hefer -  inv 2 - Direct Current [A] - I_DC", 
                         "Emek Hefer -  inv 2 - AC Voltage [V] - U_AC", 
                         "Emek Hefer -  inv 2 - Voltage DC [V] - U_DC")

jerusalem <- process_df(Jerusalem, "Jerusalem", 
                        "Jerusalem - SolarEdge SE50K-IL00IBNQ4 7E04A712 1 -  [] - E_INT_N", 
                        "Jerusalem - SolarEdge SE50K-IL00IBNQ4 7E04A712 1 - Current AC [A] - I_AC", 
                        "Jerusalem - SolarEdge SE50K-IL00IBNQ4 7E04A712 1 - Direct Current [A] - I_DC", 
                        "Jerusalem - SolarEdge SE50K-IL00IBNQ4 7E04A712 1 - AC Voltage [V] - U_AC", 
                        "Jerusalem - SolarEdge SE50K-IL00IBNQ4 7E04A712 1 - Voltage DC [V] - U_DC")

kfar_rupin <- process_df(Kfar_Rupin, "Kfar_Rupin", 
                         "Kfar Ruppin - Floating  - station 1 inverter 14 -  [] - E_INT_N", 
                         "Kfar Ruppin - Floating  - station 1 inverter 14 - Current AC [A] - I_AC", 
                         "Kfar Ruppin - Floating  - station 1 inverter 14 - Direct Current [A] - I_DC", 
                         "Kfar Ruppin - Floating  - station 1 inverter 14 - AC Voltage [V] - U_AC", 
                         "Kfar Ruppin - Floating  - station 1 inverter 14 - Voltage DC [V] - U_DC")

naharia <- process_df(Naharia, "Naharia", 
                      "Naharia - Inv 1 -  [] - E_INT_N", 
                      "Naharia - Inv 1 - Current AC [A] - I_AC", 
                      "Naharia - Inv 1 - Direct Current [A] - I_DC", 
                      "Naharia - Inv 1 - AC Voltage [V] - U_AC", 
                      "Naharia - Inv 1 - Voltage DC [V] - U_DC")
```

### Merge Electric Data to one DF

``` r
# Merge all data frames
Solar_df <- bind_rows(ashkelon, revivim, nir_david, beer_sheva, elifaz, emek_hefer, jerusalem, kfar_rupin, naharia)
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
Ashkelon_Weather <- rename_columns(Ashkelon_Weather, "Ashkelon")
Revivim_Weather <- rename_columns(Revivim_Weather, "Revivim")
Nir_David_Weather <- rename_columns(Nir_David_Weather, "Nir_David")
Beer_Sheva_Weather <- rename_columns(Beer_Sheva_Weather, "Beer_Sheva")
Elifaz_Weather <- rename_columns(Elifaz_Weather, "Elifaz")
Emek_Hefer_Weather <- rename_columns(Emek_Hefer_Weather, "Emek_Hefer")
Jerusalem_Weather <- rename_columns(Jerusalem_Weather, "Jerusalem")
Kfar_Rupin_Weather <- rename_columns(Kfar_Rupin_Weather, "Kfar_Rupin")
Naharia_Weather <- rename_columns(Naharia_Weather, "Naharia")

# Merge all Weather Data frames
Weather_df <- bind_rows(
  Ashkelon_Weather, 
  Revivim_Weather, 
  Nir_David_Weather, 
  Beer_Sheva_Weather, 
  Elifaz_Weather, 
  Emek_Hefer_Weather, 
  Jerusalem_Weather, 
  Kfar_Rupin_Weather, 
  Naharia_Weather
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
Nir_David_Weather2 <- convert_date_format(Nir_David_Weather2, datetime)
Beer_Sheva_Weather2 <- convert_date_format(Beer_Sheva_Weather2, datetime)
Elifaz_Weather2 <- convert_date_format(Elifaz_Weather2, datetime)
Emek_Hefer_Weather2 <- convert_date_format(Emek_Hefer_Weather2, datetime)
Kfar_Rupin_Weather2 <- convert_date_format(Kfar_Rupin_Weather2, datetime)
Jerusalem_Weather2 <- convert_date_format(Jerusalem_Weather2, datetime)
Naharia_Weather2 <- convert_date_format(Naharia_Weather2, datetime)

# Merge all Weather Data frames
Weather_df2 <- bind_rows(
  Ashkelon_Weather2, 
  Revivim_Weather2, 
  Nir_David_Weather2, 
  Beer_Sheva_Weather2, 
  Elifaz_Weather2, 
  Emek_Hefer_Weather2, 
  Jerusalem_Weather2, 
  Kfar_Rupin_Weather2, 
  Naharia_Weather2
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

## Feature Engineering

### Season Column

Create Season Column and add Dummy variables

``` r
# Create Season column by the number of the month - From Wikipedia
merged_df <- merged_df %>%
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
merged_df <- create_dummy_variables(merged_df, "Season")
```

### Altitude Column

Create Altitudes Column

``` r
# Merge altitude data with merged_df based on Location
merged_df <- merge(merged_df, Altitudes, by = "Location", all.x = TRUE)
```

### Area Column

Create Area Column and add Dummy variables

``` r
# Create Area column based on Location - From Google Maps
merged_df$Area <- ifelse(merged_df$Location %in% c("Naharia", "Kfar_Rupin", "Nir_David"), "North",
                         ifelse(merged_df$Location %in% c("Emek_Hefer", "Jerusalem"), "Center",
                                ifelse(merged_df$Location %in% c("Ashkelon", "Beer_Sheva", "Elifaz", "Revivim"), "South", NA)))

# Create a function to Make dummy variables

merged_df <- create_dummy_variables(merged_df, "Area")
```

### Temperature Range

Create Temperature Range Column

``` r
# Create a new variable for temperature ranges
merged_df$Temperature_Range <- cut(merged_df$Temperature_C, breaks = 5, labels = c("Very Low", "Low", "Medium", "High", "Very High"))
```

## Missing Values Handelling

Check for NA and NULL Values

``` r
# Check for NULL values
null_values <- sapply(merged_df, function(x) sum(is.null(x)))

# Check for NA values
na_values <- sapply(merged_df, function(x) sum(is.na(x)))

# Check for empty values
empty_values <- sapply(merged_df, function(x) sum(x == ""))

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
    ##                 Description                       Month 
    ##                           0                           0 
    ##                      Season                SeasonAutumn 
    ##                           0                           0 
    ##                SeasonSpring                SeasonSummer 
    ##                           0                           0 
    ##                SeasonWinter                    Altitude 
    ##                           0                           0 
    ##                        Area                  AreaCenter 
    ##                           0                           0 
    ##                   AreaNorth                   AreaSouth 
    ##                           0                           0 
    ##           Temperature_Range 
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
    ##                 Description                       Month 
    ##                           0                           0 
    ##                      Season                SeasonAutumn 
    ##                           0                           0 
    ##                SeasonSpring                SeasonSummer 
    ##                           0                           0 
    ##                SeasonWinter                    Altitude 
    ##                           0                           0 
    ##                        Area                  AreaCenter 
    ##                           0                           0 
    ##                   AreaNorth                   AreaSouth 
    ##                           0                           0 
    ##           Temperature_Range 
    ##                          50

``` r
# Empty values
print(empty_values)
```

    ##                    Location                   Timestamp 
    ##                           0                          NA 
    ##      Daily_energy_yield_kWh               Temperature_C 
    ##                          NA                          NA 
    ##           Max_Temperature_C           Min_Temperature_C 
    ##                          NA                          NA 
    ##   Relative_Humidity_Percent                     Rain_mm 
    ##                          NA                          NA 
    ##                Current_AC_A            Direct_Current_A 
    ##                          NA                          NA 
    ##                AC_Voltage_V                Voltage_DC_V 
    ##                          NA                          NA 
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
    ##                 Description                       Month 
    ##                           0                           0 
    ##                      Season                SeasonAutumn 
    ##                           0                           0 
    ##                SeasonSpring                SeasonSummer 
    ##                           0                           0 
    ##                SeasonWinter                    Altitude 
    ##                           0                           0 
    ##                        Area                  AreaCenter 
    ##                           0                           0 
    ##                   AreaNorth                   AreaSouth 
    ##                           0                           0 
    ##           Temperature_Range 
    ##                          NA

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

# Check for empty values
empty_values <- sapply(df, function(x) sum(x == ""))

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
    ##                 Description                       Month 
    ##                           0                           0 
    ##                      Season                SeasonAutumn 
    ##                           0                           0 
    ##                SeasonSpring                SeasonSummer 
    ##                           0                           0 
    ##                SeasonWinter                    Altitude 
    ##                           0                           0 
    ##                        Area                  AreaCenter 
    ##                           0                           0 
    ##                   AreaNorth                   AreaSouth 
    ##                           0                           0 
    ##           Temperature_Range 
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
    ##                 Description                       Month 
    ##                           0                           0 
    ##                      Season                SeasonAutumn 
    ##                           0                           0 
    ##                SeasonSpring                SeasonSummer 
    ##                           0                           0 
    ##                SeasonWinter                    Altitude 
    ##                           0                           0 
    ##                        Area                  AreaCenter 
    ##                           0                           0 
    ##                   AreaNorth                   AreaSouth 
    ##                           0                           0 
    ##           Temperature_Range 
    ##                          50

``` r
# Empty values
print(empty_values)
```

    ##                    Location                   Timestamp 
    ##                           0                          NA 
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
    ##                 Description                       Month 
    ##                           0                           0 
    ##                      Season                SeasonAutumn 
    ##                           0                           0 
    ##                SeasonSpring                SeasonSummer 
    ##                           0                           0 
    ##                SeasonWinter                    Altitude 
    ##                           0                           0 
    ##                        Area                  AreaCenter 
    ##                           0                           0 
    ##                   AreaNorth                   AreaSouth 
    ##                           0                           0 
    ##           Temperature_Range 
    ##                          NA

``` r
# Complete cases
print(complete_cases)
```

    ## [1] 3231

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

    ## `geom_smooth()` using formula = 'y ~ x'

![](Final_Project_files/figure-gfm/unnamed-chunk-20-1.png)<!-- -->

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

    ## `geom_smooth()` using formula = 'y ~ x'

    ## Warning: Removed 2 rows containing non-finite outside the scale range
    ## (`stat_smooth()`).

    ## Warning: Removed 2 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

    ## Warning: Removed 26 rows containing missing values or values outside the scale range
    ## (`geom_smooth()`).

![](Final_Project_files/figure-gfm/unnamed-chunk-21-1.png)<!-- -->

``` r
# Cloud Cover vs. Daily Energy Yield
suppressWarnings(suppressMessages(ggplot(df, aes(x = Cloud_Cover, y = Daily_energy_yield_kWh)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Effect of Cloud Cover on Daily Energy Yield",
       x = "Cloud Cover (%)",
       y = "Daily Energy Yield (kWh)")))
```

    ## `geom_smooth()` using formula = 'y ~ x'

![](Final_Project_files/figure-gfm/unnamed-chunk-22-1.png)<!-- -->

``` r
# Solar Radiation vs. Daily Energy Yield
suppressWarnings(suppressMessages(ggplot(df, aes(x = Solar_Radiation, y = Daily_energy_yield_kWh)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Solar Radiation vs. Daily Energy Yield",
       x = "Solar Radiation",
       y = "Daily Energy Yield (kWh)")))
```

    ## `geom_smooth()` using formula = 'y ~ x'

![](Final_Project_files/figure-gfm/unnamed-chunk-23-1.png)<!-- -->

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

![](Final_Project_files/figure-gfm/unnamed-chunk-24-1.png)<!-- -->

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

![](Final_Project_files/figure-gfm/unnamed-chunk-25-1.png)<!-- -->

``` r
library(corrplot)
```

    ## corrplot 0.92 loaded

``` r
# Calculate the correlation matrix
cor_matrix <- cor(df[, c("Daily_energy_yield_kWh", "Temperature_C", "Relative_Humidity_Percent", "Solar_Radiation", "UV_Index", "Rain_mm", "Wind_Speed", "Cloud_Cover")])

corrplot(cor_matrix, method = "color", type = "full", addrect = 4, 
         tl.col = "black")
```

![](Final_Project_files/figure-gfm/unnamed-chunk-26-1.png)<!-- -->

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
```

    ## 
    ## Attaching package: 'plm'

    ## The following objects are masked from 'package:dplyr':
    ## 
    ##     between, lag, lead

``` r
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

``` r
library(caret) # For RMSE calculation
```

    ## Warning: package 'caret' was built under R version 4.4.1

    ## Loading required package: lattice

    ## 
    ## Attaching package: 'caret'

    ## The following objects are masked from 'package:yardstick':
    ## 
    ##     precision, recall, sensitivity, specificity

    ## The following object is masked from 'package:purrr':
    ## 
    ##     lift

``` r
# Assuming you have a dataset df with predictors and response variable 'y'
# Fit the two models
model1 <- lm(Daily_energy_yield_kWh ~ Temperature_C + Relative_Humidity_Percent + Rain_mm + Daylight_Hours + Solar_Radiation + UV_Index + Wind_Speed + Wind_Gust + Cloud_Cover, data = df)
model2 <- lm(Daily_energy_yield_kWh ~ Location + Area + Altitude, data = df)

# Make predictions using both models
pred1 <- predict(model1, df)
pred2 <- predict(model2, df)

# Combine predictions using weights (w1 and w2)
w1 <- 0.7
w2 <- 0.3
combined_pred <- w1 * pred1 + w2 * pred2

# Calculate R²
ss_total <- sum((df$Daily_energy_yield_kWh - mean(df$Daily_energy_yield_kWh))^2)
ss_res <- sum((df$Daily_energy_yield_kWh - combined_pred)^2)
r_squared <- 1 - (ss_res / ss_total)

# Calculate adjusted R²
n <- nrow(df)
p <- ncol(df) - 1
adj_r_squared <- 1 - ((1 - r_squared) * (n - 1) / (n - p - 1))

# Calculate RMSE
rmse <- RMSE(combined_pred, df$Daily_energy_yield_kWh)

# Print the results
cat("R²:", r_squared, "\n")
```

    ## R²: 0.6239939

``` r
cat("Adjusted R²:", adj_r_squared, "\n")
```

    ## Adjusted R²: 0.6200554

``` r
cat("RMSE:", rmse, "\n")
```

    ## RMSE: 0.9522931

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

# Plot distribution of coefficients
ggplot(boot_coefs, aes(x = estimate)) +
  geom_histogram(bins = 30) +
  facet_wrap(~ term, scales = "free", ncol = 3) +
  labs(title = "Bootstrap distributions of regression coefficients")
```

![](Final_Project_files/figure-gfm/unnamed-chunk-32-1.png)<!-- -->

``` r
# Plot distribution of metrics
ggplot(boot_metrics, aes(x = estimate)) +
  geom_histogram(bins = 30) +
  facet_wrap(~ term, scales = "free") +
  labs(title = "Bootstrap distributions of model metrics")
```

![](Final_Project_files/figure-gfm/unnamed-chunk-32-2.png)<!-- -->

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
