# Preprocessing:

## Load Library For Preprocessing

```{r}
library(tidyverse)
library(readxl)
```


## Read Altitudes Data

```{r}
# Read the Data for Altitudes for the Locations
Altitudes <- read_excel("../data/Altitudes.xlsx")
```

## Read Electric Data

```{r}
# Read the Data of Solar Panels
Revivim <- read_excel("../data/Revivim.xlsx")
Nir_David <- read_excel("../data/Nir David.xlsx")
Ashkelon <- read_excel("../data/Ashkelon.xlsx")
Beer_Sheva <- read_excel("../data/Beer Sheva.xlsx")
Elifaz <- read_excel("../data/Elifaz.xlsx")
Emek_Hefer <- read_excel("../data/Emek Hefer.xlsx")
Jerusalem <- read_excel("../data/Jerusalem.xlsx")
Kfar_Rupin <- read_excel("../data/Kfar Rupin.xlsx")
Naharia <- read_excel("../data/Naharia.xlsx")
```

## Read Weather Data from Meteorological Service

```{r}
# Read Weather Data from Meteorological Service
Ashkelon_Weather <- read_excel("../data/Ashkelon_Weather.xlsx")
Beer_Sheva_Weather <- read_excel("../data/Beer_Sheva_Weather.xlsx")
Elifaz_Weather <- read_excel("../data/Elifaz_Weather.xlsx")
Emek_Hefer_Weather <- read_excel("../data/Emek_Hefer_Weather.xlsx")
Jerusalem_Weather <- read_excel("../data/Jerusalem_Weather.xlsx")
Kfar_Rupin_Weather <- read_excel("../data/Kfar_Rupin_Weather.xlsx")
Naharia_Weather <- read_excel("../data/Naharia_Weather.xlsx")
Nir_David_Weather <- read_excel("../data/Nir_David_Weather.xlsx")
Revivim_Weather <- read_excel("../data/Revivim_Weather.xlsx")
```

## Read Weather Data from Visual Crossing

```{r}
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

## Preprocess Electric Data to one DF

```{r}
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

```


```{r}
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

## Merge Electric Data to one DF

```{r}
# Merge all data frames
Solar_df <- bind_rows(ashkelon, revivim, nir_david, beer_sheva, elifaz, emek_hefer, jerusalem, kfar_rupin, naharia)
```


## Merge Weather Data from Meteorological Service to one DF

```{r}
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

## Preprocess Weather Data from Meteorological Service to one DF

```{r}
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

## Join Weather Data from Meteorological Service With Electric Data

```{r}
# Join the data frames
merged_df <- inner_join(Weather_df, Solar_df, by = c("Location", "Timestamp"))
```

## Merge and Preprocess Weather Data from Visual Crossing to one DF

```{r}
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

## Join Weather Data from Meteorological Service and Electric Data with Visual Crossing

```{r}
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

```{r}
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
  dummies <- model.matrix(~ . - 1, data = df[column_name, drop = FALSE])
  
  # Convert the matrix to a data frame and rename columns appropriately
  dummy_df <- as.data.frame(dummies)
  colnames(dummy_df) <- gsub(column_name, paste0(column_name, "_"), colnames(dummy_df))
  
  # Combine the original dataframe with the dummy variables
  df <- cbind(df, dummy_df)
  
  return(df)
}

# Example usage for the "Season" column
merged_df <- create_dummy_variables(merged_df, "Season")

# Merge altitude data with merged_df based on Location
merged_df <- merge(merged_df, Altitudes, by = "Location", all.x = TRUE)

# Create Area column based on Location - From Google Maps
merged_df$Area <- ifelse(merged_df$Location %in% c("Naharia", "Kfar_Rupin", "Nir_David"), "North",
                         ifelse(merged_df$Location %in% c("Emek_Hefer", "Jerusalem"), "Center",
                                ifelse(merged_df$Location %in% c("Ashkelon", "Beer_Sheva", "Elifaz", "Revivim"), "South", NA)))

# Create a function to Make dummy variables

merged_df <- create_dummy_variables(merged_df, "Area")
```

## Check for NA and NULL Values

```{r}
# Check for NULL values
null_values <- sapply(merged_df, function(x) sum(is.null(x)))

# Check for NA values
na_values <- sapply(merged_df, function(x) sum(is.na(x)))

# Check for empty values
empty_values <- sapply(merged_df, function(x) sum(x == ""))

# Check for complete cases
complete_cases <- sum(complete.cases(merged_df))

# Print the results
print("Null values:")
print(null_values)
print("NA values:")
print(na_values)
print("Empty values:")
print(empty_values)
print("Complete cases:")
print(complete_cases)
```

## Handelling Missing Values Step:

```{r}
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

## Check for NA and NULL Values

```{r}
# Check for NULL values
null_values <- sapply(df, function(x) sum(is.null(x)))

# Check for NA values
na_values <- sapply(df, function(x) sum(is.na(x)))

# Check for empty values
empty_values <- sapply(df, function(x) sum(x == ""))

# Check for complete cases
complete_cases <- sum(complete.cases(df))

# Print the results
print("Null values:")
print(null_values)
print("NA values:")
print(na_values)
print("Empty values:")
print(empty_values)
print("Complete cases:")
print(complete_cases)
```

# Data Analysis

## Visualization

```{r}
# Temperature vs. Daily Energy Yield
ggplot(df, aes(x = Temperature_C, y = Daily_energy_yield_kWh)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Correlation between Daily Energy Yield and Temperature",
       x = "Temperature (°C)",
       y = "Daily Energy Yield (kWh)")

```

```{r}
# Rain vs. Daily Energy Yield
gg_rain <- ggplot(train_data, aes(x = Rain_mm, y = Daily_energy_yield_kWh)) +
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
gg_rain
```

```{r}
# Cloud Cover vs. Daily Energy Yield
ggplot(df, aes(x = Cloud_Cover, y = Daily_energy_yield_kWh)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Effect of Cloud Cover on Daily Energy Yield",
       x = "Cloud Cover (%)",
       y = "Daily Energy Yield (kWh)")
```

```{r}
# Solar Radiation vs. Daily Energy Yield
ggplot(df, aes(x = Solar_Radiation, y = Daily_energy_yield_kWh)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Solar Radiation vs. Daily Energy Yield",
       x = "Solar Radiation",
       y = "Daily Energy Yield (kWh)")
```

```{r}
# Create a new variable for temperature ranges
df$Temperature_Range <- cut(df$Temperature_C, breaks = 5, labels = c("Very Low", "Low", "Medium", "High", "Very High"))

# Histogram for Daily Energy Yield with respect to Temperature Ranges
gg <- ggplot(df, aes(x = Temperature_Range, y = Daily_energy_yield_kWh)) +
  geom_bar(stat = "identity", fill = "blue", color = df$Temperature_Range, alpha = 0.7) +
  labs(
    x = "Temperature Range",
    y = "Daily Energy Yield (kWh)",
    title = "Histogram of Daily Energy Yield",
    subtitle = "across Temperature Ranges"
  ) +
  theme_minimal()
gg
```

```{r}
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

```{r}
library(corrplot)
# Calculate the correlation matrix
cor_matrix <- cor(df[, c("Daily_energy_yield_kWh", "Temperature_C", "Relative_Humidity_Percent", "Solar_Radiation", "UV_Index", "Rain_mm", "Wind_Speed", "Cloud_Cover")])

corrplot(cor_matrix, method = "color", type = "full", addrect = 4, 
         tl.col = "black")
```

## Linear Regression

```{r}
# Load necessary libraries
library(tidymodels)
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

### Regular Linear Regression

```{r}
# Fit the linear regression
df_fit <- linear_reg() %>% 
  set_engine("lm") %>% 
  fit(Daily_energy_yield_kWh ~ Temperature_C + Relative_Humidity_Percent + Rain_mm + Solar_Radiation + UV_Index + Wind_Speed + Wind_Gust + Cloud_Cover, data = train_data)

# Extract the fitted model object
lm_fit <- df_fit$fit

# Get the tidy coefficients
tidy_coefs <- tidy(lm_fit)
print(tidy_coefs)

# Get the model summary statistics
model_summary <- glance(lm_fit)

# Print the model summary statistics
print(model_summary)

# Extract RMSE from residuals
rmse <- sqrt(mean(lm_fit$residuals^2))

# Print RMSE
cat("RMSE: ", rmse, "\n")
```

### Fixed Model Linear Regression

```{r}
# Load necessary libraries
library(plm)

# Convert the data to a tibble
df_tibble2 <- as_tibble(df)

# Estimate fixed effects model using plm
fixed_model <- plm(Daily_energy_yield_kWh ~ Temperature_C + Relative_Humidity_Percent + Rain_mm + Solar_Radiation + UV_Index + Wind_Speed + Wind_Gust + Cloud_Cover + factor(Location),
                   data = df_tibble2,
                   model = "within")

# Summary of the model
summary(fixed_model)

# Extract residuals
residuals <- residuals(fixed_model)

# Calculate RMSE
rmse <- sqrt(mean(residuals^2))

# Print RMSE
cat("RMSE: ", rmse)
```

## Bootstrap

```{r}
# Function to fit model and extract metrics
fit_model <- function(split) {
  model <- lm(Daily_energy_yield_kWh ~ Temperature_C + Relative_Humidity_Percent + Rain_mm + Daylight_Hours + Solar_Radiation + UV_Index + Wind_Speed + Wind_Gust + Cloud_Cover, data = analysis(split))
  data.frame(
    term = c("R2", "adj_R2", "RMSE"),
    estimate = c(summary(model)$r.squared,
                 summary(model)$adj.r.squared,
                 sqrt(mean(residuals(model)^2)))
  )
}

# Perform bootstrapping
set.seed(123)  # for reproducibility
boot_samples <- bootstraps(df, times = 15000)

# Fit model to each bootstrap sample and collect results
boot_metrics <- boot_samples %>%
  mutate(metrics = map(splits, fit_model)) %>%
  unnest(metrics)

# Plot distribution of metrics
ggplot(boot_metrics, aes(x = estimate)) +
  geom_histogram() +
  facet_wrap(~ term, scales = "free") +
  labs(title = "Bootstrap distributions of model metrics")

# Calculate confidence intervals
ci_metrics <- boot_metrics %>%
  group_by(term) %>%
  summarize(
    mean_estimate = mean(estimate),
    lower_ci = quantile(estimate, 0.025),
    upper_ci = quantile(estimate, 0.975)
  )

print(ci_metrics)
```
