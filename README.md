# SISE2601 Project Team 2 - Solar Panels Project

### Project Overview

This repository contains the analysis and findings for our team project in SISE2601 - Data Analysis with R. We investigate the question:

**"How can the optimization of solar panel placement influence electricity productivity in different geographic locations and climatic conditions?"**

### Repository Structure

- `code/`: R script for data processing and analysis
- `code/README_files/figure-gfm/`: Output graphs and charts

### Getting Started

To compile and run the project, follow these steps:

1. **Clone the repository:**
    ```
    git clone https://github.com/Yarin-Shohat/Solar-Panels-Analysis-Project.git
    ```
2. **Navigate to the project directory:**
    ```
    cd Solar-Panels-Analysis-Project
    ```
3. **Add Data:**  
    Download the Data and Add the `data/` folder into to the folder `Solar-Panels-Analysis-Project/`  
   
   so that all the data will be in  `Solar-Panels-Analysis-Project/data/`
5. **Run R Script:**  
    Run the R Markdown File in `Solar-Panels-Analysis-Project/code/Final-Project.Rmd`

### Key Features

- Analysis of solar panel efficiency across various locations
- Impact of climatic conditions on electricity production
- Optimization strategies for solar panel placement
- Data visualization of key findings
- R code for data processing and analysis

### Tools Used

- R for data analysis and visualization
- RStudio as the development environment
- Linear Regression with Fixed Model effect
- Bootstrap

### Data Overview

We have data from 9 different locations in Israel.

The data we collected is from January 1, 2023, until December 31, 2023:

1. <ins>**Weather Data from the Solar Panels Company**</ins> (Private Data)
2. <ins>**Data from Israel Meteorological Service:**</ins>  
    We collected data about the Weather From [link](https://ims.gov.il/he/data_gov)
3. <ins>**Weather Data from Visual Crossing:**</ins>   
    We collected more data about the Weather from [link](https://www.visualcrossing.com/weather/weather-data-services)
4. <ins>**Altitudes Data:**</ins>  
    We collected data about the Altitudes of the Locations From [link](https://he.wikipedia.org/)
5. <ins>**Area location divide:**</ins>   
    We collected data about the General Area in Israel of the Locations From [link](https://www.google.com/maps)
6. <ins>**Seasons Data:**</ins>   
    We collected data about the seasons in Israel from [link](https://he.wikipedia.org/wiki/%D7%A2%D7%95%D7%A0%D7%95%D7%AA_%D7%94%D7%A9%D7%A0%D7%94)

Description about the Features:

1. Location: The geographic location where data was collected (e.g., "Ashkelon")
2. Timestamp: The date on which the data was recorded
3. Daily_energy_yield_kWh: The daily energy yield in kilowatt-hours recorded on the given date (Sum from that day)
4. Temperature_C: The average temperature in degrees Celsius recorded on the given date (Average from that day)
5. Max_Temperature_C: The maximum temperature in degrees Celsius recorded on the given date (Max from that day)
6. Min_Temperature_C: The minimum temperature in degrees Celsius recorded on the given date (Min from that day)
7. Relative_Humidity_Percent: The relative humidity percentage measured on the given date (Average from that day)
8. Rain_mm: The amount of rainfall in millimeters recorded on the given date (Average from that day)
9. Current_AC_A: The alternating current (AC) measured in amperes on the given date (Average from that day)
10. Direct_Current_A: The direct current (DC) measured in amperes on the given date (Average from that day)
11. AC_Voltage_V: The AC voltage measured in volts on the given date (Average from that day)
12. Voltage_DC_V: The DC voltage measured in volts on the given date (Average from that day)
13. Daylight_Hours: The number of daylight hours on the given date
14. Temperature_C_Feel_Like: The average "feels like" temperature in degrees Celsius on the given date
15. Max_Temperature_C_Fell_Like: The maximum "feels like" temperature in degrees Celsius on the given date
16. Min_Temperature_C_Feel_Like: The minimum "feels like" temperature in degrees Celsius on the given date
17. Solar_Radiation: The amount of solar radiation received on the given date
18. UV_Index: The ultraviolet (UV) index on the given date
19. Wind_Speed: The average wind speed on the given date
20. Wind_Gust: The maximum wind gust speed on the given date
21. Visibility: The average visibility distance on the given date
22. Cloud_Cover: The percentage of sky covered by clouds on the given date
23. Description: A brief description of the weather conditions on the given date
24. Month: The month number in which the data was recorded (numeric)
25. Season: The season during which the data was recorded (Winter, Spring, Summer, Autumn)
26. Season_Autumn: Binary indicator for Autumn (0 = No, 1 = Yes)
27. Season_Spring: Binary indicator for Spring (0 = No, 1 = Yes)
28. Season_Summer: Binary indicator for Summer (0 = No, 1 = Yes)
29. Season_Winter: Binary indicator for Winter (0 = No, 1 = Yes)
30. Altitude: The altitude at which the data was collected, in meters
31. Area: The general area where the data was collected (e.g., "South")
32. Area_Center: Binary indicator for Center area (0 = No, 1 = Yes)
33. Area_North: Binary indicator for North area (0 = No, 1 = Yes)
34. Area_South: Binary indicator for South area (0 = No, 1 = Yes)
35. Temperature_Range: The difference between the maximum and minimum temperature for the given date
