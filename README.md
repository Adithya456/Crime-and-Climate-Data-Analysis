# Crime and Climate Data Analysis

## Project Description
This project explores crime patterns in Colchester, UK, for 2023 alongside climate data to examine possible links between crime rates and climate conditions. Analyzing crime categories, locations, and seasonal trends provides insights into public safety and examines potential environmental impacts on crime.

## Overview
This study focuses on understanding how climate factors may influence crime rates. By combining crime data with climate data, we investigate patterns such as seasonal spikes, high-crime areas, and any apparent relationship between weather conditions (e.g., temperature, humidity) and crime categories. The objectives of this project are to:

1. Examine correlations between climate metrics and crime types.
2. Identify crime hotspots across Colchester.
3. Observe seasonal variations in crime frequency and type.
4. Provide insights into how climate and environmental factors may impact crime.

## Project Structure
- **Data Preprocessing**: Cleaned crime and climate datasets, handled missing values, and transformed variables for consistency.
- **Exploratory Data Analysis (EDA)**: Visualizations include correlation heatmaps, bar plots, and time series to assess patterns in both datasets.
- **Crime & Climate Relationship Analysis**: Statistical and visual analysis of climate factors, such as temperature, humidity, and precipitation, alongside crime frequencies.
- **Geographic Crime Visualization**: Mapped crime locations to highlight areas with higher crime rates.

## Project Files
- **data/crime_data.csv**: Contains crime details, including categories, locations, and dates.
- **data/climate_data.csv**: Daily climate data for Colchester, UK, in 2023.
- **scripts/Crime_Climate_Data_Analysis**: R scripts for data preprocessing, EDA, and visualizations.
- **report/Crime_Climate_Data_Analysis_Report.pdf**: Detailed project report with methodology, results, and conclusions.
- **README.md**: Project documentation (this file).

## Data Processing Steps
- **Crime Data**: Removed irrelevant columns, replaced missing `outcome_status` values with "Information Not Available," and aggregated data by location and type.
- **Climate Data**: Cleaned data, grouped climate variables by date, and standardized variables for consistent analysis.

## Exploratory Data Analysis
1. **Crime Patterns**: Analyzed crime frequency by month, location, and type, revealing high rates of violent crime and anti-social behavior, peaking in January and September.
2. **Climate Trends**: Used time series and density plots to understand temperature, precipitation, and humidity variations throughout the year.
3. **Crime & Climate Correlation**: Investigated relationships between climate conditions and crime rates using correlation matrices and time series comparisons.

## Key Findings
- **Crime Trends**: Violent and anti-social crimes were the most frequent, with noticeable seasonal fluctuations.
- **Climate Impact**: Preliminary findings suggest correlations between extreme temperatures and higher crime rates, particularly in specific crime categories.

## Dependencies
The following R packages are required:

- `dplyr` - Data manipulation
- `plotly` - Interactive visualizations
- `ggplot2` - Data visualization
- `lubridate` - Date handling
- `xts` - Time series handling
- `forecast` - Forecasting models
- `kableExtra` - Enhanced table output
- `leaflet` - Map visualization
- `ggcorrplot` - Correlation plotting
- `reshape2` - Data reshaping

Install dependencies with:
```bash
pip install -r requirements.txt
```

## Results and Conclusion

This analysis shows that violent and anti-social crimes were predominant, with seasonal peaks and possible links to climate variables like extreme temperatures. The geographic mapping of crime occurrences identified Colchester areas with higher crime rates, supporting targeted safety interventions.

## Contributing
Contributions are welcome! If you find any issues or want to improve the code, feel free to open a pull request or create an issue in the repository.

## License
This project is licensed under the [MIT License](./LICENSE). See the LICENSE file for more details.


For more details, please refer to the [project report](./Crime_Climate_Data_Analysis_Report.pdf).
