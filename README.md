# EPA-Airnow-API-report
This report requests one month of data from the EPA Airnow system's API. 
EPA's AAirnow system - read more here: https://www.airnow.gov/, collects both hourly observation data from states' air quality monitors as well as forecast data provided by states. 

This report will pull all data (forecasts and observations for all monitors in Delaware) for a user-defined month. It will then find the appropriate forecast for each day (using only the day-ahead forecast where available, or the weekend forecast). It also finds the maximum daily AQI value from any monitor in the state. 
Finally, the report provides some summary statistics for the difference between the forecast AQI and the actual max AQI in the state, plots the boxplot of the data, and has a few summary sentences about the data. 

To adapt this report to another geographic location, the bbox, or lat/lon bounding box, would need to be changed. Also, you will have to register and acquire your own API key. 

