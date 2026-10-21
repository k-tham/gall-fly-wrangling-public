https://kinglee-gall-wrangling.share.connect.posit.cloud/

Thanks for using my Shiny app! Here's a guide of the directory. I recommend you view the README.md file rather than the preview in GitHub, which garbles the formatting.

folder/
├── raw_data/
│   ├── gall_data_for_R.xlsx
│   ├── 1995_monthlyweatherdata
│   ├── 1996_monthlyweatherdata
│   ├── ...
│   └── 2026_monthlyweatherdata
├── shiny/
│   ├── data/
│   │   ├── gall_and_weather_data
│   │   └── rsconnect/
│   └── app.R
├── scraping.R
└── wrangling.R

scraping.R pulls once year of weather data from the NOAA API for station USC00190120. It cleans the data to show one row per month and saves the resulting file "2026_monthlyweatherdata.RData" in the raw_data folder.

wrangling.R combines all of the yearly weather files into one dataset and joins it to the field data (all_data_for_R.xlsx) by the year and semester of sampling. The resulting file "gall_and_weather_data.RData" is saved in the data folder within Shiny.

app.R contains the code for the Shiny app.

raw_data holds all of the yearly weather files made by scraping.R and the field data spreadsheet.

data holds the final merged "gall_and_weather_data.RData" that the app loads.

rsconnect contains the deployment settings used for publishing the app to the web.

