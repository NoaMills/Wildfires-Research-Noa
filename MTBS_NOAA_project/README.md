Wildfires-Research-Noa
Code written by Noa Mills under supervision on Dr. Zhu Kai

The five code files wildfires_code_i {i=1,2,3,4,5} perform the following.
The first three code files produce the data files firedatai.csv {i=1,2,3}

wildfires_code_1
  Section 1:
      Downloads MTBS fire data, NOAA stations and inventory data to respective directories, constructs associated data frames
      Stations dataframe contains data on where NOAA weather stations are
      Inventory dataframe contains data on what weather variables each station observes,
        and the timeframes of observation for each variable
      Constructs stationsUS dataframe which only contains stations in the US that were not decomissioned before 1984
        and that observe the variables temperature max (TMAX), temperature min (TMIN) and precipitation (PRCP)
      Incorporates variable observation start and stop date data from inventory dataframe to stationsUS dataframe
  Section 2:
      Identifies the closest weather station (station ID, station long/lat, distance to fire) to each fire
      Since not all stations record all variables at a time, a station is found for each fire for each variable (TMAX, TMIN, PRCP)
        based on the variable observation start and stop date data
      Saves firedata1.csv which contains original mtbs firedata and closest fire stations for TMAX, TMIN, and PRCP variables
      Saves stationsUS.csv dataframe which contains location, ID, and start/stop years of variable observations for stations in US only


wildfires_code_2a
  To be run on local computer
      Downloads NOAA weather data files for each station identified in section 2 of wildfires_code_1
      In previous runs, some of the data files have been corrupted. Created code to identify and redownload corrupted files.
        This should be fixed, but I kept the code to replace corrupted files
wildfires_code_2b
  To be run on remote computer
      Extracts weather data from NOAA data files downloaded in section 2a and incorporates weather data in firedata dataframe
      Includes weather data for each variable from 21 days before the event until 35 days after
        TMAX.3 represents TMAX value 3 days before the fire, TMAX3 3 days after
wildfires_code_2c
  In progress
      Identifies fires with too much missing or flagged weather data
      For flagged fires, identifies next nearest weather stations and pulls weather data again

wildfires_code_3
  Section 1:
    Identify the state of each fire
    Identify the geographic region of each fire based on the state
      Geographic regions based National Weather Service
  Section 2:
    Subset data based on assessment type and geographic region to prepare for regression analysis

wildfires_code_4
    Data visualizations, including histograms, dot plots, and maps exploring the relationship between weather and fire size, and the differences between fire types
    Informative plots saved and pushed to github repositories. Uninformative plot code left, but plots not included in repo.

wildfires_code_5
    Perform robust multiple linear regression
    Independent variables are: year, TMAXavg, TMINavg, PRCPavg. Dependent variable is acres burned.
