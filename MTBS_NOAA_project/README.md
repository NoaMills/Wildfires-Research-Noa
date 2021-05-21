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

Scripts 2a, 2b and 2c are to be run iteratively to identify stations with flagged or missing data, identify next nearest stations, and extract updated data. For each row in the dataframe, the iterations continue until either weather data is downloaded with minimal missing or flagged data, OR until the next nearest station is 250 km or more away from the fire, at which point the data is entirely irrelevant. Due to the high computational cost of these operations, scripts 2a and 2c are run on the UCSC Hummingbird computing cluster. Since new weather data files cannot be downloaded directly on the cluster, script 2b downloads them locally, and the user must upload the files to the cluster.

wildfires_code_2a
  To be run on remote computer. The first time this script is run, the data file firedata1.csv is loaded and then written as firedata2a_1.csv.
  After 2b and 2c have been run at least once, script 2a reads the datafile firedata2c_i.csv and identifies the next nearest stations for fires that have yet to be updated. Saves firedata to firedata2a_i.csv where i is the iteration.

wildfires_code_2b
  To be run on local computer
      Downloads all weather files that have not yet been downloaded. User must then upload data to cluster as specified below. Saves firedata to firedata2b_i.csv where i is the iteration.

wildfires_code_2c
  Extracts weather data for each fire that requires weather data updates. After extraction, identifies which fires have associated weather data that still has too much missing or flagged data. Saves firedata to firedata2c_i.csv where i is the iteration. Prints out the number of rows that still need updating. If any are nonzero, the user knows to run another iteration.

These 3 scripts are to be run iteratively as follows. With firedata up until 2018, a total of 9 iterations were required.

-Run 2a remotely
-Transfer firedata2a_i.csv to local
-Run 2b locally
-Transfer firedata2b_i.csv to remote
-Transfer all noaadata to remote
  Recommended approach to only transfer new data files:
  rsync -av --ignore-existing ./*.csv user@hb.ucsc.edu:~/Data/noaa/noaadata
-Run 2c remotely

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
    Perform robust multiple linear regression to model acreage burned based on weather variables, year, month, and region
