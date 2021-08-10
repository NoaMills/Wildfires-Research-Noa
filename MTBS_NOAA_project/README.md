Wildfires-Research-Noa
Code written by Noa Mills under supervision on Dr. Zhu Kai

The code files wildfires_code_i {i=1,2,3,4,5...} perform the following.
Most files produce the data files firedatai.csv {i=1,2,3...} though some produce plots or perform analyses instead of adding to the dataset.

wildfires_code_1
  Downloads MTBS fire data, NOAA stations and inventory data to respective directories, constructs associated data frames
  Stations dataframe contains data on where NOAA weather stations are
  Inventory dataframe contains data on what weather variables each station observes,
    and the timeframes of observation for each variable
  Constructs stationsUS dataframe which only contains stations in the US that were not decomissioned before 1984
    and that observe the variables temperature max (TMAX), temperature min (TMIN) and precipitation (PRCP)
  Incorporates variable observation start and stop date data from inventory dataframe to stationsUS dataframe
  Identifies the closest weather station (station ID, station long/lat, distance to fire) to each fire
  Since not all stations record all variables at a time, a station is found for each fire for each variable (TMAX, TMIN, PRCP)
    based on the variable observation start and stop date data
  Saves firedata1.csv which contains original mtbs firedata and closest fire stations for TMAX, TMIN, and PRCP variables
  Saves stationsUS.csv dataframe which contains location, ID, and start/stop years of variable observations for stations in US only

wildfires_code_2a
  SUPERSEDED BY SCRIPTS 2A, 2B AND 2C BUT LEFT FOR REFERENCE. This version extracts weather data from 3 days before ignition until 3 days after ignition. Scripts 2a, 2b and 2c extract data from 21 days before ignition to 35 days after. This provides a more thorough analysis, but is much more computationally expensive, and are intended to be run on a computing cluster. If you do not have access to a computing cluster, then you can run script 2 locally, though it will take a while and won't provide as much data. Note that the following code scripts assume you have run scripts 2a, 2b and 2c.

********
Scripts 2a, 2b and 2c are to be run iteratively to identify stations with flagged or missing data, identify next nearest stations, and extract updated data. For each row in the dataframe, the iterations continue until either weather data is downloaded with minimal missing or flagged data, OR until the next nearest station is 250 km or more away from the fire, at which point the data is deemed irrelevant. Due to the high computational cost of these operations, scripts 2a and 2c are run on the UCSC Hummingbird computing cluster. Since new weather data files cannot be downloaded directly on the cluster, script 2b downloads them locally, and the user must upload the files to the cluster.
********

wildfires_code_2a
  To be run on computing cluster. The first time this script is run, the data file firedata1.csv is loaded and then written as firedata2a_1.csv.
  After 2b and 2c have been run at least once, script 2a reads the datafile firedata2c_i.csv and identifies the next nearest stations for fires that have yet to be updated. Saves firedata to firedata2a_i.csv where i is the iteration.

wildfires_code_2b
  To be run on local computer. Downloads all weather files that have not yet been downloaded. User must then upload data to cluster as specified below. Saves firedata to firedata2b_i.csv where i is the iteration.

wildfires_code_2c
  To be run on computing cluster. Extracts weather data for each fire that requires weather data updates. After extraction, identifies which fires have associated weather data that still has too much missing or flagged data. Saves firedata to firedata2c_i.csv where i is the iteration. Prints out the number of rows that still need updating. If any are nonzero, the user knows to run another iteration.

These 3 scripts are to be run iteratively as follows. With firedata up until 2018, a total of 9 iterations were required. A future update for either me or a future student would involve writing a shell script to perform these iterations so the user does not need to do so manually.

-Run 2a remotely
-Transfer firedata2a_i.csv to local
-Run 2b locally
-Transfer firedata2b_i.csv to remote
-Transfer all noaadata to remote
  Recommended approach to only transfer new data files:
  rsync -av --ignore-existing ./*.csv user@hb.ucsc.edu:~/Data/noaa/noaadata
-Run 2c remotely

wildfires_code_3
  Identify the state of each fire
  Identify the geographic region of each fire based on the state
    Geographic regions based National Weather Service
  Creates dummy (boolean) variables to encode geographic region membership, fire type, and month in preparation for regression analysis
  Creates columns storing the average ELM data across each week (3 weeks before, 2 weeks before, ... , 5 weeks after) and average ELM data across each month


wildfires_code_4
    Data visualizations, including histograms, dot plots, and maps exploring the relationship between weather and fire size, and the differences between fire types
    Informative plots saved and pushed to github repositories. Uninformative plot code left, but plots not included in repo.

wildfires_code_5
    Perform robust multiple linear regression to model acreage burned based on weather variables, year, month, and region

wildfires_code_6
    Downloads and incorporates Palmer Drought Severity Index (PSDI) from NOAA into the firedata dataframe. This data is built on precipitation and temperature data, but
    is interpolated across space as opposed to the station-based weather data.

wildfires_code_7
    Incorporates data on Time Since Last Burn (TSLB). Creates two additional data attributes, TSLB10 and TSLB25 which specify the number of days
    since there was a fire within 10 km, and within 25 km respectively. Fires with no recorded previous fire within 10 or 25 km are given the value NA.
    Categorizes TSLB data into 8 bins, and creates boxplots of log acreage burned by bin.
    Code intended to be run remotely on a computing cluster.

wildfires_code_8
    Performs Multiple Linear Regression with various combinations of predictors (station based data, PSDI data, TSLB10 and TSLB25 continuous and binned)
    to compare which has the greatest adjusted R-squared value.
