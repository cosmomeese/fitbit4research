# fitbit4research
R script to connect to Fitbit API, manage access tokens and download intraday data

inspired by work by S. Bromberg (https://github.com/simonbromberg/googlefitbit) who made the google sheet version off which this is based 
the R part mostly builds off work by (https://github.com/teramonagi/fitbitr).
N.B. the the script presently uses a forked version of teramonagi's work (https://github.com/cosmomeese/fitbitr) since it requires use of an update to the httr library which has not yet been accepted into the official version (also forked https://github.com/cosmomeese/httr). This complicated mess is to allow us to specify query parameters to the authentication page to help with token management.

N.B. file paths are not cleaned in this version, so update these as required on your version

To use:

1) update FitbitAPIKeys
update the 'FitbitAPIKeys - Example.R' file with your client, secret keys and callback URL (see Fitbit API keys preperation from https://github.com/teramonagi/fitbitr/blob/master/README.md)
rename 'FitbitAPIKeys - Example.R' to 'FitbitAPIKeys.R'

2) add accounts (Study ID, download start date, email addresses) to 'fitbitLogins-Clean-OnePerEmail.csv'
N.B. passwords are recommended but optional since these must be entered manually anyways
N.B. similarly, end date is optional. If not provided the script will just continue to download the most recent data whenever it is run.

3)
open 'fitbitPullScriptForStudy.R' and change the CSV_FILEPATH to match the file you created in 2)
also specify in RDATA_SAVE_FILEPATH where you want the script data file to be stored (this will contain both the fitbit data frame as fitbitData.df, and the meta data (from CSV_FILEPATH) and the most recent token information as metaDataAndTokens.list.

4)
run 'fitbitPullScriptForStudy.R' and follow the prompts in the console (see the instructions above the pullFitbitData() function for examples)

N.B. the Fitbit API is rate limited to 150 requests per hour. The current script will (greedily) attempt to download as much data as possible and will hit the rate limit if you try to download too many days at once. If this happens the script will report and throw an error, refresh the token and move on to the next person. This is fine. Simply wait until the next hour and try again to continue downloading more days of data.
