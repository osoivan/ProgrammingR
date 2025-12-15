############################################################
# BEFORE YOU START — CREATE YOUR ACCOUNT
#
# 1. Go to https://www.shinyapps.io/
# 2. Click "Sign Up" and create a free account.
# 3. After logging in, go to:
#      Account → Tokens → "Show Token"
#    You will need:
#       - your username
#       - your token
#       - your secret
#
# Only after creating your account you can continue.
############################################################


############################################################
# YOUR SHINY APP FOLDER MUST LOOK LIKE THIS:
#
#   C:/EcuadorInamhi/
#       ├── app.R               # Main Shiny file
#       ├── data.csv            # (Optional) any data files used
#       ├── stations.geojson    # (Optional) spatial files
#       ├── image.png           # (Optional) assets for the UI
#       └── other_files...
#
# IMPORTANT:
#   - All files used by the app must be INSIDE this folder.
#   - Do NOT use absolute paths like "C:/something/data.csv".
#   - Always use relative paths: "data.csv", "stations.geojson".
############################################################


############################################################
# STEP 1 — Install rsconnect (only once)
############################################################
# install.packages("rsconnect")


############################################################
# STEP 2 — Load the library
############################################################
library(rsconnect)


############################################################
# STEP 3 — Set your shinyapps.io account information
#
# IMPORTANT:
#   You MUST create your account at https://www.shinyapps.io/
#   before running this step.
#
# Once your account exists, go to:
#      Account → Tokens → "Show Token"
#
# Then replace 'YYYYYYY' and 'XXXXXX' below with the token
# and secret provided by shinyapps.io.
############################################################
rsconnect::setAccountInfo(
  name   = "cesarivanalvarez",  # your shinyapps.io username
  token  = "YYYYYYY",           # replace with your TOKEN
  secret = "XXXXXX"             # replace with your SECRET
)


############################################################
# STEP 4 — Deploy your app
#
# rsconnect::deployApp() requires the path to the FOLDER
# containing your Shiny app.
#
# ✔ CORRECT:
#     rsconnect::deployApp("C:/EcuadorInamhi/")
#
# ✘ WRONG:
#     rsconnect::deployApp("C:/EcuadorInamhi/app.R")
#     (You must deploy the folder, not the file)
############################################################
rsconnect::deployApp("C:/EcuadorInamhi")


############################################################
# STEP 5 — View real-time logs (helpful for debugging)
#
# If the app fails, this will show:
#   - missing files
#   - missing packages
#   - R code errors
############################################################
rsconnect::showLogs(appName = "Ecuador_Inamhi", streaming = TRUE)
