install.packages('rsconnect')

rsconnect::setAccountInfo(name='cesarivanalvarez', token='F31E90D4FAF737354ADF3C313A5D5191', secret='XXXXXXXX') 

library(rsconnect)
# Here put the folder where is your shiny app. It must be called app.R inside the folder you choose - CHANGE SECRET XXX FOR YOUR CREDENTIAL
rsconnect::deployApp('C:/DWD_Temperature_App') HERE_REPLACE_FOR_SHINY_CREDENTIALS
rsconnect::showLogs(appName = "DWD_Temperature_Visualizer", streaming = TRUE)

