install.packages('rsconnect')

library(rsconnect)

rsconnect::setAccountInfo(name='cesarivanalvarez', token='YYYYYYY', secret='XXXXXX') # Here put your credentials get when you create an account on ShinyApps

# Here put the folder where is your shiny app. It must be called app.R inside the folder you choose, additionally the data you used must be in the same folder
rsconnect::deployApp('C:/EcuadorInamhi')
rsconnect::showLogs(appName = "Ecuador_Inamhi", streaming = TRUE)
