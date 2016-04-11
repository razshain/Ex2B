install.packages("plyr")
install.packages("jsonlite")
install.packages("descr")
install.packages("ROCR")
install.packages("mlbench")
install.packages("pbkrtest")
install.packages("caret")
install.packages("bitops")
install.packages("lme4")
install.packages("Matrix")
library(plyr)
library(jsonlite)
library(descr, warn.conflicts = FALSE, quietly = TRUE, verbose = FALSE)
library( ROCR , warn.conflicts = FALSE, quietly = TRUE, verbose = FALSE)
library(foreach)
library(RCurl)
library(mlbench)
library(Matrix)
library(lme4)
library(bitops)
library(pbkrtest)
library(caret)

setwd("C:\\Users\\pc\\Dropbox\\Raz&Livnat\\lastYear\\b\\ofrit\\ass1\\part 2")
all_data <- data.frame(matrix(NA, nrow=0, ncol=1))
getHistoricalWeather <- function(country,city)
{
  base.url <- 'http://api.wunderground.com/api/e62e03d8fcd25e54/'
  # compose final url
  final.url <- paste(base.url, 'history/q/',country,'/',city, '.json', sep='')
  
  
  # reading in as raw lines from the web service
  conn <- url(final.url)
  raw.data <- readLines(conn, n=-1L, ok=TRUE)
  # Convert to a JSON
  weather.data <- fromJSON(paste(raw.data, collapse=""))
  
  weather.data$history$observations$date$mon
  data<- weather.data$history$observations
  hour <-data[1]$date$hour
  df2<-data.frame(city,data[1]$date$mon,hour,data[3],data[4],data[5],data[6],data[7],data[8],data[9],data[10],data[11],data[12],data[14],data[15],data[16],data[17],data[18])
  close(conn)
  return(df2)
}


weather.data <- getHistoricalWeather('Israel','Tel Aviv-Yafo')
weather.data <- rbind(as.matrix(weather.data), as.matrix(getHistoricalWeather('Israel','Holon')))
weather.data <- rbind(as.matrix(weather.data), as.matrix(getHistoricalWeather('Israel','Rishon LeZiyyon')))
weather.data <- rbind(as.matrix(weather.data), as.matrix(getHistoricalWeather('Israel','Bat Yam')))
weather.data <- rbind(as.matrix(weather.data), as.matrix(getHistoricalWeather('Israel','Netanya')))
weather.data <- rbind(as.matrix(weather.data), as.matrix(getHistoricalWeather('Israel','Hadera')))
weather.data <- rbind(as.matrix(weather.data), as.matrix(getHistoricalWeather('Israel','Ramat Gan')))
weather.data <- rbind(as.matrix(weather.data), as.matrix(getHistoricalWeather('Israel','Haifa')))
weather.data <- rbind(as.matrix(weather.data), as.matrix(getHistoricalWeather('Israel','Kfar Saba')))
weather.data <- rbind(as.matrix(weather.data), as.matrix(getHistoricalWeather('Israel','Nesher')))
weather.data <- rbind(as.matrix(weather.data), as.matrix(getHistoricalWeather('Israel','Zefat')))
weather.data <- rbind(as.matrix(weather.data), as.matrix(getHistoricalWeather('Israel','Lod')))
weather.data <- rbind(as.matrix(weather.data), as.matrix(getHistoricalWeather('Israel','Yavne')))
weather.data <- rbind(as.matrix(weather.data), as.matrix(getHistoricalWeather('Israel','Dimona')))
weather.data <- rbind(as.matrix(weather.data), as.matrix(getHistoricalWeather('Israel','Gan Yavne')))
weather.data <- rbind(as.matrix(weather.data), as.matrix(getHistoricalWeather('Israel','Ramla')))
weather.data <- rbind(as.matrix(weather.data), as.matrix(getHistoricalWeather('Israel','Rishon Leziyyon')))
write.csv(weather.data,'weatherData.csv')



df<- as.data.frame(weather.data,col.names = names(weather.data), stringsAsFactors = default.stringsAsFactors())
df<-as.data.frame(weather.data, row.names = NULL, optional = FALSE,cut.names = FALSE, col.names = names(weather.data), fix.empty.names = TRUE,stringsAsFactors = default.stringsAsFactors())

#df$city<- as.numeric(df$city)
df$data.1..date.mon<- NULL #as.numeric(df$data.1..date.mon)
df$hour<-NULL
df$tempi<- as.numeric(df$tempi)
df$tempm<- as.numeric(df$tempm)
df$tempi<- as.numeric(df$tempi)
df$dewptm<- as.numeric(df$dewptm)
df$dewpti<- as.numeric(df$dewpti)
df$hum<- as.numeric(df$hum)
df$wspdm<- as.numeric(df$wspdm)
df$wspdi<- as.numeric(df$wspdi)
df$wgustm<- as.numeric(df$wgustm)
df$wgusti<- as.numeric(df$wgusti)
df$wdird<- as.numeric(df$wdird)
df$vism<- as.numeric(df$vism)
df$visi<- as.numeric(df$visi)
df$pressurem<- as.numeric(df$pressurem)
df$pressurei<- as.numeric(df$pressurei)
df$windchillm<- as.numeric(df$windchillm)
sapply(df, class)

barplot(table(df$city),col="pink",main="")
#weather.data$hour <- as.numeric(weather.data$hour)
hist(df$tempm,col="green")
abline(v=12,lwd=2,col="red")
abline(v=median(df$tempm),lwd=4,col="yellow")
#boxplot(Number_of_Vehicles~Number_of_Casualties,data=df,col="red")

## build a correlation matrix based on the first 100000 rows ##
cor(df[sapply(df, function(x) !is.numeric(x))])
corr.matrix = cor(df, use = "pairwise.complete.obs")

corr.matrix[is.na(corr.matrix)] = 0
corr.matrix
write.csv(corr.matrix,'corrMatrixWeather.csv')

corr.list = foreach(i = 1:nrow(corr.matrix)) %do% {
  rownames(corr.matrix[corr.matrix[,i] > 0.996,])
}
## remove empty sets ##
corr.list = corr.list[sapply(corr.list, function(x) length(x) > 0 )]
## remove duplicated sets ##
corr.list = unique(corr.list)
corr.list

varX<-apply(df, 2, var)
varX
write.csv(varX,'varianceWeather.csv')

