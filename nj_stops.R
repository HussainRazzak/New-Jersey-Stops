library(stringr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(chron)
library(lubridate)
library(chron)
library(fpp2)
library(uroot)
library(forecast)
library(fpp2)
library(e1071)
library(cowplot)
library(class)
library(fpc)
library(cluster)

# READ CSV FILES
nj_csv <- read.csv("C:/Users/razzakh/Desktop/NJ_cleaned.csv")

#read in vehicle colors
veh_colors  <- read.csv("C:/Users/razzakh/Desktop/colors.csv")

#read in holidays
holidays <- read.csv("C:/Users/razzakh/Desktop/holidays.csv")

#extract date and year
holidays$doh <- as.Date(holidays$doh)
holidays$year <- substr(holidays$doh, 1,4)
#add date and year back to the dataframe
nj_h <- left_join(nj_2016,holidays,by="stop_date")

holidays$stop_date <- holidays$doh
#merge dataframe with condensed vehicle colors dataframe
nj_csv <- merge(nj_csv,veh_colors) 

#lower case the county name
nj_csv$county <- tolower(gsub('([A-z]+) .*', '\\1', nj_csv$county_name))

#create a separate year column
nj_csv$year <- substr(nj_csv$stop_date, 1,4)

#create a column and format the stop_date as a date object
nj_csv$occur <- as.Date(nj_csv$stop_date)

#create a separate column month
nj_csv$occur_month <- month(nj_csv$occur)

#create a separate column day 
nj_csv$day <- day(nj_csv$occur)

#strip spaces from the violation
nj_csv$vio_clean <- str_replace_all(nj_csv$violation, fixed(" "), "")

#separate the violations and separate them into individal rows
nj_csv_v <- separate_rows(nj_csv,vio_clean)
unique(nj_csv_v$vio_clean)
#determine if color and make of car increases chances of a police stop
nj_2016 <- subset(nj_csv, (stop_outcome=="Summons") & year==2015 & location_raw =="West Paterson Boro")

head(make_group)
plot(make_group)

#function to return different plots
# parameter: plot_type
# 1: stop outcome
# 2: vehicle make
# 3: vehicle color
# 4: driver gender

plot_year <- function(x_year,plot_type)
{
  nj_year <- subset(nj_csv,year==x_year)
  #calculate 
  make_group <- nj_year %>%
    group_by(stop_date) %>%
    summarise(count=n(),
              summons=sum(stop_outcome=="Summons"),
              warnings = sum(stop_outcome=="Warning"),
              light    = sum(color_cat=="light"),
              dark     = sum(color_cat=="dark"),
              honda    = sum(vehicle_make =="Honda"),
              toyota   = sum(vehicle_make =="Toyota"),
              ford     = sum(vehicle_make =="Ford"),
              chevy    = sum(vehicle_make =="Chevrolet"),
              nissan   = sum(vehicle_make =="Nissan"),
              dodge    = sum(vehicle_make =="Dodge"),
              female   = sum(driver_gender=="F"),
              male     = sum(driver_gender=="M"),
              asian    = sum(driver_race=="Asian"),
              black    = sum(driver_race=="Black"),
              hispanic = sum(driver_race=="Hispanic"),
              white    = sum(driver_race=="White")

              )
  #return grouped dataset
  if(plot_type == 999)
  {
    return(make_group)
  }
    
  make_group_t <- ts(make_group[c("summons","warnings")])
  
  if(plot_type == 1)
  {
    make_group_t <- ts(make_group[c("summons","warnings")])

    time_plot <-autoplot(make_group_t,facets = F) +
      geom_smooth() +
      labs(title=paste("Stops by stop outcome in", x_year, sep=" "),
           y = "Number of Stops",
           x = "days of the year")      
  }
  else if (plot_type == 2)
  {
    make_group_t <- ts(make_group[c("honda","toyota","ford","chevy","nissan","dodge")])

    time_plot <-autoplot(make_group_t,facets = F) +
      geom_smooth() +
      labs(title=paste("Stops by vehicle make in", x_year, sep=" "),
           y = "Number of Stops",
           x = "days of the year")      
  }
  else if(plot_type == 3)
  {
    make_group_t <- ts(make_group[c("light","dark")])
    
    time_plot <-autoplot(make_group_t,facets = F) +
      geom_smooth() +
      labs(title=paste("Stops by vehicle color in", x_year, sep=" "),
           y = "Number of Stops",
           x = "days of the year")          
  }
  else if (plot_type ==4)
  {
    make_group_t <- ts(make_group[c("female","male")])
    
    time_plot <-autoplot(make_group_t,facets = F) +
      geom_smooth() +
      labs(title=paste("Stops by driver gender in", x_year, sep=" "),
           y = "Number of Stops",
           x = "days of the year")              
  }
  else if (plot_type ==5)
  {
    make_group_t <- ts(make_group[c("asian","black","hispanic","white")])
    
    time_plot <-autoplot(make_group_t,facets = F) +
      geom_smooth() +
      labs(title=paste("Stops by driver gender in", x_year, sep=" "),
           y = "Number of Stops",
           x = "days of the year")              
  }
  
  return(time_plot)
}

year_2015_stop_outcome  = plot_year(2015,1)
year_2015_vehicle_make  = plot_year(2015,2)
year_2015_vehicle_color = plot_year(2015,3)
year_2015_driver_gender = plot_year(2015,4)
year_2015_driver_race   = plot_year(2015,5)

year_2016_stop_outcome  = plot_year(2016,1)
year_2016_vehicle_make  = plot_year(2016,2)
year_2016_vehicle_color = plot_year(2016,3)
year_2016_driver_gender = plot_year(2016,4)
year_2016_driver_race   = plot_year(2016,5)

year_2015_stop_outcome
year_2015_vehicle_make
year_2015_vehicle_color
year_2015_driver_gender
year_2015_driver_race

year_2016_stop_outcome
year_2016_vehicle_make
year_2016_vehicle_color
year_2016_driver_gender
year_2016_driver_race

# Insight: Vehicle make does not affect number of stop
# Insight: Warnings are issues more often in the beginning of the year, 
#          but it changes towards the summer and it changes again towards the winter
# Insight: vehicle color does not affect stoppage

#linear regression
nj_2015 <- plot_year(2015,999)
nj_2016 <- plot_year(2016,999)

#put month as the categorical variable

nj_2016$month <- month(as.Date(nj_2016$stop_date))
nj_2016$month <- as.factor(nj_2016$month)
nj_2016 <- subset(nj_2016,month==10)
nj_2016 <- nj_2016[,-c(1:2)] 

nj_2015$month <- month(as.Date(nj_2015$stop_date))
nj_2015$month <- as.factor(nj_2015$month)
nj_2015 <- subset(nj_2015,month==10)
nj_2015 <- nj_2015[,-c(1:2)] 

linear_2016_w <- lm(nj_2016$warnings ~ .,data = nj_2016[,-1])
par(mfrow=c(2,2))
plot(linear_2016_w)
summary(linear_2016_w)
 
linear_2016_s <- lm(nj_2016$summons ~ .,data = nj_2016[,-1])
par(mfrow=c(2,2))
plot(linear_2016_s)
summary(linear_2016_s)

##

#clustering
#normalize function
normalize <- function(x){
  return ((x - min(x)) / (max(x) - min(x)))
}
nj_2016_n <- nj_2016[,-17]
nj_2015_n <- nj_2015[,-17]
#normalize the dataset without the categorical column
nj_2016_n <- as.data.frame(lapply(nj_2016_n, normalize))
nj_2015_n <- as.data.frame(lapply(nj_2015_n, normalize))

#set training limits
n=length(nj_2016$month); n
366*.6
nt=220
set.seed(1) ## to make the calculations reproducible in repeated runs
train <- sample(1:n,nt)

#run knn with different k clusters
nearest1 <- knn(train=nj_2016_n[train,],test=nj_2016_n[-train,],cl=nj_2016$month[train],k=1)
nearest5 <- knn(train=nj_2016_n[train,],test=nj_2016_n[-train,],cl=nj_2016$month[train],k=5)
nearest6 <- knn(train=nj_2016_n[train,],test=nj_2016_n[-train,],cl=nj_2016$month[train],k=6)

nearest2 <- knn(train=nj_2015_n,test=nj_2016_n,cl=nj_2016$month,k=2)
nearest3 <- knn(train=nj_2015_n,test=nj_2016_n,cl=nj_2016$month,k=3)
data.frame(nj_2016$month,nearest2,nearest3)

data.frame(nj_2016$month[-train],nearest1,nearest5)

plot(nj_2015_n[,c(3,4)],col=nj_2016$month,cex=.8,main="1-nearest neighbor")
points(nj_2016_n[,c(3,4)],bg=nearest1,pch=21,col=grey(.9),cex=1.25)
nearest2_correct=100*sum(nj_2016$month==nearest2)/(n-nt)
nearest2_correct # 27.2% correct


## plot for k=1 (single) nearest neighbor
plot(nj_2016_n[train,c(3,4)],col=nj_2016$month[train],cex=.8,main="1-nearest neighbor")
points(nj_2016_n[-train,c(3,4)],bg=nearest1,pch=21,col=grey(.9),cex=1.25)

nearest1_correct=100*sum(nj_2016$month[-train]==nearest1)/(n-nt)
nearest1_correct # 27.2% correct

nearest5_correct=100*sum(nj_2016$month[-train]==nearest5)/(n-nt)
nearest5_correct # 28.5% correct

nearest6_correct=100*sum(nj_2016$month[-train]==nearest6)/(n-nt)
nearest6_correct # 29.4% correct

sample_rows <- sample(1:365,50)

nj_2016_k <- kmeans(nj_2016_n[sample_rows,]12)
plotcluster(nj_2016_n[sample_rows,c(1,3,4)], nj_2016_k$cluster)
clusplot(nj_2016_n[sample_rows,c(1,3)], nj_2016_k$cluster,color=TRUE, shade=TRUE,labels=2, lines=0)

dist(nj_2016$month)

# Cluster using single linkage: hclust.single
hclust.single <- hclust(dist(nj_2016$month), method="complete")
# Plot dendrogram of hclust.complete
plot(hclust.single, main="complete")


nj_see_2016 <- nj_2016[c(36,122,335,341,364),]

nj_passaic <- subset(nj_csv,nj_csv$county_name=="Passaic County" & year ==2016)

unique((nj_passaic$location_raw))

1+1
rgroup <- nj_csv %>%
  group_by(location_raw,stop_date) %>%
  summarise(
             asian_s    = sum(driver_race=="Asian" & stop_outcome=="Summons"),
             asian_w    = sum(driver_race=="Asian" & stop_outcome=="Warning"),
             black_s    = sum(driver_race=="Black" & stop_outcome=="Summons"),
             black_w    = sum(driver_race=="Black" & stop_outcome=="Warning"),
             hispanic_s    = sum(driver_race=="Hispanic" & stop_outcome=="Summons"),
             hispanic_w    = sum(driver_race=="Hispanic" & stop_outcome=="Warning"),
             white_s    = sum(driver_race=="White" & stop_outcome=="Summons"),
             white_w    = sum(driver_race=="White" & stop_outcome=="Warning")
             
            )

rgroup_t <- ts(rgroup[c("white_w","white_s")])

cgroup_t <- ts(cgroup[c("warnings","summons","light","dark")])
allgroup_t <- ts(allgroup[c("warnings","summons","light","dark","honda","toyota","ford","chevy","nissan","dodge")])
allgroup_t <- ts(allgroup[c("warnings","light","dark","honda","toyota")])
allgroup_t <- ts(allgroup)


?autoplot
autoplot(rgroup_t,facets = F) +
  geom_smooth() +
  labs("NJ Stops from 2009-2016",
       y = "Number of Stops",
       x = "days of the year")


install.packages("tsfknn")
library(tsfknn)

?knn_forecasting

pred <- knn_forecasting(make_group_2, h = 12, lags = 1:12, k = 2)
pred$prediction # To see a time series with the forecasts
plot(pred) # To see a plot with the forecast