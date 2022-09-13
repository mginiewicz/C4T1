# Title: C4T1 - Domain Research and Exploratory Data Analysis

#Updated:  9/13/2022


###############
# Project Notes
###############


# Clear console: CTRL + L


###############
# Housekeeping
###############

# Clear objects if necessary
rm(list = ls())

# get working directory
getwd()

# set working directory 
setwd("C:/Users/giniewic/OneDrive - HP Inc/Documents/Personal/UT Data Analytics Cert/Course 4/C4T1")

# see files in working directory
dir()



###############
# Load packages
###############
install.packages("Rtools")
install.packages("caret")
install.packages("corrplot")
install.packages("readr")
install.packages("mlbench")
install.packages("doParallel")
install.packages("reshape2")
install.packages("dplyr")
install.packages("arules")
install.packages("arulesViz")
install.packages("RMariaDB")
install.packages("lubridate")
library(caret)
library(corrplot)
library(readr)
library(mlbench)
library(doParallel)
library(e1071)
library(gbm)
library(ggplot2)
library(writexl)
library(reshape2)
library(dplyr)
library(arules)
library(arulesViz)
library(RMariaDB)
library(lubridate)



#####################
# Parallel Processing
#####################

#detectCores()         #detect number of cores
#cl <- makeCluster(2)  # select number of cores
#registerDoParallel(cl) # register cluster
#getDoParWorkers()      # confirm number of cores being used by RStudio
#  Stop Cluster -- After performing tasks, make sure to stop cluster
#stopCluster(cl)
#detectCores()


####################
# Import data
####################


# Create DB connection
con = dbConnect(MariaDB(), user='deepAnalytics',password='Sqltask1234!', dbname='dataanalytics2018', host='data-analytics-2018.cbrosir2cswx.us-east-1.rds.amazonaws.com')

# List tables in DB
dbListTables(con)

#[1] "iris"    "yr_2006" "yr_2007" "yr_2008" "yr_2009" "yr_2010"

######################
# Save datasets
######################




##################
# Evaluate data
##################

### Use Iris Table as example ###

# List attributes in table
dbListFields(con, 'iris')

# Query the database
irisALL <- dbGetQuery(con, "SELECT * FROM iris")
irisSELECT <- dbGetQuery(con, "SELECT SepalLengthCm, SEpalWidthCm FROM iris")

# List attributes in yr_2006 table
dbListFields(con, 'yr_2006')
#[1] "id"                    "Date"                 
#[3] "Time"                  "Global_active_power"  
#[5] "Global_reactive_power" "Global_intensity"     
#[7] "Voltage"               "Sub_metering_1"       
#[9] "Sub_metering_2"        "Sub_metering_3" 

dbListFields(con, 'yr_2007')

#*** THE DATA WE WILL PULL FOR EACH YEAR IS Date, Time, Sub_metering_1, Sub_metering_2, Sub_metering_3




##################
# Preprocess data
##################

# Download tables with specified attributes
yr_2006 <- dbGetQuery(con, "SELECT Date, Time, Sub_metering_1, Sub_metering_2, Sub_metering_3 FROM yr_2006")

yr_2007 <- dbGetQuery(con, "SELECT Date, Time, Sub_metering_1, Sub_metering_2, Sub_metering_3 FROM yr_2007")

yr_2008 <- dbGetQuery(con, "SELECT Date, Time, Sub_metering_1, Sub_metering_2, Sub_metering_3 FROM yr_2008")

yr_2009 <- dbGetQuery(con, "SELECT Date, Time, Sub_metering_1, Sub_metering_2, Sub_metering_3 FROM yr_2009")

yr_2010 <- dbGetQuery(con, "SELECT Date, Time, Sub_metering_1, Sub_metering_2, Sub_metering_3 FROM yr_2010")


### Analyze tables

str(yr_2006)
summary(yr_2006)
head(yr_2006)
tail(yr_2006)

### Date & Time are chr data
### All 3 sub_metering attributes are numerical data 

### yr_2006:
#### sub_metering_1: goes from 0-77
#### sub_metering_2: goes from 0-74
#### sub_metering_3: goes from 0-20
#### data from 12/16/2006 to 12/31/2006

str(yr_2007)
summary(yr_2007)
head(yr_2007)
tail(yr_2007)

### yr_2007:
#### sub_metering_1: goes from 0-78
#### sub_metering_2: goes from 0-78
#### sub_metering_3: goes from 0-20
#### data goes from 1/1/2007 to 12/31/2007

str(yr_2008)
summary(yr_2008)
head(yr_2008)
tail(yr_2008)

### yr_2008:
#### sub_metering_1: goes from 0-80
#### sub_metering_2: goes from 0-76
#### sub_metering_3: goes from 0-31
#### data goes from 1/1/2008 to 12/31/2008

str(yr_2009)
summary(yr_2009)
head(yr_2009)
tail(yr_2009)

### yr_2009:
#### sub_metering_1: goes from 0-82
#### sub_metering_2: goes from 0-77
#### sub_metering_3: goes from 0-31
#### data goes from 1/1/2009 to 12/31/2009

str(yr_2010)
summary(yr_2010)
head(yr_2010)
tail(yr_2010)

### yr_2010:
#### sub_metering_1: goes from 0-88
#### sub_metering_2: goes from 0-80
#### sub_metering_3: goes from 0-31
#### data goes from 1/1/2010 to 11/26/2010

# Create Primary Data Frame with multi-year dataset
# Remove 2006 and 2010 since they do not contain an entire year
data <- bind_rows(yr_2007, yr_2008, yr_2009)

str(data)
summary(data)
head(data)
tail(data)

### Date & Time are chr data
### All 3 sub_metering attributes are numerical data 
#### sub_metering_1: goes from 0-82;  mean: 1.159
#### sub_metering_2: goes from 0-78;  mean: 1.343
#### sub_metering_3: goes from 0-31;  mean: 6.217
#### data goes from 01/01/2007 to 12/31/2009


# Combine Date & Time into a new column

data <-cbind(data,paste(data$Date,data$Time), stringsAsFactors=FALSE)

colnames(data)

# Rename the new DateTime column
colnames(data)[6] <- "DateTime"

colnames(data)

summary(data)

# Move the DateTime attribute within the dataset
data <- data[,c(ncol(data),1:(ncol(data)-1))]

# Check columns
head(data)

# Convert DateTime from chr to POSIXct
data$DateTime <- as.POSIXct(data$DateTime, "%Y/%m/%d %H:%M:%S")

# Add the time zone
attr(data$DateTime, "tzone") <- "UTC"

# check columns again
str(data)
summary(data)
head(data)
tail(data)

# Create "year" attribute with lubridate
data$year <- year(data$DateTime)

# Check data with new attribute
str(data)
summary(data)
head(data)
tail(data)

# Create month attribute with lubridate
data$month <- month(data$DateTime)
summary(data)

# Create quarter attribute with lubridate
data$quarter <- quarter(data$DateTime)
summary(data)
head(data)
tail(data)

#####################
# EDA/Visualizations
#####################

#--- Statistics ---#
summary(data)

summary(data$Sub_metering_1)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.000   0.000   0.000   1.159   0.000  82.000 

summary(data$Sub_metering_2)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.000   0.000   0.000   1.343   1.000  78.000 

summary(data$Sub_metering_3)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.000   0.000   1.000   6.216  17.000  31.000

## sub_metering 3 uses the most power on average; 
##  sub_metering 1 and 2 seem to have some outliers, since their max is much  higher than sub_metering 3


#--- Plots ---#
barplot_Submeter1_year<-ggplot(data=data, aes(x=year, y=Sub_metering_1)) +
  geom_bar(stat="identity") +
  ggtitle("Sub_metering_1 by year") + 
  theme(plot.title = element_text(hjust = 0.5))

barplot_Submeter1_year

# Create smaller dataset with just meters + year 
year_data <- data[, c(4,5,6,7)]

summary(year_data)

# move year to first column 
year_data <- year_data[,c(ncol(year_data),1:(ncol(year_data)-1))]

summary(year_data)


# bar plot 
gg <- melt(year_data,id="year")

summary(gg)
head(gg)
tail(gg)
tail(data)

ggplot(gg, aes(x=variable, y=value, fill=factor(year))) +
  stat_summary(fun=mean, geom="bar", position=position_dodge(1)) +
  scale_color_discrete("year") +
  labs(title="Average Sub-Metering by Year", x="Metering", y= "Average") +
  theme(plot.title = element_text(hjust = 0.5))


# Create smaller dataset with just meters + month 
month_data <- data[, c(4,5,6,8)]

summary(month_data)

# move year to first column 
month_data <- month_data[,c(ncol(month_data),1:(ncol(month_data)-1))]

summary(month_data)


# bar plot 
gg1 <- melt(month_data,id="month")

summary(gg1)
head(gg1)
tail(gg1)

ggplot(gg1, aes(x=variable, y=value, fill=factor(month))) +
  stat_summary(fun=mean, geom="bar", position=position_dodge(1)) +
  scale_color_discrete("month") +
  labs(title="Average Sub-Metering by Month", x="Metering", y= "Average") +
  theme(plot.title = element_text(hjust = 0.5))


# Create smaller dataset with just meters + quarter 
quarter_data <- data[, c(4,5,6,9)]

summary(quarter_data)

# move year to first column 
quarter_data <- quarter_data[,c(ncol(quarter_data),1:(ncol(quarter_data)-1))]

summary(quarter_data)


# bar plot 
gg2 <- melt(quarter_data,id="quarter")

summary(gg2)
head(gg2)
tail(gg2)

ggplot(gg2, aes(x=variable, y=value, fill=factor(quarter))) +
  stat_summary(fun=mean, geom="bar", position=position_dodge(1)) +
  scale_color_discrete("quarter") +
  labs(title="Average Sub-Metering by Quarter", x="Metering", y= "Average") +
  theme(plot.title = element_text(hjust = 0.5))


# Create box plots to look for outliers

ggplot(gg, aes(x=variable, y=value)) +
  geom_boxplot()

### Submetering 1 & 2 have a lot of outliers


##################
# Train/test sets
##################




#####################
# Modeling
#####################



##################
# Improve Model
##################



#########################
# Visualize Model Results
#########################

