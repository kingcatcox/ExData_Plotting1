#
# Download the zip into a temp file and read into a table 
#
temp <- tempfile()
download.file("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip",temp)
data <- read.csv(unz(temp, "household_power_consumption.txt"),stringsAsFactors = FALSE,sep=';')
unlink(temp)
#
# Load libraries we need 
#
library(dplyr)
library(lubridate)
library(readr)
#
# filter out the records we need, convert the date column to date/time and convert global power to
# a numeric value
#
globpwr<-data %>% 
  as_tibble() %>% 
  mutate(Date = dmy(Date),Time = hms(Time)) %>% 
  mutate(DateTime = make_datetime(year=year(Date),month=month(Date),day=day(Date),
                                  hour=hour(Time),min=minute(Time))) %>%
  filter(between(DateTime,ymd_hm("2007-02-01 00:00"),ymd_hm("2007-02-02 23:59"))) %>%
  mutate( Date = NULL, Time = NULL) %>%
  select( DateTime, Global_active_power:Sub_metering_3) %>%
  mutate( Global_active_power = parse_number(Global_active_power) )
# Open the .png file for plot
png(file="plot1.png")
#
# Now build a histogram in one quick command 
#
hist(globpwr$Global_active_power,col="red",main="Global Active Power",
     xlab="Global Active Power (kilowatts)",ylab="Frequency")
dev.off()