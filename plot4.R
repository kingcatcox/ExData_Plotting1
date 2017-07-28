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
  mutate( Sub_metering_1 = parse_number(Sub_metering_1) ) %>%
  mutate( Sub_metering_2 = parse_number(Sub_metering_2) ) %>%
  mutate( Sub_metering_3 = parse_number(Sub_metering_3) ) %>%
  mutate( Voltage = parse_number(Voltage) ) %>%
  mutate( Global_active_power = parse_number(Global_active_power) ) %>%
  mutate( Global_reactive_power = parse_number(Global_reactive_power) ) 

# Open the .png file for plot
png(file="plot4.png")
#
# Now build a panel 
#
par(mfrow=c(2,2))
#
# The plot from part 2
#
with(globpwr,plot(DateTime,Global_active_power,type="l",xlab="",
                  ylab="Global Active Power (kilowatts)"))
#
with(globpwr,plot(DateTime,Voltage,type="l",xlab="datetime",
                  ylab="Voltage"))
#
with(globpwr,plot(DateTime,Sub_metering_1,col="black",type="l",xlab="",
                  ylab="Energy sub metering"))
with(globpwr,lines(DateTime,Sub_metering_2,col="blue"))
with(globpwr,lines(DateTime,Sub_metering_3,col="red"))
legend("topright",col=c("black","blue","red"),
       legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"),lty=1,lwd=2)
#
with(globpwr,plot(DateTime,Global_reactive_power,type="l",xlab="datetime",
                  ylab="Global_reactive_power"))

dev.off()