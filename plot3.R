plot3 <- function()
{
        #Loading, reading and splitting the data
        household_file <- file.path(getwd(),"household_power_consumption.txt")
        household_data <- read.csv(household_file, header = TRUE, sep = ";", quote = "\"",dec = ".", fill = TRUE, stringsAsFactors = FALSE)
        
        library(dplyr)
        #Casting data as Date
        household_data <- mutate(household_data, Date = as.Date(household_data$Date, format = "%d/%m/%Y"))        
        #Subseting only to the necessary dates
        dt_subset <- filter(household_data, Date == "2007-02-01" | Date == "2007-02-02")
        #Transform the data to numeric
        dt_subset <- mutate (dt_subset, 
                             Sub_metering_1 = as.numeric(dt_subset$Sub_metering_1),
                             Sub_metering_2 = as.numeric(dt_subset$Sub_metering_2), 
                             Sub_metering_3 = as.numeric(dt_subset$Sub_metering_3))
        #Transform the data to time
        #dt_subset <- mutate(dt_subset, Time = time(dt_subset$Time, format = "%h:%m:%s"))
        dt_subset <- mutate (dt_subset, DateTime = paste(dt_subset$Date, dt_subset$Time, sep=" "))
        dt_subset <- mutate (dt_subset, DateTime = as.POSIXct(DateTime, format = '%Y-%m-%d %H:%M:%S',tz='GMT'))
        daterange=c(as.POSIXlt(min(dt_subset$DateTime)), as.POSIXlt(max(dt_subset$DateTime)))
        #Until here the data is as required in dt_subset
        dt_subset
                
        # Construct PNG file with a width of 480 pixels and a height of 480 pixels
        #dev.size(units=c("px"))  #the default is 7 inches
        #6.4 inches is 480 pixels
        windows(width=6.4, height=6.4)
        plot(dt_subset$Sub_metering_1 ~ dt_subset$DateTime, dt_subset, xaxt = "n", type='l', col="black", ylab="Energy sub metering", xlab= "")
        lines(dt_subset$Sub_metering_2 ~ dt_subset$DateTime, dt_subset, col="red")
        lines(dt_subset$Sub_metering_3 ~ dt_subset$DateTime, dt_subset, col="blue")
        axis.POSIXct(1, at=seq(daterange[1], daterange[2], by="day"), format="%a")
        legend("topright",c("Sub_metering_1","Sub_metering_2", "Sub_metering_3"), lty=c(1,1,1), col=c("black", "red","blue"))
        dev.copy(png, file="plot3.png")        
        dev.off()
        
}