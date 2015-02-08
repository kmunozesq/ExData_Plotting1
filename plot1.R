plot1 <- function()
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
        dt_subset <- mutate(dt_subset, Global_active_power = as.numeric(dt_subset$Global_active_power))
        #Until here the data is as required in dt_subset
        
                
        # Construct PNG file with a width of 480 pixels and a height of 480 pixels
        #dev.size(units=c("px"))  #the default is 7 inches
        #6.4 inches is 480 pixels
        windows(width=6.4, height=6.4)
        with(dt_subset, hist(dt_subset$Global_active_power, col= "red", main =  paste("Global Active Power"), ylim= range(0:1200), breaks=12,xlab = "Global Active Power (Kilowatts)"))
        dev.copy(png, file="plot1.png")
        dev.off()
        
}