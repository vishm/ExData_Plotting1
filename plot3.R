library(dplyr)

plot3 <- function() {
    readFilteredData <- function(datafilename) {
        
        print("loading data")
        
        data <- read.csv(datafilename, sep = ";", 
                         na.strings = "?", 
                         colClasses = c("character", "character", "numeric", 
                                        "numeric", "numeric", "numeric", 
                                        "numeric", "numeric","numeric"))
        
        data <- tbl_df(data)
        data$Date <- as.Date(data$Date, "%d/%m/%Y")
        
        print("fitering data")
        filtered <- filter(data, Date %in% c(as.Date("2007-02-02", "%Y-%m-%d"),as.Date("2007-02-01", "%Y-%m-%d")))
        
        filtered
    }
    
    
    datafilename =  "data/household_power_consumption.txt"
    household_data <- readFilteredData(datafilename)
    
    print("grapthing data")
    png(width=480, height=480, file="plot3.png") # prevent legend issue plot direct
    
    par(mfrow = c(1,1))
    
    # Plotting against dateTie rather than simply Day gives us our expectation
    dateTime   <- as.POSIXlt(paste(as.Date(filtered$Date, format="%d/%m/%Y"), filtered$Time, sep=" "))
    
    with(household_data, plot(dateTime, Sub_metering_1,  xlab = "", 
                              ylab = "Energy sub metering",type= "n")) 
    with(household_data, lines( dateTime, Sub_metering_1, col = "black"))
    with(household_data, lines( dateTime, Sub_metering_2, col = "red"))
    with(household_data, lines( dateTime, Sub_metering_3, col = "blue"))
    legend("topright", 
           lty = c(1,1,1), col = c("black", "red", "blue"), 
           legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
    
    
    # write to file    
    dev.off()
}