library(dplyr)

plot2 <- function() {
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
    par(mfrow = c(1,1))
    
    # Plotting against dateTie rather than simply Day gives us our expectation
    dateTime   <- as.POSIXlt(paste(as.Date(filtered$Date, format="%d/%m/%Y"), filtered$Time, sep=" "))
    with(household_data, plot(dateTime, Global_active_power, xlab = "" , ylab = "Global Active Power (killowatts)", type = "l"))
    
    
    # write to file 
    dev.copy(png, width=480, height=480, file="plot2.png")
    dev.off()
}