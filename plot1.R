library(dplyr)

plot1 <- function() {
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
    }
    
    
    datafilename =  "data/household_power_consumption.txt"
    household_data <- readFilteredData(datafilename)
    
    print("grapthing data")    
    par(mfrow = c(1,1))
    
    hist(household_data$Global_active_power, col = "Red", xlab="Global Active Power (killowatts)", main= "Global Active Power")
    
    # write to file
    dev.copy(png, width=480, height=480, file="plot1.png")
    dev.off()
}