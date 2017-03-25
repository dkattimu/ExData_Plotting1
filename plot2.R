library(data.table)
library(tidyverse)
#select working folder. This will open a dialog for user to select the working directory which is then assigned to the folder variable
folder =choose.dir(caption = "Select the Working Directory") 
if( !is.na(folder) ){ # only process if a folder is chosen
    setwd(folder) # set working directory
    #name of data file
    fileName ="household_power_consumption.txt"
    cCls<-rep("character",9) # data type for columns
    system.time(data<-fread(fileName,sep =";",colClasses = cCls )) # read file
    system.time(data <-data %>% mutate(Date=as.Date(Date,format="%d/%m/%Y"))) # read data
    
    fromDate=as.Date("1/2/2007",format="%d/%m/%Y") # from date
    toDate=as.Date("2/2/2007",format="%d/%m/%Y") # to Date
    #extract data to plot
    system.time(subData<-data %>%
                          filter(Date>= fromDate, Date<=toDate) %>%  # get data for given dates
                          select(Global_active_power,Date,Time) %>%  # select variables
                          transmute(Global_active_power=as.numeric(Global_active_power),DateTime=paste(Date, Time)) %>% # Just get the date plus time and active power variables
                          mutate(Date_time=as.POSIXct(DateTime)) # way of getting date and time as a POSIXct object to get right label on x-axis
                )
    # plot graph
    with(subData,
         plot(x=Date_time, y=Global_active_power, type="l", ylab="Global Active Power (kilowatts)", xlab="")
    )

    #copy plot to png file
    dev.copy(png, file="plot2.png")
    dev.off()
    
}

