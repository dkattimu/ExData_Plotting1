
library(data.table)
library(tidyverse)
#select working folder. This will open a dialog for user to select the working directory which is then assigned to the folder variable
folder =choose.dir(caption = "Select the Working Directory") 
if( !is.na(folder) ){ #

    setwd(folder) # set working directory
    #name of data file
    fileName ="household_power_consumption.txt"
    cCls<-rep("character",9) # data type for columns
    system.time(data<-fread(fileName,sep =";",colClasses = cCls )) # read file
    system.time(data <-data %>% mutate(Date=as.Date(Date,format="%d/%m/%Y"))) # read data
    
    fromDate=as.Date("1/2/2007",format="%d/%m/%Y") # from date
    toDate=as.Date("2/2/2007",format="%d/%m/%Y") # to Date
    # extract the subset of
    system.time(subData<-data %>% 
                    filter(Date>= fromDate,Date<=toDate) %>% # get data for given dates
                    select(Date,Global_active_power) %>% # select variable
                    filter(Global_active_power!="?")%>% # remove the "?"/ empty values
                    mutate(Global_active_power=as.numeric(Global_active_power)) # convert string to numerical
                )
    
    # plot histogram
    
    with(subData,
         hist(Global_active_power,main="Global Active Power ",col="red", xlab="Global Active Power (kilowatts)", ylab="Frequency")
         )

    #copy plot to png file
    dev.copy(png, file="plot1.png") # default is 480 x480
    dev.off()
}