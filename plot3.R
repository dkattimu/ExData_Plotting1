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
    system.time(subData<-data %>% 
                     filter(Date>= fromDate, Date<=toDate) %>%  # get data for given dates
                     select(Global_active_power,Sub_metering_1,Sub_metering_2,Sub_metering_3,Date,Time)%>%   # select variables
                     filter(Sub_metering_1!="?",Sub_metering_2!="?",Sub_metering_3!="?")%>%  # remove the "?"/ empty values
                     transmute(Global_active_power=as.numeric(Global_active_power),Sub_metering_1=as.numeric(Sub_metering_1),
                                   Sub_metering_2=as.numeric(Sub_metering_2),Sub_metering_3=as.numeric(Sub_metering_3),DateTime=paste(Date, Time))%>% 
                     mutate(Date_time=as.POSIXct(DateTime))# Just get the date plus time and active power variables
    )
    #plot sub-metering_1
    with(subData, plot(x=Date_time,y=Sub_metering_1, type="l", col="black",ylab="Energy sub metering",xlab = ""))
    with(subData, lines(x=Date_time,y=Sub_metering_2, col="red"))
    with(subData, lines(x=Date_time,y=Sub_metering_3, col="blue"))
    legend("topright",lty=c(1,1,1),col=c("black","red","blue"), legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"))
    #copy plot to png file
    dev.copy(png, file="plot3.png")
    dev.off()
}

