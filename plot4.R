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
    
    system.time(subData<-data %>% 
                    filter(Date>= fromDate, Date<=toDate) %>% # get data for given dates
                    filter(Sub_metering_1!="?",Sub_metering_2!="?",Sub_metering_3!="?",Global_active_power!="?")%>% # remove the "?"/ empty values
                    filter(Voltage!="?",Global_reactive_power!="?")%>% # remove the "?"/ empty values
                    mutate(Sub_metering_1=as.numeric(Sub_metering_1),Sub_metering_2=as.numeric(Sub_metering_2),
                           Sub_metering_3=as.numeric(Sub_metering_3),Global_active_power=as.numeric(Global_active_power),DateTime=paste(Date,Time),
                           Voltage=as.numeric(Voltage),Global_reactive_power=as.numeric(Global_reactive_power)) %>%
                    mutate(Date_time=as.POSIXct(DateTime)) # way of getting date and time as a POSIXct object to get right label on x-axis
    )
    #set graph structure
    par(mfrow=c(2,2))
    #plot 1
    with(subData,plot(x=Date_time, y=Global_active_power, type="l", ylab="Global Active Power", xlab=""))
    #plot 2
    with(subData,plot(x=Date_time, y=Voltage, type="l", ylab="Voltage", xlab="datetime"))
    #plot 3
    with(subData, plot(x=Date_time,y=Sub_metering_1, type="l", col="black",ylab="Energy sub metering", xlab=""))
    with(subData, lines(x=Date_time,y=Sub_metering_2, col="red"))
    with(subData, lines(x=Date_time,y=Sub_metering_3, col="blue"))
    legend("topright",lty=c(1,1,1),col=c("black","red","blue"), legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"))
    #plot 4
    with(subData,plot(x=Date_time, y=Global_reactive_power, type="l", ylab="Global_reactive_power", xlab="datetime"))
    #copy plot to png file
    dev.copy(png, file="plot4.png")
    dev.off()
}