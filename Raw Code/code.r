

df <- read.csv("repdata_data_StormData.csv.bz2")

library(tidyverse)
library(lubridate)
library(scales)

df$year <- year(as.Date(df$BGN_DATE, format = "%m/%d/%Y"))
df %>% ggplot(aes(year)) +
        geom_histogram(binwidth = 1, color="Black", fill="lightblue") +
        ylab("Number of Events") +
        xlab("Year")

df1 <- df %>% filter(year >= 1995)


deaths <- df1 %>% group_by(EVTYPE) %>%
        summarize(Fatalites = sum(FATALITIES)) %>%
        arrange(desc(Fatalites)) %>%
        top_n(10)

deaths %>% ggplot(aes(x = reorder(EVTYPE,Fatalites),Fatalites)) +
        geom_col(col = "Black", fill = "Red") +
        coord_flip() +
        ylab("Total Fatalities") +
        xlab("Event Type") +
        ggtitle("Fatalities by Weather Event") +
        scale_y_continuous(breaks = seq(0,2000,len = 9))


injured <- df1 %>% group_by(EVTYPE) %>%
        summarize(Injuries = sum(INJURIES)) %>%
        arrange(desc(Injuries)) %>%
        top_n(10)

injured %>% ggplot(aes(x = reorder(EVTYPE,Injuries),Injuries)) +
        geom_col(col = "Black", fill = "Yellow") +
        coord_flip() +
        ylab("Total Injuries") +
        xlab("Event Type") +
        ggtitle("Injuries by Weather Event") +
        scale_y_continuous(breaks = seq(0,25000,len = 6))


real_damage <- function(number, multiplier){
        if(multiplier == 'B'){
                return(number * 1000000000)
        } else if(multiplier == 'M'){
                return(number * 1000000)
        } else if(multiplier == 'K'){
                return(number * 1000)
        } else{
                return(number)
        }
}

df1$prop_damage <- mapply(real_damage, df1$PROPDMG, df1$PROPDMGEXP)
df1$crop_damage <- mapply(real_damage,df1$CROPDMG,df1$CROPDMGEXP)
df1$econ_damage <- df1$prop_damage + df1$crop_damage

damage <- df1 %>% group_by(EVTYPE) %>% 
        summarize(Total.Damage = sum(econ_damage)) %>% 
        arrange(desc(Total.Damage)) %>%
        top_n(10)

damage %>% ggplot(aes(x = reorder(EVTYPE,Total.Damage),Total.Damage)) +
        geom_col(col = "Black", fill = "Orange") +
        coord_flip() +
        ylab("Total Damage (Billons of Dollars)") +
        xlab("Event Type") +
        ggtitle("Economic Damage by Weather Event") +
        scale_y_continuous(breaks = seq(0, 150000000000, len = 7),labels = unit_format(unit="", scale = 1e-9))

