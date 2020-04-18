

df <- read.csv("repdata_data_StormData.csv.bz2")

library(tidyverse)
df %>% group_by(EVTYPE) %>%
        summarize(Casualities = sum(FATALITIES + INJURIES)) %>%
        arrange(desc(Casualities))

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

df$prop_damage <- mapply(real_damage, df$PROPDMG, df$PROPDMGEXP)
df$crop_damage <- mapply(real_damage,df$CROPDMG,df$CROPDMGEXP)
df$econ_damage <- df$prop_damage + df$crop_damage

df %>% group_by(EVTYPE) %>% 
        summarize(Total.Damage = sum(econ_damage)) %>% 
        arrange(desc(Total.Damage))
