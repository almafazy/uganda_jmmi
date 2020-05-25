
unit_conversions <- read.csv("inputs/Conversion factors for sold loose items.csv",na.strings = c(""," ","NA"))


#Collection_order-----

unit_conversions <-unit_conversions %>% mutate(Month_num = ifelse(Month == "January",1,
                                                                ifelse(Month == "February",2,
                                                                       ifelse(Month == "March",3,
                                                                              ifelse(Month == "July",7,
                                                                                     ifelse(Month == "October",10,
                                                                                            ifelse( Month == "November",11,
                                                                                                    ifelse(Month == "December",12,"Categorise Month")))
                                                                                     )))))

unit_conversions$Date <- paste0(unit_conversions$Year,"-",unit_conversions$Month_num)

unit_conversions <- unit_conversions %>%  arrange(Date) %>% select(Date,Settlement: Observed_price_charcoal)

unit_conversions[unit_conversions == 0] <- NA



#settlement aggregations----
unit_conversions_medians <- unit_conversions %>%  
  select(-Date) %>% 
  group_by(Settlement) %>% 
  summarise_all(funs(median(., na.rm = TRUE)))

colnames(unit_conversions_medians)<-paste0("Median_",colnames(unit_conversions_medians))
names(unit_conversions_medians)[names(unit_conversions_medians) == "Median_Settlement"] <- "settlement"



unit_conversions_max <- unit_conversions %>%  
  select(-Date) %>% 
  group_by(Settlement) %>% 
  summarise_all(funs(max(., na.rm = TRUE)))



colnames(unit_conversions_max)<-paste0("Max_",colnames(unit_conversions_max))
names(unit_conversions_max)[names(unit_conversions_max) == "Max_Settlement"] <- "settlement"




unit_conversions_min <- unit_conversions %>%  
  select(-Date) %>% 
  group_by(Settlement) %>% 
  summarise_all(funs(min(., na.rm = TRUE)))

colnames(unit_conversions_min)<-paste0("Min_",colnames(unit_conversions_min))
names(unit_conversions_min)[names(unit_conversions_min) == "Min_Settlement"] <- "settlement"




unit_conversions_average <- unit_conversions %>%  
  select(-Date) %>% 
  group_by(Settlement) %>% 
  summarise_all(funs(mean(., na.rm = TRUE)))


colnames(unit_conversions_average)<-paste0("Mean_",colnames(unit_conversions_average))
names(unit_conversions_average)[names(unit_conversions_average) == "Mean_Settlement"] <- "settlement"



analysis_df_list<-list(unit_conversions_average,unit_conversions_medians,unit_conversions_max,unit_conversions_min)

settlement_unit_conversions <-purrr::reduce(analysis_df_list, left_join)




#settlement date aggregations----
unit_conversions_medians <- unit_conversions %>%  
  group_by(Settlement,Date) %>% 
  summarise_all(funs(median(., na.rm = TRUE)))

colnames(unit_conversions_medians)<-paste0("Median_",colnames(unit_conversions_medians))
names(unit_conversions_medians)[names(unit_conversions_medians) == "Median_Settlement"] <- "settlement"
names(unit_conversions_medians)[names(unit_conversions_medians) == "Median_Date"] <- "Date"

unit_conversions_medians_sd <- unit_conversions_medians %>% 
  group_by(settlement) %>% 
  summarise_all(funs(sd(., na.rm = TRUE)))

colnames(unit_conversions_medians_sd)<-paste0("Median_",colnames(unit_conversions_medians_sd))
names(unit_conversions_medians_sd)[names(unit_conversions_medians_sd) == "Median_Settlement"] <- "settlement"
names(unit_conversions_medians_sd)[names(unit_conversions_medians_sd) == "Median_Date"] <- "Date"



write.csv(unit_conversions_medians_sd,"outputs/measurement_unit Monthly medians standard_deviation.csv")




unit_conversions_max <- unit_conversions %>%  
  group_by(Settlement,Date) %>% 
  summarise_all(funs(max(., na.rm = TRUE)))



colnames(unit_conversions_max)<-paste0("Max_",colnames(unit_conversions_max))
names(unit_conversions_max)[names(unit_conversions_max) == "Max_Settlement"] <- "settlement"
names(unit_conversions_max)[names(unit_conversions_max) == "Max_Date"] <- "Date"




unit_conversions_min <- unit_conversions %>%  
  group_by(Settlement,Date) %>% 
  summarise_all(funs(min(., na.rm = TRUE)))

colnames(unit_conversions_min)<-paste0("Min_",colnames(unit_conversions_min))
names(unit_conversions_min)[names(unit_conversions_min) == "Min_Settlement"] <- "settlement"
names(unit_conversions_min)[names(unit_conversions_min) == "Min_Date"] <- "Date"




unit_conversions_average <- unit_conversions %>%  
  group_by(Settlement,Date) %>% 
  summarise_all(funs(mean(., na.rm = TRUE)))


colnames(unit_conversions_average)<-paste0("Mean_",colnames(unit_conversions_average))
names(unit_conversions_average)[names(unit_conversions_average) == "Mean_Settlement"] <- "settlement"
names(unit_conversions_average)[names(unit_conversions_average) == "Mean_Date"] <- "Date"


#settlement level unit standard deviation

unit_conversions_sd <- unit_conversions %>%  
  group_by(Settlement,Date) %>% 
  summarise_all(funs(sd(., na.rm = TRUE)))


colnames(unit_conversions_sd)<-paste0("SD_",colnames(unit_conversions_sd))
names(unit_conversions_sd)[names(unit_conversions_sd) == "SD_Settlement"] <- "settlement"
names(unit_conversions_sd)[names(unit_conversions_sd) == "SD_Date"] <- "Date"




analysis_df_list<-list(unit_conversions_sd, unit_conversions_average,unit_conversions_medians,unit_conversions_max,unit_conversions_min)

settlement_date_conversions <-purrr::reduce(analysis_df_list, left_join)





write.csv(settlement_date_conversions,"outputs/Unit_conversions_longitudinal_data.csv")








#National conversions----------

unit_conversions$Country <- "Uganda"

unit_conversions_medians <- unit_conversions %>%  
  select(-Date,-Settlement) %>% 
  group_by(Country) %>% 
  summarise_all(funs(median(., na.rm = TRUE)))

colnames(unit_conversions_medians)<-paste0("Median_",colnames(unit_conversions_medians))
names(unit_conversions_medians)[names(unit_conversions_medians) == "Median_Country"] <- "Country"



unit_conversions_max <- unit_conversions %>%  
  select(-Date,-Settlement) %>% 
  group_by(Country) %>% 
  summarise_all(funs(max(., na.rm = TRUE)))



colnames(unit_conversions_max)<-paste0("Max_",colnames(unit_conversions_max))
names(unit_conversions_max)[names(unit_conversions_max) == "Max_Country"] <- "Country"




unit_conversions_min <- unit_conversions %>%  
  select(-Date, -Settlement) %>% 
  group_by(Country) %>% 
  summarise_all(funs(min(., na.rm = TRUE)))

colnames(unit_conversions_min)<-paste0("Min_",colnames(unit_conversions_min))
names(unit_conversions_min)[names(unit_conversions_min) == "Min_Country"] <- "Country"




unit_conversions_average <- unit_conversions %>%  
  select(-Date, -Settlement) %>% 
  group_by(Country) %>% 
  summarise_all(funs(mean(., na.rm = TRUE)))


colnames(unit_conversions_average)<-paste0("Mean_",colnames(unit_conversions_average))
names(unit_conversions_average)[names(unit_conversions_average) == "Mean_Country"] <- "Country"



analysis_df_list<-list(unit_conversions_average,unit_conversions_medians,unit_conversions_max,unit_conversions_min)

national_unit_conversions <-purrr::reduce(analysis_df_list, left_join)




list_of_datasets <- list("Set unit convrsions" = settlement_unit_conversions,"National conversions" = national_unit_conversions)

write.xlsx(list_of_datasets, file = "outputs/UG_COnvid_jmmi_unit_conversions.xlsx")


write.csv(unit_conversions,"outputs/unit_conversions.csv")





