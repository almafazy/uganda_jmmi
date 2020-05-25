library(dplyr)
library(butteR)
library(openxlsx)
library(tidyverse)
library(srvyr)
library(stringr)
library(purrr)

#Load data-------
#READ in the JMMI data

df_may <- data_clean
names(df_may)[1] <- "start" 
df_april <- read.csv("inputs/Markets_and_CoViD_19_tool_2020_04 _15th_13th.csv",stringsAsFactors=FALSE, na.strings = c(""," ","NA"))

df_april$decrease__pct <- rep(NA, nrow(data_clean))
df_april$decrease__pct[data_clean$customers_change == "Decreased"] <- data_clean$customers_perc_change[data_clean$customers_change == "Decreased"]
df_april$increase_pct <- rep(NA, nrow(data_clean))
df_april$increase_pct[data_clean$customers_change == "Increased"] <- data_clean$customers_perc_change[data_clean$customers_change == "Increased"]


df_march <- read.csv("inputs/March raw data.csv",stringsAsFactors=FALSE, na.strings = c(""," ","NA"))


df_march <- df_march %>% rename( "settlement" = Settlement, "market" = Market, "price_maize_g" = maize_grain_price,
                                 "price_maize_f" = Maize_flour_price,"price_millet_f" = Millet_flour_price, "price_beans" = Beans_price, 
                                 "price_sorghum" = Sorghum_price, "price_oil" = Veg_oil_price, "price_cassava" = Cassava_price, 
                                 "price_salt" = Salt_price, "price_dodo" = Vegetable_price, "price_milk" = Milk_price, 
                                 "price_soap" = Observed_price_soap, "price_firewood" = Observed_price_firewood, "price_charcoal" = Observed_price_charcoal, 
                                 "price_fish" = Fish_price, "price_pads" = Observed_price_pads)



df <- bind_rows(df_may,df_april, df_march)


settlement_data <- read.csv("inputs/settlement_list.csv",stringsAsFactors=FALSE, na.strings = c(""," ","NA"))
district_data <- read.csv("inputs/Districts_list.csv",stringsAsFactors=FALSE, na.strings = c(""," ","NA"))

data_merge <- read.csv("outputs/Indesign/data_merge.csv",stringsAsFactors=T, na.strings = c(""," ","NA"))


district_data <- district_data %>%  mutate(DISTRICT = str_to_sentence(DName2019) )

Adjumani_coordinates <- settlement_data %>% filter(DISTRICT == "Adjumani") %>% 
                        select(DISTRICT,Longitude,Latitude) %>% 
                        group_by(DISTRICT) %>%
                        summarise_all(funs(mean(., na.rm = TRUE))) %>% 
                        mutate(NAME0 = "Adjumani")



settlement_data <- bind_rows(settlement_data,Adjumani_coordinates)


df <- df %>% mutate(settlement = str_replace(settlement, "Arua/Rhino Camp", "Rhino"),
                    settlement = str_replace(settlement, "Koboko/Lobule", "Lobule"))




df_setlement <- left_join(df,settlement_data,by=c("settlement"="NAME0")) 



df_setlement <- left_join(df_setlement,district_data,by = "DISTRICT") #%>% distinct( X_uuid, .keep_all= TRUE)




#Some house cleaning-----
# Remove columns that we don't need and rename our uuid columns 

df <- df_setlement %>% select(settlement:DISTRICT,F15Regions,DName2019) %>%  
  rename( "uuid"= X_uuid ) %>% 
  select(-contains("X_"),-instanceID,-NAME,- OBJECTID.x ) %>% 
  mutate(sub_regions = str_to_sentence(F15Regions))


df$Regions <- "South West"
df$Regions[df$sub_regions == "Acholi" | df$sub_regions == "West nile" ] <- "West Nile"

df$Regions[df$DISTRICT == "Bunyoro" ] <- "West Nile"

#remove columns that are fully blanks
df <- Filter(function(x)!all(is.na(x) ), df)
df <- df %>%  filter(!is.na(market))

#Collection period


df$month[df$Month == "March"] <- "March"
df$month[df$month == "4"]<- "April"
df$month[df$month == "5"]<- "May"

df$half[df$day < 15 ]<- "Bi 1"
df$half[df$day > 14] <- "Bi 2"


df$period <- paste(df$half,"-",df$month)

#Instead of FALSE/TRUE we change them to yes/no 

sep <- df %>% lapply(function(x){if(is.logical(x)){return(as.character(x))
  }else{
    return(x)}}) %>% do.call(cbind,.) %>% as.data.frame(.,stringsAsFactors=F)

df1 <- sep
df1[sep==2] <- "no"
df1[sep==1] <- "yes"

df1[sep=="FALSE"] <- "no"
df1[sep=="TRUE"] <- "yes"

df1 <- df1 %>%  mutate(market_final = ifelse(market == "Other",market_other,market))


#Prices columns

#Trader_info <- df1 %>%  select(uuid,Regions,DISTRICT,settlement,market_final,"trader_name","trader_contact")

item_prices <- df1 %>%  select(uuid,period,Regions,DISTRICT,settlement,market_final,starts_with("price_"))

item_prices[item_prices == 99] <- NA

# Because WFP added  no  in the columns we now have to turn them into integers

item_prices[ , 7:25] <- apply(item_prices[ , 7:25], 2,            
                    function(x) as.numeric(as.character(x)))

#Collection_order

item_prices <- item_prices %>% mutate(collection_order = ifelse(period == "Bi 1 - May",3,
                                                                ifelse(period == "Bi 2 - April",2,1)))
#Median prices ----

national_items <- item_prices %>%  
  select(-uuid,-Regions,-DISTRICT,  -settlement, -market_final) %>% 
  group_by(period,collection_order) %>% 
  summarise_all(funs(median(., na.rm = TRUE)))

national_items_figures <- national_items[,2:21] %>%  filter(collection_order != 3)

national_items_dif <- national_items_figures %>% 
  map_df(~ abs(diff(.x))) %>%
  rename_all(funs(paste0(., "price_change")))

temp <- map_df(national_items_figures, ~ abs(diff(lag(.x)))) %>% setNames(paste0(names(.), '.abs.diff.lag'))

markets_items <- item_prices %>%  select(-uuid,-Regions,-DISTRICT) %>% 
  group_by(settlement,market_final,period) %>% 
  summarise_all(funs(median(., na.rm = TRUE)))


settlment_items <- item_prices %>%  select(-uuid,-market_final) %>% 
  group_by(Regions,DISTRICT,settlement,period) %>% 
  summarise_all(funs(median(., na.rm = TRUE)))

district_items <- item_prices %>%  select(-uuid,-settlement,-market_final) %>% 
  group_by(Regions,DISTRICT,period) %>% 
  summarise_all(funs(median(., na.rm = TRUE)))

region_items <- item_prices %>%  select(-uuid,-market_final,-DISTRICT,-settlement) %>% 
  group_by(Regions,period) %>% 
  summarise_all(funs(median(., na.rm = TRUE)))

Market_info <- df1 %>%  select(Regions,DISTRICT,settlement, period, agents_open, trader_restricted,
                               starts_with(c("payment_type","vendors","customer","decrease", "increase","safety","item_name_scarce.","item_scarcity_reason","stock_runout","Order_in_week_item","cross_border_trade","goods_restricted")),
                               new_rules,reason_restricted,challenge,
                               -payment_type,- item_scarcity_reason, - stock_runout_item,- order_in_week_item, - cross_border_trade_items,
                               -goods_restricted)

Market_info$payment_type.Cash..UGX. <- forcats::fct_expand(Market_info$payment_type.Cash..UGX.,c("yes","no"))
Market_info$payment_type.Voucher <- forcats::fct_expand(Market_info$payment_type.Voucher,c("yes","no"))
Market_info$payment_type.Other <- forcats::fct_expand(Market_info$payment_type.Other,c("yes","no"))
Market_info$goods_restricted.Fish.dried <- forcats::fct_expand(Market_info$goods_restricted.Fish.dried,c("yes","no"))
Market_info$goods_restricted.Milk.fresh <- forcats::fct_expand(Market_info$goods_restricted.Milk.fresh,c("yes","no"))
Market_info$order_in_week_item.Education.items..e.g..Exercise.books..Pencils. <- forcats::fct_expand(Market_info$order_in_week_item.Education.items..e.g..Exercise.books..Pencils.,c("yes","no"))
Market_info$order_in_week_item.Education.items..e.g..Exercise.books..Pencils.and.pens. <- forcats::fct_expand(Market_info$order_in_week_item.Education.items..e.g..Exercise.books..Pencils.and.pens.,c("yes","no"))
Market_info$cross_border_trade_items.Energy.items..e.g..Firewood..Charcoal. <- forcats::fct_expand(Market_info$cross_border_trade_items.Energy.items..e.g..Firewood..Charcoal.,c("yes","no"))
Market_info$order_in_week_item.Leafy.Vegetables <- forcats::fct_expand(Market_info$order_in_week_item.Leafy.Vegetables,c("yes","no"))
Market_info$order_in_week_item.cassava.fresh <- forcats::fct_expand(Market_info$order_in_week_item.cassava.fresh,c("yes","no"))
Market_info$goods_restricted.cassava.fresh <- forcats::fct_expand(Market_info$goods_restricted.cassava.fresh,c("yes","no"))
Market_info$goods_restricted.Leafy.Vegetables <- forcats::fct_expand(Market_info$goods_restricted.Leafy.Vegetables,c("yes","no"))
Market_info$item_name_scarce.Leafy.Vegetables <- forcats::fct_expand(Market_info$item_name_scarce.Leafy.Vegetables,c("yes","no"))

dfsvy_jmmi_markets <-srvyr::as_survey(Market_info)

jmmi_columns <- Market_info %>% select(-Regions,-DISTRICT,-settlement, -period) %>%  colnames() %>% dput()

region_jmmi_safety <- Market_info %>% select(contains("safety")) %>%  colnames() %>% dput()


#Region level aggregation-----------

region_jmmi <-butteR::mean_proportion_table(design = dfsvy_jmmi_markets,
                                           list_of_variables = jmmi_columns,
                                           aggregation_level = c("Regions","period"),
                                           round_to = 2,
                                           return_confidence = FALSE,
                                           na_replace = FALSE)

region_jmmi_safety <-butteR::mean_proportion_table(design = dfsvy_jmmi_markets,
                                            list_of_variables = jmmi_columns,
                                            aggregation_level = c("Regions","period"),
                                            round_to = 2,
                                            return_confidence = FALSE,
                                            na_replace = FALSE)


national_jmmi <-butteR::mean_proportion_table(design = dfsvy_jmmi_markets,
                                            list_of_variables = jmmi_columns,
                                            aggregation_level = "period",
                                            round_to = 2,
                                            return_confidence = FALSE,
                                            na_replace = FALSE)




settlement_jmmi <-butteR::mean_proportion_table(design = dfsvy_jmmi_markets,
                                              list_of_variables = jmmi_columns,
                                              aggregation_level = c("Regions","DISTRICT","settlement","period"),
                                              round_to = 2,
                                              return_confidence = FALSE,
                                              na_replace = FALSE)





#district_jmmi <- district_jmmi %>% filter(!is.na(payment_type.Cash..UGX..yes))

settlement_jmmi <- settlement_jmmi %>% filter(!is.na(payment_type.Cash..UGX..yes))

write.csv(settlement_jmmi,"outputs/settlement_summary.csv",na = "")

#region count-----

markets_per_region <- item_prices %>%  select(Regions,period, market_final) %>% 
  group_by(Regions,period) %>% 
 summarise(num_market_assessed = n_distinct(market_final),
           num_assessed = length(period)) %>% 
  rename("level"=Regions) %>% filter(period == "Bi 1 - May") %>% 
  select(level,num_market_assessed,num_assessed)

settlements_per_region <- item_prices %>%  select(Regions,settlement,period) %>% 
  group_by(Regions,period) %>% 
  summarise(markets_numer = n_distinct(settlement))


#nationwide
markets_nationwide <- item_prices %>%  select(Regions,period, market_final) %>% 
  group_by(period) %>% 
  summarise(num_market_assessed = n_distinct(market_final),
            num_assessed = length(period),
            level = "Nationwide") %>% 
  filter(period == "Bi 1 - May") %>% 
  select(level,num_market_assessed,num_assessed)


data_merge_summary <- bind_rows(markets_nationwide,markets_per_region)


#settlement MMEB ranking
source("scripts/unit_measurement.R")

source("scripts/listed_settlements_MEB.R")

source("scripts/listed_items.R")

source("scripts/page3.R")




#Data exports-------

list_of_datasets <- list("National level median" = national_items,"Market median price" = markets_items, 
                         "Settlement median price" = settlment_items,"Region median" = region_items, 
                         "Disrict Median" = district_items,"Region market info" = region_jmmi, 
                         "Naional market info" = national_jmmi, "Settlement markets info" = settlement_jmmi, 
                         "Trader level prices" = df1)

write.xlsx(list_of_datasets, file = "outputs/UG_Covid_jmmi_13may2020_period.xlsx")



analysis_df_list<-list(data_merge_summary,
       #                data_merge_settlement_MEB,
                       data_merge_runout_items,
                       data_merge_market_functionality,
                       data_merge_top5s)

data_merge_final <-purrr::reduce(analysis_df_list, left_join)



write.csv(data_merge_final,"outputs/data_merge_jmmi.csv",na = "")


# 
# write.csv(settlment_MEB,"outputs/settlements_MEB.csv",na = "")
# 
# 
