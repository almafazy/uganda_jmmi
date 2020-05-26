#MEB calculations----

item_prices_fill <- item_prices %>% 
  group_by(settlement) %>% 
  arrange(settlement,collection_order) %>% 
  fill(price_maize_g,price_maize_f,price_millet_f,price_beans,price_salt,price_dodo,price_sorghum,price_cassava,price_milk,price_soap,price_fish) %>% 
  fill(price_maize_g,price_maize_f,price_millet_f,price_beans,price_salt,price_dodo,price_sorghum,price_cassava,price_milk,price_soap,price_fish, .direction = "up")


#Unit conversions


settlement_unit_conversions <- settlement_unit_conversions %>%   mutate(settlement = str_replace(settlement, "Arua/Rhino Camp", "Rhino"),
                                                                        settlement = str_replace(settlement, "Koboko/Lobule", "Lobule") ) 


analysis_df_list<-list(item_prices_fill,settlement_unit_conversions)

item_prices_fill_units <-purrr::reduce(analysis_df_list, left_join)


item_prices_fill_units <- item_prices_fill_units %>% select(everything(),-contains("Observed_price"))


# maxPrices per KG

item_prices_fill_units$price_cassava_kg_max <- item_prices_fill_units$price_cassava / item_prices_fill_units$Max_Observed_quantity_cassava
item_prices_fill_units$price_dodo_kg_max <- item_prices_fill_units$price_dodo / item_prices_fill_units$Max_Observed_quantity_dodo
item_prices_fill_units$price_fish_kg_max <- item_prices_fill_units$price_fish / item_prices_fill_units$Max_Observed_quantity_fish
item_prices_fill_units$price_firewood_kg_max <- item_prices_fill_units$price_firewood / item_prices_fill_units$Max_Observed_quantity_firewood

#min prices per kg

item_prices_fill_units$price_cassava_kg_min <- item_prices_fill_units$price_cassava / item_prices_fill_units$Min_Observed_quantity_cassava
item_prices_fill_units$price_dodo_kg_min <- item_prices_fill_units$price_dodo / item_prices_fill_units$Min_Observed_quantity_dodo
item_prices_fill_units$price_fish_kg_min <- item_prices_fill_units$price_fish / item_prices_fill_units$Min_Observed_quantity_fish
item_prices_fill_units$price_firewood_kg_min <- item_prices_fill_units$price_firewood / item_prices_fill_units$Min_Observed_quantity_firewood

#median prices per kg

item_prices_fill_units <- item_prices_fill_units %>% 
  mutate(price_cassava_kg_median = (price_cassava / Median_Observed_quantity_cassava),
         price_dodo_kg_median = (price_dodo / Median_Observed_quantity_dodo),
         price_fish_kg_median = (price_fish/Median_Observed_quantity_fish),
         price_firewood_kg_median = (price_firewood / Median_Observed_quantity_firewood))



# Max Monthly FMEB calculations

item_prices_fill_units$monthly_HH_cereals <- item_prices_fill_units$price_maize_g * 8.7 *5
item_prices_fill_units$monthly_HH_beans <- item_prices_fill_units$price_beans * 5.4 * 5
item_prices_fill_units$monthly_HH_sorghum <- item_prices_fill_units$price_sorghum * 1.5 * 5
item_prices_fill_units$monthly_HH_oil <- item_prices_fill_units$price_oil * 0.75 * 5
item_prices_fill_units$monthly_HH_salt <- item_prices_fill_units$price_salt * 0.15 * 5
item_prices_fill_units$monthly_HH_milk <- item_prices_fill_units$price_milk * 0.3 * 5

FMEB_regular <- item_prices_fill_units[,59:64]
item_prices_fill_units$FMEB_regular  <- rowSums(FMEB_regular)


item_prices_fill_units$monthly_HH_leaves_max <- item_prices_fill_units$price_dodo_kg_max * 3 * 5
item_prices_fill_units$monthly_HH_fish_max <- item_prices_fill_units$price_fish_kg_max * 0.6 * 5
item_prices_fill_units$monthly_HH_cassava_max <- item_prices_fill_units$price_cassava_kg_max * 0.6 * 5

FMEB_max <- item_prices_fill_units[,65:67]
item_prices_fill_units$FMEB_max  <- rowSums(FMEB_max)



#Min conversion calculations
item_prices_fill_units$monthly_HH_cassava_min <- item_prices_fill_units$price_cassava_kg_min * 0.6 * 5
item_prices_fill_units$monthly_HH_leaves_min <- item_prices_fill_units$price_dodo_kg_min * 3 * 5
item_prices_fill_units$monthly_HH_fish_min <- item_prices_fill_units$price_fish_kg_min * 0.6 * 5

FMEB_min <- item_prices_fill_units[,68:70]

item_prices_fill_units$FMEB_min  <- rowSums(FMEB_min)



#Median conversion calculations
item_prices_fill_units$monthly_HH_cassava_median <- item_prices_fill_units$price_cassava_kg_median * 0.6 * 5
item_prices_fill_units$monthly_HH_leaves_median <- item_prices_fill_units$price_dodo_kg_median * 3 * 5
item_prices_fill_units$monthly_HH_fish_median <- item_prices_fill_units$price_fish_kg_median * 0.6 * 5

FMEB_median <- item_prices_fill_units[,74:76]

item_prices_fill_units$FMEB_median  <- rowSums(FMEB_median)

item_prices_fill_units <- item_prices_fill_units %>% rowwise() %>% 
  mutate(FMEB_median = sum (FMEB_median, FMEB_regular, na.rm = TRUE),
         FMEB_max = sum ( FMEB_max, FMEB_regular, na.rm = TRUE),
         FMEB_min = sum ( FMEB_min, FMEB_regular, na.rm = TRUE),)
?rowwise


#settlment level MMEB


settlment_MEB_fill <- item_prices_fill_units %>%  select(-uuid,-market_final) %>% 
  group_by(Regions,DISTRICT,settlement,period) %>% 
  summarise_all(funs(median(., na.rm = TRUE)))


settlment_MEB_fill_map <- settlment_MEB_fill %>%  filter(period == "Bi 1 - May")


write.csv(settlment_MEB_fill_map,"outputs/Updated_settlment_MEB_fill_map.csv")

region_items_MEB <-item_prices_fill_units[,c(3,7:77)]

region_MEB <- region_items_MEB %>% 
  group_by(Regions,collection_order) %>% 
  summarise_all(funs(median(., na.rm = TRUE)))#%>% 
  #select(Regions,FMEB) %>% rename("level" = Regions, "food_basket_cost"= FMEB)

national_items_MEB <- item_prices_fill_units[,c(7:77)]

national_MEB <- national_items_MEB %>%  
  group_by(collection_order) %>% 
  summarise_all(funs(median(., na.rm = TRUE))) #%>% mutate(level = "Nationwide") %>% 
  #rename( "food_basket_cost"= FMEB)

###################
# Percentage Change
#####################

# Calculate percentage change between months SETTLEMENT
settlement_MEB_pct_change <- settlment_MEB_fill %>% split.data.frame(.,factor(settlment_MEB_fill$collection_order))
change_settlement_april <- mapply(function(x, y){
  if(is.numeric(x)&is.numeric(y)){
    z <- (x-y)/y*100
    return(z)}
}, settlement_MEB_pct_change[[4]], settlement_MEB_pct_change[[3]]) %>% do.call(cbind,.) %>% as.data.frame
names(change_settlement_april) %<>% gsub("price_", "",.) %>% paste0(.,"_perct", "_april") 
change_settlement_april$settlement <- settlement_MEB_pct_change[[3]]$settlement

change_settlement_march <- mapply(function(x, y){
  if(is.numeric(x)&is.numeric(y)){
    z <- (x-y)/y*100
    return(z)}
}, settlement_MEB_pct_change[[4]], settlement_MEB_pct_change[[1]]) %>% do.call(cbind,.) %>% as.data.frame
names(change_settlement_march) %<>% gsub("price_", "",.) %>% paste0(.,"_perct", "_march") 
change_settlement_march$settlement <- settlement_MEB_pct_change[[1]]$settlement

change_setllement <- merge(change_settlement_april, change_settlement_march, by = "settlement", all.y = T)

# Calculate percentage change between months REGION
region_MEB_pct_change <- region_MEB %>% split.data.frame(.,factor(region_MEB$collection_order))
change_region_april <- mapply(function(x, y){
  if(is.numeric(x)&is.numeric(y)){
    z <- (x-y)/y*100
    return(z)}
}, region_MEB_pct_change[[4]], region_MEB_pct_change[[3]]) %>% do.call(cbind,.) %>% as.data.frame
names(change_region_april) %<>% gsub("price_", "",.) %>% paste0(.,"_perct", "_april") 

change_region_march <- mapply(function(x, y){
  if(is.numeric(x)&is.numeric(y)){
    z <- (x-y)/y*100
    return(z)}
}, region_MEB_pct_change[[4]], region_MEB_pct_change[[1]]) %>% do.call(cbind,.) %>% as.data.frame
names(change_region_march) %<>% gsub("price_", "",.) %>% paste0(.,"_perct", "_march") 

percent_change_region <- cbind(change_region_april, change_region_march)
percent_change_region$Regions <- c("South West", "West Nile")


list_of_datasets <- list("Sttlement MEB" = settlment_MEB_fill,
                         "REGION MEB" = region_MEB, 
                         "National MEB" = national_MEB, 
                         "Percent_change Settlement" = change_setllement,
                         "Percent change Region" = percent_change_region)

write.xlsx(list_of_datasets, file = "outputs/UG_Covid_jmmi_May_MEBs_25052020_period.xlsx")

#MEB_cost <-bind_rows(region_MEB,national_MEB)

#march_MEB_cost <- bind_rows(march_region_MEB,march_national_MEB)

#Factsheets-----

#Settlement MEB ranking

top_settlment_MEB <- settlment_MEB_fill %>% filter(period == "Bi 1 - May") %>% 
  group_by(Regions) %>% 
  mutate(setlement_MEB_rank = rank(-FMEB,na.last = TRUE,ties.method = "random"))%>% 
  arrange(Regions,setlement_MEB_rank) %>%  filter(setlement_MEB_rank == 1) %>% select(Regions,settlement) %>% 
  rename("level" = Regions, "most_expensive_settlement1"= settlement)

top_settlment_MEB2 <- settlment_MEB_fill %>% filter(period == "Bi 1 - May") %>% 
  group_by(Regions) %>% 
  mutate(setlement_MEB_rank = rank(-FMEB,na.last = TRUE,ties.method = "random"))%>% 
  arrange(Regions,setlement_MEB_rank) %>%  filter(setlement_MEB_rank == 2) %>%  select(Regions,settlement) %>% 
  rename("level" = Regions, "most_expensive_settlement2"=settlement)


top_settlment_MEB3 <- settlment_MEB_fill %>% filter(period == "Bi 1 - May") %>% 
  group_by(Regions) %>% 
  mutate(setlement_MEB_rank = rank(-FMEB,na.last = TRUE,ties.method = "random"))%>% 
  arrange(Regions,setlement_MEB_rank) %>%  filter(setlement_MEB_rank == 3) %>%   select(Regions,settlement) %>% 
  rename("level" = Regions, "most_expensive_settlement3"=settlement)



low_settlment_MEB <- settlment_MEB_fill %>% filter(period == "Bi 1 - May") %>% 
  group_by(Regions) %>% 
  mutate(setlement_MEB_rank = rank(FMEB,na.last = TRUE,ties.method = "random"))%>% 
  arrange(Regions,setlement_MEB_rank) %>%  filter(setlement_MEB_rank == 1) %>%   select(Regions,settlement) %>% 
  rename("level" = Regions, "least_expensive_settlement1"= settlement)

low_settlment_MEB2 <- settlment_MEB_fill %>% filter(period == "Bi 1 - May") %>% 
  group_by(Regions) %>% 
  mutate(setlement_MEB_rank = rank(FMEB,na.last = TRUE,ties.method = "random"))%>% 
  arrange(Regions,setlement_MEB_rank) %>%  filter(setlement_MEB_rank == 2) %>%   select(Regions,settlement) %>% 
  rename("level" = Regions, "least_expensive_settlement2"=settlement)


low_settlment_MEB3 <- settlment_MEB_fill %>% filter(period == "Bi 1 - May") %>% 
  group_by(Regions) %>% 
  mutate(setlement_MEB_rank = rank(FMEB,na.last = TRUE,ties.method = "random"))%>% 
  arrange(Regions,setlement_MEB_rank) %>%  filter(setlement_MEB_rank == 3) %>%   select(Regions,settlement) %>% 
  rename("level" = Regions, "least_expensive_settlement3"=settlement)





#analysis_df_list<-list(top_settlment_MEB,top_settlment_MEB2,top_settlment_MEB3,low_settlment_MEB,low_settlment_MEB2,low_settlment_MEB3)

#settlements_MEB_ranking <-purrr::reduce(analysis_df_list, left_join)

#Factsheets-national level----

n_top_settlment_MEB <- settlment_MEB_fill %>% group_by(period) %>% 
  mutate(setlement_MEB_rank = rank(-FMEB,na.last = TRUE,ties.method = "random"))%>% 
  arrange(setlement_MEB_rank) %>%  select(period,settlement,setlement_MEB_rank,FMEB) 
  

lower_settlment_MEB <- settlment_MEB_fill %>% group_by(period) %>% 
  mutate(setlement_MEB_rank = rank(FMEB,na.last = TRUE,ties.method = "random"))%>% 
  arrange(setlement_MEB_rank) %>%  select(period,settlement,setlement_MEB_rank,FMEB) 


list_of_datasets <- list("expensive foodMEB" = n_top_settlment_MEB,"Least Food MEB" = lower_settlment_MEB)

write.xlsx(list_of_datasets, file = "outputs/UG_Covid_jmmi_May_first_half_MEBs_ranking_052020_period.xlsx")
  
# 
# n_top_settlment_MEB2 <- settlment_MEB[,c(3,33)] %>% 
#   mutate(setlement_MEB_rank = rank(-FMEB,na.last = TRUE,ties.method = "random"),level = "Nationwide") %>% 
#   filter(setlement_MEB_rank == 2 ) %>% rename( "most_expensive_settlement2"=settlement) %>% select(level, most_expensive_settlement2)
# 
# n_top_settlment_MEB3 <- settlment_MEB[,c(3,33)] %>% 
#   mutate(setlement_MEB_rank = rank(-FMEB,na.last = TRUE,ties.method = "random"),level = "Nationwide") %>% 
#   filter(setlement_MEB_rank == 3 ) %>% rename( "most_expensive_settlement3"=settlement) %>%  select(level, most_expensive_settlement3)
# 
# 
# 
# 
# 
# n_low_settlment_MEB <- settlment_MEB[,c(3,33)] %>% 
#   mutate(setlement_MEB_rank = rank(FMEB,na.last = TRUE,ties.method = "random"),level = "Nationwide") %>% 
#   filter(setlement_MEB_rank == 1 ) %>% rename( "least_expensive_settlement1"=settlement) %>% select(level, least_expensive_settlement1)
# 
# n_low_settlment_MEB2 <- settlment_MEB[,c(3,33)] %>% 
#   mutate(setlement_MEB_rank = rank(FMEB,na.last = TRUE,ties.method = "random"),level = "Nationwide") %>% 
#   filter(setlement_MEB_rank == 2 ) %>% rename( "least_expensive_settlement2"=settlement) %>% select(level, least_expensive_settlement2)
# 
# n_low_settlment_MEB3 <- settlment_MEB[,c(3,33)] %>% 
#   mutate(setlement_MEB_rank = rank(FMEB,na.last = TRUE,ties.method = "random"),level = "Nationwide") %>% 
#   filter(setlement_MEB_rank == 3 ) %>% rename( "least_expensive_settlement3"=settlement) %>% select(level, least_expensive_settlement3)
# 
# 
# 
# 
# 

# analysis_df_list<-list(n_top_settlment_MEB,n_top_settlment_MEB2,n_top_settlment_MEB3,n_low_settlment_MEB,n_low_settlment_MEB2,n_low_settlment_MEB3)
# 
# settlements_MEB_ranking_national <-purrr::reduce(analysis_df_list, left_join)
# 
# 
# 
# 
# data_merge_settlement_MEB <- bind_rows(settlements_MEB_ranking,settlements_MEB_ranking_national)
# 
# 
# analysis_df_list<-list(MEB_cost,data_merge_settlement_MEB)
# 
# data_merge_settlement_MEB <-purrr::reduce(analysis_df_list, left_join)
# 

