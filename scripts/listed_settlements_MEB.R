#MEB calculations----

#Monthly FMEB calculations

item_prices$monthly_HH_cereals <- item_prices$price_maize_g * 43.5
item_prices$monthly_HH_beans <- item_prices$price_beans * 27
item_prices$monthly_HH_sorghum <- item_prices$price_sorghum * 7.5
item_prices$monthly_HH_oil <- item_prices$price_oil * 3.75
item_prices$monthly_HH_cassava <- item_prices$price_cassava * 3
item_prices$monthly_HH_salt <- item_prices$price_salt * 0.75
item_prices$monthly_HH_leaves <- item_prices$price_dodo * 15
item_prices$monthly_HH_fish <- item_prices$price_fish * 3
item_prices$monthly_HH_milk <- item_prices$price_milk * 1.5

FMEB <- item_prices %>% select(monthly_HH_cereals:monthly_HH_milk)
item_prices$FMEB  <- rowSums(FMEB)

#settlment level MMEB


settlment_MEB <- item_prices %>%  select(-uuid,-market_final) %>% 
  group_by(Regions,DISTRICT,settlement,period) %>% 
  summarise_all(funs(median(., na.rm = TRUE)))





region_MEB <- item_prices %>%  select(-uuid,-market_final,-DISTRICT,-settlement) %>% 
  filter(period == "Bi 2 - April") %>% 
  group_by(Regions) %>% 
  summarise_all(funs(median(., na.rm = TRUE))) %>% 
  select(Regions,FMEB) %>% rename("level" = Regions, "food_basket_cost"= FMEB)


national_MEB <- item_prices %>%  
  filter(period == "Bi 2 - April") %>% 
  select(FMEB) %>% 
  summarise_all(funs(median(., na.rm = TRUE))) %>% mutate(level = "Nationwide") %>% 
  rename( "food_basket_cost"= FMEB)


MEB_cost <-bind_rows(region_MEB,national_MEB)





#Factsheets-----

#Settlement MEB ranking

top_settlment_MEB <- settlment_MEB %>% filter(period == "Bi 2 - April") %>% 
  group_by(Regions) %>% 
  mutate(setlement_MEB_rank = rank(-FMEB,na.last = TRUE,ties.method = "random"))%>% 
  arrange(Regions,setlement_MEB_rank) %>%  filter(setlement_MEB_rank == 1) %>%   select(Regions,settlement) %>% 
  rename("level" = Regions, "most_expensive_settlement1"= settlement)

top_settlment_MEB2 <- settlment_MEB %>% filter(period == "Bi 2 - April") %>% 
  group_by(Regions) %>% 
  mutate(setlement_MEB_rank = rank(-FMEB,na.last = TRUE,ties.method = "random"))%>% 
  arrange(Regions,setlement_MEB_rank) %>%  filter(setlement_MEB_rank == 2) %>%   select(Regions,settlement) %>% 
  rename("level" = Regions, "most_expensive_settlement2"=settlement)


top_settlment_MEB3 <- settlment_MEB %>% filter(period == "Bi 2 - April") %>% 
  group_by(Regions) %>% 
  mutate(setlement_MEB_rank = rank(-FMEB,na.last = TRUE,ties.method = "random"))%>% 
  arrange(Regions,setlement_MEB_rank) %>%  filter(setlement_MEB_rank == 3) %>%   select(Regions,settlement) %>% 
  rename("level" = Regions, "most_expensive_settlement3"=settlement)



low_settlment_MEB <- settlment_MEB %>% filter(period == "Bi 2 - April") %>% 
  group_by(Regions) %>% 
  mutate(setlement_MEB_rank = rank(FMEB,na.last = TRUE,ties.method = "random"))%>% 
  arrange(Regions,setlement_MEB_rank) %>%  filter(setlement_MEB_rank == 1) %>%   select(Regions,settlement) %>% 
  rename("level" = Regions, "least_expensive_settlement1"= settlement)

low_settlment_MEB2 <- settlment_MEB %>% filter(period == "Bi 2 - April") %>% 
  group_by(Regions) %>% 
  mutate(setlement_MEB_rank = rank(FMEB,na.last = TRUE,ties.method = "random"))%>% 
  arrange(Regions,setlement_MEB_rank) %>%  filter(setlement_MEB_rank == 2) %>%   select(Regions,settlement) %>% 
  rename("level" = Regions, "least_expensive_settlement2"=settlement)


low_settlment_MEB3 <- settlment_MEB %>% filter(period == "Bi 2 - April") %>% 
  group_by(Regions) %>% 
  mutate(setlement_MEB_rank = rank(FMEB,na.last = TRUE,ties.method = "random"))%>% 
  arrange(Regions,setlement_MEB_rank) %>%  filter(setlement_MEB_rank == 3) %>%   select(Regions,settlement) %>% 
  rename("level" = Regions, "least_expensive_settlement3"=settlement)





analysis_df_list<-list(top_settlment_MEB,top_settlment_MEB2,top_settlment_MEB3,low_settlment_MEB,low_settlment_MEB2,low_settlment_MEB3)

settlements_MEB_ranking <-purrr::reduce(analysis_df_list, left_join)

#Factsheets-national level----

n_top_settlment_MEB <- settlment_MEB[,c(3,33)] %>% 
  mutate(setlement_MEB_rank = rank(-FMEB,na.last = TRUE,ties.method = "random"),level = "Nationwide") %>% 
  filter(setlement_MEB_rank == 1 ) %>% rename( "most_expensive_settlement1"=settlement) %>% select(level, most_expensive_settlement1)

n_top_settlment_MEB2 <- settlment_MEB[,c(3,33)] %>% 
  mutate(setlement_MEB_rank = rank(-FMEB,na.last = TRUE,ties.method = "random"),level = "Nationwide") %>% 
  filter(setlement_MEB_rank == 2 ) %>% rename( "most_expensive_settlement2"=settlement) %>% select(level, most_expensive_settlement2)

n_top_settlment_MEB3 <- settlment_MEB[,c(3,33)] %>% 
  mutate(setlement_MEB_rank = rank(-FMEB,na.last = TRUE,ties.method = "random"),level = "Nationwide") %>% 
  filter(setlement_MEB_rank == 3 ) %>% rename( "most_expensive_settlement3"=settlement) %>%  select(level, most_expensive_settlement3)





n_low_settlment_MEB <- settlment_MEB[,c(3,33)] %>% 
  mutate(setlement_MEB_rank = rank(FMEB,na.last = TRUE,ties.method = "random"),level = "Nationwide") %>% 
  filter(setlement_MEB_rank == 1 ) %>% rename( "least_expensive_settlement1"=settlement) %>% select(level, least_expensive_settlement1)

n_low_settlment_MEB2 <- settlment_MEB[,c(3,33)] %>% 
  mutate(setlement_MEB_rank = rank(FMEB,na.last = TRUE,ties.method = "random"),level = "Nationwide") %>% 
  filter(setlement_MEB_rank == 2 ) %>% rename( "least_expensive_settlement2"=settlement) %>% select(level, least_expensive_settlement2)

n_low_settlment_MEB3 <- settlment_MEB[,c(3,33)] %>% 
  mutate(setlement_MEB_rank = rank(FMEB,na.last = TRUE,ties.method = "random"),level = "Nationwide") %>% 
  filter(setlement_MEB_rank == 3 ) %>% rename( "least_expensive_settlement3"=settlement) %>% select(level, least_expensive_settlement3)






analysis_df_list<-list(n_top_settlment_MEB,n_top_settlment_MEB2,n_top_settlment_MEB3,n_low_settlment_MEB,n_low_settlment_MEB2,n_low_settlment_MEB3)

settlements_MEB_ranking_national <-purrr::reduce(analysis_df_list, left_join)




data_merge_settlement_MEB <- bind_rows(settlements_MEB_ranking,settlements_MEB_ranking_national)


analysis_df_list<-list(MEB_cost,data_merge_settlement_MEB)

data_merge_settlement_MEB <-purrr::reduce(analysis_df_list, left_join)


