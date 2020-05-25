
all_jmmi <- bind_rows(region_jmmi,national_jmmi) %>% filter(period == "Bi 2 - April") 

data_merge_market_functionality <- all_jmmi %>%
  select(Regions,vendors_change.Decreased,`vendors_perc_change.By more than 50 percent`,customers_change.Decreased,
         `customers_perc_change.By more than 50 percent`,item_scarcity_reason.Supplier.unable.to.provide.enough.yes,
         item_scarcity_reason.Supplier.unable.to.provide.enough.no, payment_type.Mobile.money.yes,stock_runout.Yes,
         stock_runout.No,cross_border_trade.Decreased,`cross_border_trade.No change`,`cross_border_trade.I don't know`,
         `cross_border_trade.There is no cross-border trade`, agents_open.Yes,agents_open.No,`safety.Less secure`,`agents_open.I dont know`,
         `safety.More secure`,`safety.No difference`,trader_restricted.Yes,trader_restricted.No) %>% 
  rename("level"= Regions)


#Items scarsity----
scarce_items <- all_jmmi %>% select(Regions,starts_with("item_name_scarce.")) %>% 
  select(Regions,contains("..yes"))


scarce_items_col <- all_jmmi %>% select(starts_with("item_name_scarce.")) %>% 
  select(contains("..yes")) %>% colnames()


scarce_items  <- scarce_items %>% pivot_longer(scarce_items_col,
                                                         names_to="scarce_items",
                                                         values_to="proportion_reported")


scarce_items <- scarce_items %>% 
                                mutate(scarce_items = str_replace(scarce_items, "item_name_scarce.Agricultural.inputs..e.g..Fertilizers..Pesticides..hoes.and.pangas..yes", "Agricultural inputs"),
                                        scarce_items = str_replace(scarce_items, "item_name_scarce.Cereals..e.g..maize.grain..maize.flour..millet.flour..yes", "Cereals"),
                                        scarce_items = str_replace(scarce_items, "item_name_scarce.Education.items..e.g..Exercise.books..Pencils.and.pens..yes", "Education items"),
                                        scarce_items = str_replace(scarce_items, "item_name_scarce.Household.Items..e.g..Utensils..Clothing..Beddings..underwear..torch..yes", "Household Items"),
                                        scarce_items = str_replace(scarce_items, "item_name_scarce.Shelter.items..e.g..roofing.nails..nylon.rope...bricks..grass.thatch..eucalyptus.poles..yes", "Shelter items"),
                                        scarce_items = str_replace(scarce_items, "item_name_scarce.WASH.items..e.g..Laundry.soap..Sanitary.pads..jerrycans..basin..yes", "WASH items"),
                                        scarce_items = str_replace(scarce_items, "item_name_scarce.Energy.items..e.g..Firewood..Charcoal..yes", "Energy items"))


scarce_items <- scarce_items %>%
  group_by(Regions) %>% 
  mutate(lowest_stock_item = rank(-proportion_reported,na.last = TRUE,ties.method = "random")) 

scarce_items1 <- scarce_items %>% filter(lowest_stock_item == 1 ) %>% 
  rename( "level"= Regions,"scarce_items1" = scarce_items, "scarce_items_prop1" = proportion_reported) %>% 
  select(level:scarce_items_prop1)

scarce_items2 <- scarce_items %>% filter(lowest_stock_item == 2 ) %>% 
  rename( "level"= Regions,"scarce_items2" = scarce_items, "scarce_items_prop2" = proportion_reported) %>% 
  select(level:scarce_items_prop2)

scarce_items3 <- scarce_items %>% filter(lowest_stock_item == 3 ) %>% 
  rename( "level"= Regions,"scarce_items3" = scarce_items, "scarce_items_prop3" = proportion_reported) %>% 
  select(level:scarce_items_prop3)

scarce_items4 <- scarce_items %>% filter(lowest_stock_item == 4 ) %>% 
  rename( "level"= Regions,"scarce_items4" = scarce_items, "scarce_items_prop4" = proportion_reported) %>% 
  select(level:scarce_items_prop4)

scarce_items5 <- scarce_items %>% filter(lowest_stock_item == 5 ) %>% 
  rename( "level"= Regions,"scarce_items5" = scarce_items, "scarce_items_prop5" = proportion_reported) %>% 
  select(level:scarce_items_prop5)

#order within a week----
week_order <- all_jmmi %>% select(Regions,starts_with("order_in_week_item.")) %>% 
  select(Regions,contains("..yes"))


week_order_col <- all_jmmi %>% select(starts_with("order_in_week_item.")) %>% 
  select(contains("..yes")) %>% colnames()


week_order  <- week_order %>% pivot_longer(week_order_col,
                                               names_to="week_order",
                                               values_to="proportion_reported")


week_order <- week_order %>% 
  mutate(week_order = str_replace(week_order, "order_in_week_item.Agricultural.inputs..e.g..Fertilizers..Pesticides..hoes.and.pangas..yes", "Agricultural inputs"),
         week_order = str_replace(week_order, "order_in_week_item.Cereals..e.g..maize.grain..maize.flour..millet.flour..yes", "Cereals"),
         week_order = str_replace(week_order, "order_in_week_item.Education.items..e.g..Exercise.books..Pencils.and.pens..yes", "Education items"),
         week_order = str_replace(week_order, "order_in_week_item.Household.Items..e.g..Utensils..Clothing..Beddings..underwear..torch..yes", "Household Items"),
         week_order = str_replace(week_order, "order_in_week_item.Shelter.items..e.g..roofing.nails..nylon.rope...bricks..grass.thatch..eucalyptus.poles..yes", "Shelter items"),
         week_order = str_replace(week_order, "order_in_week_item.WASH.items..e.g..Laundry.soap..Sanitary.pads..jerrycans..basin..yes", "WASH items"),
         week_order = str_replace(week_order, "order_in_week_item.Energy.items..e.g..Firewood..Charcoal..yes", "Energy items"))


week_order <- week_order %>%
  group_by(Regions) %>% 
  mutate(lowest_stock_item = rank(-proportion_reported,na.last = TRUE,ties.method = "random")) 

week_order1 <- week_order %>% filter(lowest_stock_item == 1 ) %>% 
  rename( "level"= Regions,"week_order1" = week_order, "week_order_prop1" = proportion_reported) %>% 
  select(level:week_order_prop1)

week_order2 <- week_order %>% filter(lowest_stock_item == 2 ) %>% 
  rename( "level"= Regions,"week_order2" = week_order, "week_order_prop2" = proportion_reported) %>% 
  select(level:week_order_prop2)

week_order3 <- week_order %>% filter(lowest_stock_item == 3 ) %>% 
  rename( "level"= Regions,"week_order3" = week_order, "week_order_prop3" = proportion_reported) %>% 
  select(level:week_order_prop3)

week_order4 <- week_order %>% filter(lowest_stock_item == 4 ) %>% 
  rename( "level"= Regions,"week_order4" = week_order, "week_order_prop4" = proportion_reported) %>% 
  select(level:week_order_prop4)

week_order5 <- week_order %>% filter(lowest_stock_item == 5 ) %>% 
  rename( "level"= Regions,"week_order5" = week_order, "week_order_prop5" = proportion_reported) %>% 
  select(level:week_order_prop5)




#item_scasity reason----
scarcity_reason <- all_jmmi %>% select(Regions,starts_with("item_scarcity_reason.")) %>% 
  select(Regions,contains(".yes"))


scarcity_reason_col <- all_jmmi %>% select(starts_with("item_scarcity_reason.")) %>% 
  select(contains(".yes")) %>% colnames()


scarcity_reason  <- scarcity_reason %>% pivot_longer(scarcity_reason_col,
                                               names_to="scarcity_reason",
                                               values_to="proportion_reported")


scarcity_reason <- scarcity_reason %>% 
  mutate(scarcity_reason = str_replace(scarcity_reason, "item_scarcity_reason.Not.enough.money.to.source.items.yes", "Not enough money to source items"),
         scarcity_reason = str_replace(scarcity_reason, "item_scarcity_reason.Producers.not.producing.enough.yes", "Producers not producing enough yes"),
         scarcity_reason = str_replace(scarcity_reason, "item_scarcity_reason.Supplier.unable.to.provide.enough.yes", "Supplier unable to provide enough"),
         scarcity_reason = str_replace(scarcity_reason, "item_scarcity_reason.Difficulty.transporting.items.to.marketplace.yes", "Difficulty transporting items to marketplace yes"),
         scarcity_reason = str_replace(scarcity_reason, "item_scarcity_reason.Can.no.longer.obtain.items.due.to.restrictions.yes", "Can no longer obtain items due to restrictions yes"),
         scarcity_reason = str_replace(scarcity_reason, "item_scarcity_reason.Others..specify..yes", "Other reasons"))
         

scarcity_reason <- scarcity_reason %>%
  group_by(Regions) %>% 
  mutate(lowest_stock_item = rank(-proportion_reported,na.last = TRUE,ties.method = "random")) 

scarcity_reason1 <- scarcity_reason %>% filter(lowest_stock_item == 1 ) %>% 
  rename( "level"= Regions,"scarcity_reason1" = scarcity_reason, "scarcity_reason_prop1" = proportion_reported) %>% 
  select(level:scarcity_reason_prop1)

scarcity_reason2 <- scarcity_reason %>% filter(lowest_stock_item == 2 ) %>% 
  rename( "level"= Regions,"scarcity_reason2" = scarcity_reason, "scarcity_reason_prop2" = proportion_reported) %>% 
  select(level:scarcity_reason_prop2)

scarcity_reason3 <- scarcity_reason %>% filter(lowest_stock_item == 3 ) %>% 
  rename( "level"= Regions,"scarcity_reason3" = scarcity_reason, "scarcity_reason_prop3" = proportion_reported) %>% 
  select(level:scarcity_reason_prop3)

scarcity_reason4 <- scarcity_reason %>% filter(lowest_stock_item == 4 ) %>% 
  rename( "level"= Regions,"scarcity_reason4" = scarcity_reason, "scarcity_reason_prop4" = proportion_reported) %>% 
  select(level:scarcity_reason_prop4)

scarcity_reason5 <- scarcity_reason %>% filter(lowest_stock_item == 5 ) %>% 
  rename( "level"= Regions,"scarcity_reason5" = scarcity_reason, "scarcity_reason_prop5" = proportion_reported) %>% 
  select(level:scarcity_reason_prop5)


##Goods_restricted-----


restricted_goods <- all_jmmi %>% select(Regions,starts_with("goods_restricted.")) %>% 
  select(Regions,contains("..yes"))


restricted_goods_col <- all_jmmi %>% select(starts_with("goods_restricted.")) %>% 
  select(contains("..yes")) %>% colnames()


restricted_goods  <- restricted_goods %>% pivot_longer(restricted_goods_col,
                                                     names_to="restricted_goods",
                                                     values_to="proportion_reported")


restricted_goods <- restricted_goods %>% 
  mutate(restricted_goods = str_replace(restricted_goods, "goods_restricted.Agricultural.inputs..e.g..Fertilizers..Pesticides..hoes.and.pangas..yes", "Agricultural inputs"),
         restricted_goods = str_replace(restricted_goods, "goods_restricted.Cereals..e.g..maize.grain..maize.flour..millet.flour..yes", "Cereals"),
         restricted_goods = str_replace(restricted_goods, "goods_restricted.Education.items..e.g..Exercise.books..Pencils.and.pens..yes", "Education items"),
         restricted_goods = str_replace(restricted_goods, "goods_restricted.Household.Items..e.g..Utensils..Clothing..Beddings..underwear..torch..yes", "Household Items"),
         restricted_goods = str_replace(restricted_goods, "goods_restricted.Shelter.items..e.g..roofing.nails..nylon.rope...bricks..grass.thatch..eucalyptus.poles..yes", "Shelter items"),
         restricted_goods = str_replace(restricted_goods, "goods_restricted.WASH.items..e.g..Laundry.soap..Sanitary.pads..jerrycans..basin..yes", "WASH items"),
         restricted_goods = str_replace(restricted_goods, "goods_restricted.Energy.items..e.g..Firewood..Charcoal..yes", "Energy items"))


restricted_goods <- restricted_goods %>%
  group_by(Regions) %>% 
  mutate(lowest_stock_item = rank(-proportion_reported,na.last = TRUE,ties.method = "random")) 

restricted_goods1 <- restricted_goods %>% filter(lowest_stock_item == 1 ) %>% 
  rename( "level"= Regions,"restricted_goods1" = restricted_goods, "restricted_goods_prop1" = proportion_reported) %>% 
  select(level:restricted_goods_prop1)

restricted_goods2 <- restricted_goods %>% filter(lowest_stock_item == 2 ) %>% 
  rename( "level"= Regions,"restricted_goods2" = restricted_goods, "restricted_goods_prop2" = proportion_reported) %>% 
  select(level:restricted_goods_prop2)

restricted_goods3 <- restricted_goods %>% filter(lowest_stock_item == 3 ) %>% 
  rename( "level"= Regions,"restricted_goods3" = restricted_goods, "restricted_goods_prop3" = proportion_reported) %>% 
  select(level:restricted_goods_prop3)

restricted_goods4 <- restricted_goods %>% filter(lowest_stock_item == 4 ) %>% 
  rename( "level"= Regions,"restricted_goods4" = restricted_goods, "restricted_goods_prop4" = proportion_reported) %>% 
  select(level:restricted_goods_prop4)

restricted_goods5 <- restricted_goods %>% filter(lowest_stock_item == 5 ) %>% 
  rename( "level"= Regions,"restricted_goods5" = restricted_goods, "restricted_goods_prop5" = proportion_reported) %>% 
  select(level:restricted_goods_prop5)


#Safety reasons--------


safety_reasons_less_secure <- all_jmmi %>% select(Regions,period,starts_with("safety_reason_recoding_lesssecure."))


safety_reasons_less_secure_col <- all_jmmi %>% select(starts_with("safety_reason_recoding_lesssecure.")) %>% colnames()


safety_reasons_less_secure  <- safety_reasons_less_secure %>% pivot_longer(safety_reasons_less_secure_col,
                                                       names_to="less_safe_reasons",
                                                       values_to="proportion_reported")



safety_reasons_less_secure <- safety_reasons_less_secure %>%  
  mutate(Regions = ifelse(is.na(Regions),"nationwide",Regions)) %>% mutate(Regions = str_replace(Regions, "1", "South West"),
                                                                           Regions = str_replace(Regions, "2", "West Nile")) %>%  
  mutate(less_safe_reasons = str_remove_all(less_safe_reasons,"safety_reason_recoding_lesssecure.")) %>%  group_by(Regions,period) %>% 
   arrange(Regions,period,proportion_reported) %>% 
  mutate(less_secure_rank = rank(-proportion_reported,na.last = TRUE,ties.method = "random")) %>% arrange(Regions,period,less_secure_rank)




#safty reason more secure -------


safety_reasons_more_secure <- all_jmmi %>% select(Regions,period,starts_with("safety_reason_recoding_moresecure."))


safety_reasons_more_secure_col <- all_jmmi %>% select(starts_with("safety_reason_recoding_moresecure.")) %>% colnames()


safety_reasons_more_secure  <- safety_reasons_more_secure %>% pivot_longer(safety_reasons_more_secure_col,
                                                                           names_to="more_safe_reasons",
                                                                           values_to="proportion_reported")



safety_reasons_more_secure <- safety_reasons_more_secure %>%  
  mutate(Regions = ifelse(is.na(Regions),"nationwide",Regions)) %>% mutate(Regions = str_replace(Regions, "1", "South West"),
                                                                           Regions = str_replace(Regions, "2", "West Nile")) %>%  
  mutate(more_safe_reasons = str_remove_all(more_safe_reasons,"safety_reason_recoding_moresecure.")) %>%  group_by(Regions,period) %>% 
  arrange(Regions,period,proportion_reported) %>% 
  mutate(more_secure_rank = rank(-proportion_reported,na.last = TRUE,ties.method = "random")) %>% arrange(Regions,period,more_secure_rank)






list_of_datasets <- list("More safe reasons" = safety_reasons_more_secure ,"Less safe reasons" = safety_reasons_less_secure )

write.xlsx(list_of_datasets, file = "outputs/UG_COnvid_jmmi_15may2020_safety reasons.xlsx")













safety_reasons <- safety_reasons %>%
  group_by(Regions) %>% 
  mutate(lowest_stock_item = rank(-proportion_reported,na.last = TRUE,ties.method = "random")) 

safety_reasons1 <- safety_reasons %>% filter(lowest_stock_item == 1 ) %>% 
  rename( "level"= Regions,"safety_reasons1" = safety_reasons, "safety_reasons_prop1" = proportion_reported) %>% 
  select(level:safety_reasons_prop1)

safety_reasons2 <- safety_reasons %>% filter(lowest_stock_item == 2 ) %>% 
  rename( "level"= Regions,"safety_reasons2" = safety_reasons, "safety_reasons_prop2" = proportion_reported) %>% 
  select(level:safety_reasons_prop2)

safety_reasons3 <- safety_reasons %>% filter(lowest_stock_item == 3 ) %>% 
  rename( "level"= Regions,"safety_reasons3" = safety_reasons, "safety_reasons_prop3" = proportion_reported) %>% 
  select(level:safety_reasons_prop3)

safety_reasons4 <- safety_reasons %>% filter(lowest_stock_item == 4 ) %>% 
  rename( "level"= Regions,"safety_reasons4" = safety_reasons, "safety_reasons_prop4" = proportion_reported) %>% 
  select(level:safety_reasons_prop4)

safety_reasons5 <- safety_reasons %>% filter(lowest_stock_item == 5 ) %>% 
  rename( "level"= Regions,"safety_reasons5" = safety_reasons, "safety_reasons_prop5" = proportion_reported) %>% 
  select(level:safety_reasons_prop5)


analysis_df_list<-list(scarce_items1,scarce_items2,scarce_items3,scarce_items4,scarce_items5,
                       week_order1,week_order2,week_order3,week_order4,week_order5,scarcity_reason1,
                       scarcity_reason2,scarcity_reason3,scarcity_reason4,scarcity_reason5,
                       restricted_goods1,restricted_goods2,restricted_goods3,restricted_goods4,restricted_goods5,
                       safety_reasons1,safety_reasons2,safety_reasons3,safety_reasons4,safety_reasons5)

data_merge_top5s <-purrr::reduce(analysis_df_list, left_join)

write.csv(data_merge_top5s,"outputs/top5_reasons_recoded.csv")
