
national_jmmi$Regions <-"Nationwide"

stock_running_out <- bind_rows(region_jmmi,national_jmmi)  

stock_running_out <- stock_running_out %>% select(Regions,period, starts_with("stock_runout_item.")) %>% filter(period == "Bi 2 - April") %>% 
  select(Regions,contains("..yes"))  

stock_running_out_col <- region_jmmi %>% select(Regions,period, starts_with("stock_runout_item.")) %>% 
  select(contains("..yes")) %>% colnames()


stock_running_out  <- stock_running_out %>% pivot_longer(stock_running_out_col,
                                                         names_to="Runout_items",
                                                         values_to="proportion_reported")

stock_running_out <- stock_running_out %>% 
  mutate(Runout_items = str_remove_all(Runout_items,"stock_runout_item.")) %>% 
  mutate(Runout_items = str_replace(Runout_items, "Agricultural.inputs..e.g..Fertilizers..Pesticides..hoes.and.pangas..yes", "Agricultural inputs"),
         Runout_items = str_replace(Runout_items, "Cereals..e.g..maize.grain..maize.flour..millet.flour..yes", "Cereals"),
         Runout_items = str_replace(Runout_items, "Education.items..e.g..Exercise.books..Pencils.and.pens..yes", "Education items"),
         Runout_items = str_replace(Runout_items, "Household.Items..e.g..Utensils..Clothing..Beddings..underwear..torch..yes", "Household Items"),
         Runout_items = str_replace(Runout_items, "Shelter.items..e.g..roofing.nails..nylon.rope...bricks..grass.thatch..eucalyptus.poles..yes", "Shelter items"),
         Runout_items = str_replace(Runout_items, "WASH.items..e.g..Laundry.soap..Sanitary.pads..jerrycans..basin..yes", "WASH items"),
         Runout_items = str_replace(Runout_items, "Energy.items..e.g..Firewood..Charcoal..yes", "Energy items"))


stock_running_out <- stock_running_out %>%
  group_by(Regions) %>% 
  mutate(lowest_stock_item = rank(-proportion_reported,na.last = TRUE,ties.method = "random")) 

lowest_stock_item1 <- stock_running_out %>% filter(lowest_stock_item == 1 ) %>% 
  rename( "level"= Regions,"lowest_stock_item1" = Runout_items, "lowest_stock_item_prop1" = proportion_reported) %>% 
  select(level:lowest_stock_item_prop1)

lowest_stock_item2 <- stock_running_out %>% filter(lowest_stock_item == 2 ) %>% 
  rename( "level"= Regions,"lowest_stock_item2" = Runout_items, "lowest_stock_item_prop2" = proportion_reported) %>% 
  select(level:lowest_stock_item_prop2)

lowest_stock_item3 <- stock_running_out %>% filter(lowest_stock_item ==3 ) %>% 
  rename( "level"= Regions,"lowest_stock_item3" = Runout_items, "lowest_stock_item_prop3" = proportion_reported) %>% 
  select(level:lowest_stock_item_prop3)

analysis_df_list<-list(lowest_stock_item1,lowest_stock_item2,lowest_stock_item3)

data_merge_runout_items <-purrr::reduce(analysis_df_list, left_join)








