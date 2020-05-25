# Cleaning 
library(glue)
library(clog)
cleaning_log <- read.csv2("./inputs/mathias_cl_log.csv", stringsAsFactors = F)
data_raw <- read.csv2("./inputs/Market data 1st to 14th May 2020.csv", stringsAsFactors = F, encoding = "UTF-8")



?clog_clean
clog <- cleaninglog(ids = cleaning_log$ï..uuid, 
                    variables = cleaning_log$indicator, 
                    new_values = cleaning_log$new_value_1, 
                    data_id_column_name = "X_uuid", change = rep(T,366))
# names(cleaning_log) <- c("ids", "spotted_by", "name", "variables", "current_value","new_values", "new_value_2", "issue")
# attr(cleaning_log, 'data_id_column_name') <- "X_uuid"
# cleaning_log$change <- T

data_clean <- clog_clean(df = data_raw, cleaninglog = clog)

data_clean$safety_reason_recoding_lesssecure <- rep(NA, nrow(data_clean))
data_clean$safety_reason_recoding_lesssecure[data_clean$safety == "Less secure"] <- data_clean$safety_reason[data_clean$safety == "Less secure"]
data_clean$safety_reason_recoding_moresecure <- rep(NA, nrow(data_clean))
data_clean$safety_reason_recoding_moresecure[data_clean$safety == "More secure"] <- data_clean$safety_reason[data_clean$safety == "More secure"]


data_clean$decrease__pct <- rep(NA, nrow(data_clean))
data_clean$decrease__pct[data_clean$customers_change == "Decreased"] <- data_clean$customers_perc_change[data_clean$customers_change == "Decreased"]
data_clean$increase_pct <- rep(NA, nrow(data_clean))
data_clean$increase_pct[data_clean$customers_change == "Increased"] <- data_clean$customers_perc_change[data_clean$customers_change == "Increased"]

