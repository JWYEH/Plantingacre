library(data.table)
library(dplyr)
library(getUSDA)
devtools::load_all("../BPI/")

plantedCorn <- get_USDA(commodity_desc = "CORN",
                        statisticcat_desc = "AREA PLANTED",
                        short_desc = "CORN - ACRES PLANTED",
                        agg_level_desc = "STATE",
                        year = 1950:2019,
                        reference_period_desc = "YEAR") %>%
  clean_usda(value_rename_to = "acre_corn") %>%
  glimpse()


plantedSoy <- get_USDA(commodity_desc = "SOYBEANS",
                       statisticcat_desc = "AREA PLANTED",
                       short_desc = "SOYBEANS - ACRES PLANTED",
                       agg_level_desc = "COUNTY",
                       year = 2019:2019,
                       reference_period_desc = "YEAR")%>%
  clean_usda(value_rename_to = "acre_soy") %>% 
  glimpse()

yieldCorn <- get_USDA(commodity_desc = "CORN",
                      statisticcat_desc = "YIELD",
                      agg_level_desc = "STATE",
                      year = 1950:2019,
                      util_practice_desc = "GRAIN",
                      reference_period_desc = "YEAR") %>%
  clean_usda(value_rename_to = "yield_corn") %>% 
  glimpse()

yieldSoy <- get_USDA(commodity_desc = "SOYBEANS",
                     statisticcat_desc = "YIELD",
                     agg_level_desc = "STATE",
                     year = 1950:2019,
                     reference_period_desc = "YEAR") %>%
  clean_usda(value_rename_to = "yield_soy") %>% 
  glimpse()

region <- fread("us_farm_prod_region.csv")

main <- merge(plantedCorn, plantedSoy,all.x = T) %>%
  merge(yieldCorn, all.x = T) %>%
  merge(yieldSoy, all.x = T) %>%
  merge(region, all.x = T) %>%
  as.data.table() %>%
  glimpse()

main[, acre_corn := get_impute0(acre_corn)]
main[, acre_soy := get_impute0(acre_soy)]

write.csv(main, "pooled_usda_states.csv", row.names = F)

