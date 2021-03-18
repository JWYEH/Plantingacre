plantedCorn <- get_USDA(commodity_desc = "CORN",
                        statisticcat_desc = "AREA PLANTED",
                        short_desc = "CORN - ACRES PLANTED",
                        agg_level_desc = "NATIONAL",
                        year = 1800:2019,
                        reference_period_desc = "YEAR") %>%
  clean_usda(rename_to = "acre_planted_corn") %>% 
  glimpse()


plantedSoy <- get_USDA(commodity_desc = "SOYBEANS",
                       statisticcat_desc = "AREA PLANTED",
                       short_desc = "SOYBEANS - ACRES PLANTED",
                       agg_level_desc = "NATIONAL",
                       year = 1800:2019,
                       reference_period_desc = "YEAR")%>%
  clean_usda(rename_to = "acre_planted_soy") %>% 
  glimpse()


harvestCorn <- get_USDA(commodity_desc = "CORN",
                        statisticcat_desc = "AREA HARVESTED",
                        short_desc = "CORN, GRAIN - ACRES HARVESTED",
                        agg_level_desc = "NATIONAL",
                        year = 1800:2019,
                        reference_period_desc = "YEAR") %>%
  clean_usda(rename_to = "acre_harvest_corn") %>% 
  glimpse()

harvestSoy <- get_USDA(commodity_desc = "SOYBEANS",
                       statisticcat_desc = "AREA HARVESTED",
                       short_desc = "SOYBEANS - ACRES HARVESTED",
                       agg_level_desc = "NATIONAL",
                       year = 1800:2019,
                       reference_period_desc = "YEAR") %>%
  clean_usda(rename_to = "acre_harvest_soy") %>% 
  glimpse()

yieldCorn <- get_USDA(commodity_desc = "CORN",
                      statisticcat_desc = "YIELD",
                      agg_level_desc = "NATIONAL",
                      year = 1800:2019,
                      util_practice_desc = "GRAIN",
                      reference_period_desc = "YEAR") %>%
  clean_usda(rename_to = "yield_corn") %>% 
  glimpse()

yieldSoy <- get_USDA(commodity_desc = "SOYBEANS",
                     statisticcat_desc = "YIELD",
                     agg_level_desc = "NATIONAL",
                     year = 1800:2019,
                     reference_period_desc = "YEAR") %>%
  clean_usda(rename_to = "yield_soy") %>% 
  glimpse()

price <- fread("../data/raw/price_soy_corn_doe_1866-2019.csv") # avg annual farm price
names(price)[names(price) %in% "Year"] <- "year"

main <- merge(plantedCorn, plantedSoy, all.y = T) %>%
  merge(harvestCorn, all.x = T) %>%
  merge(harvestSoy, all.x = T) %>%
  merge(yieldCorn, all.x = T) %>%
  merge(yieldSoy, all.x = T) %>%
  merge(price, all.x = T) %>%
  as.data.table() %>%
  glimpse()

main[, pratio_cornsoy := corn_adjusted/soy_adjusted]
write.csv(main, "pooled_usda.csv", row.names = F)
