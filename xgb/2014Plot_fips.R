library(usmap)
library(ggplot2)
library(maps)
library(dplyr)
library(tidyverse)

x <- fread("../data/raw/GPOS_2006-2015.csv")

maps::county.fips %>%
  as.tibble %>%
  extract(polyname, c("region", "subregion"), "^([^,]+),([^,]+)$") ->
  dfips

map_data("county") %>%
  left_join(dfips) ->
  dall



y <- x[x$year ==2015,]

ExampleFIPS <- x$SHIPPING_FIPS_CODE


z <- dall %>%
  mutate(is_example = fips %in% ExampleFIPS) %>%
  ggplot(aes(long, lat, group = group)) +
  geom_polygon(aes(fill=is_example), color="gray70") +
  coord_map() +
  scale_fill_manual(values=c("TRUE"="red", "FALSE"="gray90"))
plot(z, main = as.character(i))

