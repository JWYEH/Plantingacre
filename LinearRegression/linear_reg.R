devtools::load_all("../BPI")
pkg <- list("data.table", "dplyr", "ggplot2", "kableExtra", "readxl")
apply(pkg, library, character.only = T)


acre <- fread("../data/raw/acreplanted_national_corn_nass_1926-2020.csv")
price <- fread("../data/raw/price_soy_corn_doe_1866-2019.csv")
acre_corn <- merge(acre, price, all.x = T)
acre_corn <- acre_corn[Year!=2020, c("Year", "CORN - ACRES PLANTED  -  <b>VALUE</b>", "corn_adjusted", "soy_adjusted")]
names(acre_corn) <- c("year", "acre_corn", "price_corn", "price_soy")
acre_corn[, acre_corn:=rm_comma(acre_corn)] %>% glimpse()
acre_corn[, pratio_cornsoy := price_corn/price_soy] %>% glimpse()
acre_corn[, acre_corn_norm := get_normalized(acre_corn)] %>% glimpse()


acre_soy <- fread("../data/raw/acreplanted_national_soy_nass_1924-2020.csv")
acre_soy <- acre_soy[Period=="YEAR", c("Year", "Value")]
acre_soy[, Value:=rm_comma(Value)]
names(acre_soy) <- c("year", "acre_soy")

acre_corn <- merge(acre_corn, acre_soy, all.x = T)

plot(acre_corn$pratio_cornsoy, acre_corn$acre_corn_norm)
abline(lm(acre_corn$acre_corn_norm ~acre_corn$pratio_cornsoy))

## revenue data
rev_75 <- read_excel("../data/raw/us_corn_cost_return_1975-1995.xls")
rev_75 <- rev_75[c(2, 44, 55, 59, 60), 1:22]
rev_75 <- transpose(rev_75)
names(rev_75) <- c("year", "gross_prod_yera", "cost_corn", "price_corn", "yield_corn")
rev_75 <- rev_75[-1, ]

rev_96 <- read_excel("../data/raw/us_corn_cost_return_1996-2019.xlsx")
rev_96 <- rev_96[c(6, 14, 60, 66, 64), 1:25]
rev_96 <- transpose(rev_96)
names(rev_96) <- c("year", "gross_prod_yera", "cost_corn", "price_corn", "yield_corn")
rev_96 <- rev_96[-1, ]

rev_corn <- rbind(rev_75, rev_96) %>% glimpse()
rev_corn <- apply(rev_corn, 2, as.numeric)

rev_corn <- data.table(as.numeric(rev_corn))
rev_corn[, .SD := lapply(.SD, as.numeric)]
rev_corn <- rev_corn[order(rev_corn$year),]






scaler = 0.3*10E6
ggplot(acre_corn, aes(x= year)) +
  geom_col(aes(y = acre_corn),  fill = "gray70", color = "gray90") +
  geom_line(aes(y = price_corn*scaler,color = "price corn"), size = 1) +
  geom_line(aes(y = price_soy*scaler,color = "price soy"),  size = 1) +
  scale_y_continuous(name = "acre planted (acre)",
                     sec.axis = sec_axis(~. /scaler, 
                                         name = "USD/BU adjusted by CPI"))


scaler = 10E8
ggplot(acre_corn, aes(x= year)) +
  geom_col(aes(y = acre_corn),  fill = "gray70", color = "gray90") +
  geom_line(aes(y = pratio_cornsoy*scaler,color = "price corn"), size = 1) 


scaler = 0.15*10E8
ggplot(acre_corn, aes(x= year)) +
  geom_line(aes(y = acre_corn)) +
  geom_line(aes(y = pratio_cornsoy*scaler,color = "price ratio corn to soy"), size = 1) 



ggplot(acre_corn, aes(x= pratio_cornsoy, y = acre_corn_norm)) +
  geom_point() +
  geom_smooth(method = 'loess', formula ='y ~ x') 

