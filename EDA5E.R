library(ggplot2)
library(dplyr)
data(diamonds)
     
ggplot(diamonds, aes(x = price, fill = cut)) +geom_histogram() +facet_wrap(~ color, ncol = 2) +scale_fill_brewer(type = "qual") +labs(title = "Diamond Prices by Color (D-J)")
ggplot(diamonds, aes(x = table, y = price, color = cut)) +
  geom_point() +
  scale_color_brewer(type = "qual")

diamonds$volume = with(diamonds, x * y * z)
diamonds$volume[diamonds$volume == 0] <- NA

ggplot(diamonds, aes(x = volume, y = price, color = clarity)) + 
  geom_point() + scale_color_brewer(type = "div") +
  scale_x_continuous(limits = c(0, quantile(diamonds$volume, 0.99, na.rm = TRUE))) +
  scale_y_continuous(trans = "log10")

setwd("~/Work/R/SlideRule/Data")
pf<-read.delim("pseudo_facebook.tsv")

pf$prop_initiated <- 
  with(pf, ifelse(friend_count == 0,
                  as.numeric(NA),  
                  as.numeric(friendships_initiated) / as.numeric(friend_count)
  )
  )
pf$year_joined <- as.integer(2014 - ceiling(pf$tenure / 365))
pf$year_joined.bucket <- cut(pf$year_joined, breaks = c(2004, 2009, 2011, 2012, 2014))
pf.prop_by_tenure <- summarise(group_by(pf, tenure, year_joined.bucket), 
                               median_prop_initiated = median(prop_initiated)
)
pf.prop_by_tenure <- pf.prop_by_tenure[complete.cases(pf.prop_by_tenure), ]
ggplot(pf.prop_by_tenure, aes(x = tenure, y = median_prop_initiated, color = year_joined.bucket)) +
  geom_line() +
  labs(x = "Tenure (days)",
       y = "Proportion of Friendships Initiated (median)",
       color = "Year Joined"
  )

ggplot(pf.prop_by_tenure, aes(x = tenure, y = median_prop_initiated, color = year_joined.bucket)) +
  geom_smooth() +
  labs(x = "Tenure (days)",
       y = "Proportion of Friendships Initiated (median)",
       color = "Year Joined"
  )

with(pf[pf$year_joined.bucket == "(2012,2014]", ], mean(prop_initiated, na.rm = TRUE))

ggplot(diamonds, aes(cut, price/carat, colour = color)) + 
  geom_point(position = 'jitter') +
  scale_color_brewer(type = 'div') +
  facet_wrap (~ clarity)

load('ocpc.RData')
load('black_gold.RData')

# Rank countries in black_gold by correlation between Income and Consumption
bg.cor <- summarise(group_by(black_gold, Country),
                    Cor = cor(Income, Consumption))
bg.cor <- bg.cor[order(bg.cor$Cor), ]
black_gold$Country <- factor(black_gold$Country, levels = bg.cor$Country)

# Show summary statistics for bg.cor
summary(bg.cor)

for(y in seq(1965, 2010, 15)) {
  writeLines(paste("\nYear", as.character(y), "Summary Statistics for black_gold:"))
  print(summary(black_gold[black_gold$Year == as.character(y), ]))
}

consumption.delta <- summarise(group_by(ocpc, Country),
                               Delta = max(Consumption) - min(Consumption)
)
delta_ranking <- consumption.delta$Country[order(-consumption.delta$Delta)]
ocpc$Country <- factor(ocpc$Country, levels = delta_ranking)


ggplot(ocpc[ocpc$Country %in% delta_ranking[1:9], ], aes(x = Year, y = Consumption, group = Country)) +
  geom_point() +
  facet_wrap(~ Country) +
  scale_x_discrete(breaks = as.character(seq(1965, 2010, 10)))

country.cor <- function(x) {return(round(bg.cor[bg.cor$Country == x, "Cor"], 2))}

country.cor.labeller <- function(x) {
  return(ifelse(x %in% bg.cor$Country,
                paste(x, " (cor = ", country.cor(x), ")", sep = ""),
                NA))}
bg.cor$CountryLabel <- sapply(bg.cor$Country, country.cor.labeller)


black_gold$CountryLabel <- factor(sapply(black_gold$Country, country.cor.labeller), levels = unique(sapply(black_gold$Country, country.cor.labeller)))

black_gold$CountryLabel <- factor(black_gold$CountryLabel, levels = union(bg.cor[order(bg.cor$Cor), ]$CountryLabel, NA))


ggplot(black_gold[black_gold$CountryLabel %in% bg.cor$CountryLabel[1:9], ],
       aes(x = Year, y = Consumption, group = CountryLabel,
           colour = Income)) +
  geom_point() +
  facet_wrap( ~ CountryLabel) +
  scale_x_discrete(breaks = as.character(seq(1965, 2010, 10))) +
  scale_color_gradient(name = "Income (2000 USD)", low = "red", high = "forestgreen", trans = "sqrt") +
  labs(title = "Per Capita Oil Consumption vs. Income, Lowest Correlations")

bg.cor <- bg.cor[order(-bg.cor$Cor), ]  # Put countries with highest Cor at the top of the list

ggplot(black_gold[black_gold$CountryLabel %in% bg.cor$CountryLabel[1:9], ],
       aes(x = Year, y = Consumption, group = CountryLabel,
           colour = Income)) +
  geom_point() +
  facet_wrap( ~ CountryLabel) +
  scale_x_discrete(breaks = as.character(seq(1965, 2010, 10))) +
  scale_color_gradient(name = "Income (2000 USD)", low = "red", high = "forestgreen", trans = "sqrt") +
  labs(title = "Per Capita Oil Consumption vs. Income, Highest Correlations")

ggplot(black_gold[black_gold$CountryLabel %in% bg.cor$CountryLabel[1:10] & black_gold$Country != 'Singapore', ],
       aes(x = Year, y = Consumption, group = CountryLabel,
           colour = Income)) +
  geom_point() +
  facet_wrap( ~ CountryLabel) +
  scale_x_discrete(breaks = as.character(seq(1965, 2010, 10))) +
  scale_color_gradient(name = "Income (2000 USD)", low = "red", high = "forestgreen", trans = "sqrt") +
  labs(title = "Per Capita Oil Consumption vs. Income, Highest Correlations")

income_ranking_2010.desc <- factor(black_gold[black_gold$Year == 2010, ][order(-black_gold[black_gold$Year == 2010, ]$Income), ]$Country)
black_gold$Country <- factor(black_gold$Country, levels = union(income_ranking_2010.desc, unique(black_gold$Country)))

# Plot consumption over time for countries with highest incomes
ggplot(black_gold[black_gold$Country %in% income_ranking_2010.desc[1:9], ],
       aes(x = Year, y = Consumption, group = CountryLabel,
           colour = Income)) +
  geom_point() +
  facet_wrap(~CountryLabel) +
  scale_x_discrete(breaks = as.character(seq(1965, 2010, 10))) +
  scale_color_gradient(name="Income (2000 USD)", low = "red", high = "forestgreen", trans = "sqrt") +
  labs(title = "Per Capita Oil Consumption vs. Income, Highest Incomes")

income_ranking_2010 <- factor(black_gold[black_gold$Year == 2010, ][order(black_gold[black_gold$Year == 2010, ]$Income), ]$Country)
black_gold$Country <- factor(black_gold$Country, levels = union(income_ranking_2010, unique(black_gold$Country)))

# Plot consumption over time for countries with lowest incomes
ggplot(black_gold[black_gold$Country %in% income_ranking_2010[1:9], ],
       aes(x = Year, y = Consumption, group = CountryLabel,
           colour = Income)) +
  geom_point() +
  facet_wrap(~CountryLabel) +
  scale_x_discrete(breaks = as.character(seq(1965, 2010, 10))) +
  scale_color_gradient(name="Income (2000 USD)", low = "red", high = "forestgreen", trans = "sqrt") +
  labs(title = "Per Capita Oil Consumption vs. Income, Lowest Incomes")

ggplot(black_gold, aes(Year, Income, colour = Consumption)) +
  geom_jitter() +
  scale_color_gradient(name = "Oil Consumption (tons/person/year)", low = "forestgreen", high = "red", trans = "sqrt") +
  scale_x_discrete(breaks = seq(1965, 2010, 10)) +
  scale_y_continuous(breaks = seq(0, 50000, 5000))


