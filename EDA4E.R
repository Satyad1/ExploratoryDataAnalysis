library(ggplot2)
library(datasets)
library(tidyr)
library(dplyr)
library(gridExtra)

data(diamonds)

ggplot(diamonds, aes(x, price)) + geom_point()

ggplot(diamonds[diamonds$x > 0 & diamonds$price > 0, ]) + 
  geom_point(aes(x = x, y = price), alpha = .05, color = "blue") + 
  labs(title = "Diamond Price vs. Length", x = "Length of diamond (mm)", y = "Price of diamond (USD)") +
  scale_x_continuous(breaks = 0:12, limits = c(min(diamonds$x[diamonds$x > 0]), quantile(diamonds$x, 0.999))) +
  scale_y_continuous(breaks = seq(0, max(diamonds$price), 2000))

with(diamonds,
     data.frame(cor_x_price = cor(x, price),
                cor_y_price = cor(y, price),
                cor_z_price = cor(z, price)
     )
)

ggplot(diamonds, aes(depth, price)) +
  geom_point() +
  labs(x = "Depth (%)", y = "Price (USD)", title = "Diamond Price vs. Depth")

ggplot(diamonds, aes(depth, price)) +
  geom_point(alpha = 0.01) +
  scale_x_continuous(breaks = seq(0, 100, 2)) +
  labs(x = "Depth (%)", y = "Price (USD)", title = "Diamond Price vs. Depth")

with(diamonds,
     c(quantile(depth, 0.05),
       quantile(depth, 0.25),
       quantile(depth, 0.75),
       quantile(depth, 0.95)
     )
)

with(diamonds, cor(depth, price))

ggplot(diamonds, aes(carat, price)) +
  geom_point(colour = "blue") +
  scale_x_continuous(limits = c(0, quantile(diamonds$carat, 0.99))) +
  scale_y_continuous(limits = c(0, quantile(diamonds$price, 0.99))) +
  labs(title = "Diamond Price vs. Mass", x = "Mass (carats)", y = "Price (USD")  

diamonds$volume <- with(diamonds, x * y * z)
ggplot(diamonds, aes(volume, price)) + geom_point()

with(diamonds[diamonds$volume > 0 & diamonds$volume < 800, ], cor(volume, price))

ggplot(diamonds[diamonds$volume > 0 & diamonds$volume < 800, ], aes(x = volume, y = price)) +
  geom_point(alpha = 1/100, colour = "blue") + 
  geom_smooth(method = "lm", colour = "purple") +
  labs(title = "Price vs. Volume", x = "Volume", y = "Price (USD)" )

diamondsByClarity <- summarise(group_by(diamonds, clarity),
                               mean_price = mean(price),
                               median_price = median(price),
                               min_price = min(price),
                               max_price = max(price),
                               n = n()
)

diamondsByClarity

# Bar plot diamond mean price by clarity
p1 <- ggplot(diamonds_mp_by_clarity, aes(clarity, mean_price)) + geom_bar(stat = "identity") +
  labs(title = "Diamonds: Mean Price by Clarity", x = "Diamond Clarity", y = "Mean Price (USD)")

# Bar plot diamond mean price by color
p2 <- ggplot(diamonds_mp_by_color, aes(color, mean_price)) + geom_bar(stat = "identity") +
  labs(title = "Diamonds: Mean Price by Color", x = "Diamond Color", y = "Mean Price (USD)")

# Output both plots as a single image
grid.arrange(p1, p2)



