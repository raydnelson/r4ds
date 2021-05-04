write.csv(ecommerce_tibble, "ecommerce.csv")
write.csv(seattle_tibble, "seattle.csv")


retail %>% 
  stl(s.window = "periodic")  %>% 
  autoplot() +
  labs(
    title = "Season-Trend with Loess Decomposition of Retail Sales",
    x = "",
    y = "Billions of Dollars"
  )

library(Quandl)
building <- Quandl("FRED/RSBMGESDN", type = "ts")
vehicle <- Quandl("FRED/TOTALNSA", type = "ts")


building %>% 
  stl(s.window = 7)  %>% 
  autoplot() +
  labs(
    title = "Season-Trend with Loess Decomposition of Building Materials",
    x = "",
    y = "Billions of Dollars"
  )

building %>% 
  forecast(h = 36) %>% 
  autoplot() +
  labs(
    title = "Building Materials, Garden Equipment and Supplies Dealers",
    x = "",
    y = "Billions of Dollars"
  )



vehicle_tibble <- tibble(time = as.Date(time(vehicle %>% as.timeSeries)),
                         sales = series(vehicle %>% as.timeSeries))


vehicle_tibble %>% 
  ggplot(aes(x = time, y = sales)) +
  geom_line() +
  geom_smooth(span = .2, se = FALSE) +
  labs(
    title = "The business cycle strongly influences total vehicle sales.",
    y = "Thousands of Units",
    x = "",
    caption = "Source: U.S. Bureau of Economic Analysis"
  )


