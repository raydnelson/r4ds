# 15.3.1 Exercise

summary(gss_cat$rincome)
gss_cat %>% 
  ggplot(aes(x = rincome, fill = rincome)) +
  geom_bar()

gss_cat %>% 
  ggplot(aes(x = rincome, fill = rincome)) +
  geom_bar(show.legend = FALSE) +
  coord_flip()