# Factors
# Initial: 25 Oct 2018
# Revision: 25 Feb 2019
# Ray Nelson

# Libraries
library(tidyverse)
library(forcats)

# 15.2 Creating factors
(x1 <- c("Dec", "Apr", "Jan", "Mar"))
(x2 <- c("Dec", "Apr", "Jam", "Mar"))

month_levels <- c(
  "Jan", "Feb", "Mar", "Apr", "May", "Jun", 
  "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
)

(y1 <- factor(x1, levels = month_levels))


(y2 <- factor(x2, levels = month_levels))


factor(x1)

f1 <- factor(x1, levels = unique(x1))
unique(x1)

(f2 <- x1 %>% factor() %>% fct_inorder())


f2 %>% levels()

# 15.3 General Social Survey
gss_cat
gss_cat %>% glimpse()
gss_cat %>% summary()

gss_cat %>% 
  count(race)

gss_cat %>% 
  ggplot(aes(x = race, fill = race)) +
    geom_bar(show.legend = FALSE)

gss_cat %>% 
  ggplot(aes(x = race, fill = race)) +
  geom_bar(show.legend = FALSE) +
  scale_x_discrete(drop = FALSE)

# 15.4 Modifying factor order

## TV Hours by religion
relig_summary <- gss_cat %>% 
  group_by(relig) %>% 
  summarise(
    age = mean(age, na.rm = TRUE),
    tvhours = mean(tvhours, na.rm = TRUE),
    n = n()
  )

relig_summary$relig %>% 
  levels()

relig_summary$relig %>% levels()

relig_summary %>% 
  ggplot(aes(tvhours, relig)) +
    geom_point()

relig_summary %>% 
  ggplot(aes(tvhours, fct_reorder(relig, tvhours, .desc = TRUE))) +
  geom_point()

relig_summary %>% 
  mutate(relig = fct_reorder(relig, tvhours))
  ggplot(aes(tvhours, relig)) +
    geom_point()
  
relig_summary <- relig_summary %>%
  mutate(relig = fct_reorder(relig, tvhours, desc = TRUE))

relig_summary$relig %>% 
  levels()

## Average age over reported income levels

(rincome_summary <- gss_cat %>%
  group_by(rincome) %>%
  summarise(
    age = mean(age, na.rm = TRUE),
    n = n()
  ))

rincome_summary %>%
  mutate(rincome = fct_reorder(rincome, age))  %>% 
  ggplot(aes(x= age, y = rincome)) +
    geom_point()

rincome_summary %>%
  ggplot(aes(x= age, y = rincome)) +
    geom_point()

# Age by marital status

by_age <- gss_cat %>%
  filter(!is.na(age)) %>%
  count(age, marital) %>%
  group_by(age) %>%
  mutate(prop = n / sum(n))

by_age %>% 
  ggplot(aes(x = age, y = prop, color = marital)) +
    geom_line(na.rm = TRUE)
  
by_age %>% 
  ggplot(aes(x = age, y = prop, color = fct_reorder2(marital, age, prop))) +
    geom_line(na.rm = TRUE) +
    labs(colour = "marital")

gss_cat %>% 
  mutate(marital = marital %>% fct_infreq() %>% fct_rev()) %>% 
  ggplot(aes(x = marital, fill = marital)) +
    geom_bar(show.legend = FALSE) +
  labs(
    title = "Marital Status",
    subtitle = "General Social Science Survey",
    x = "",
    y = "Number of Respondents"
  )

# 15.5 Modifying factor levels

gss_cat %>% count(partyid)

gss_cat %>%
  mutate(partyid = fct_recode(partyid,
                              "Republican, strong"    = "Strong republican",
                              "Republican, weak"      = "Not str republican",
                              "Independent, near rep" = "Ind, near rep",
                              "Independent, near dem" = "Ind, near dem",
                              "Democrat, weak"        = "Not str democrat",
                              "Democrat, strong"      = "Strong democrat"
  )) %>%
  count(partyid)

gss_cat %>%
  mutate(partyid = fct_recode(partyid,
                              "Republican, strong"    = "Strong republican",
                              "Republican, weak"      = "Not str republican",
                              "Independent, near rep" = "Ind,near rep",
                              "Independent, near dem" = "Ind,near dem",
                              "Democrat, weak"        = "Not str democrat",
                              "Democrat, strong"      = "Strong democrat",
                              "Other"                 = "No answer",
                              "Other"                 = "Don't know",
                              "Other"                 = "Other party"
  )) %>%
  count(partyid)

gss_cat %>%
  mutate(partyid = fct_collapse(partyid,
                                other = c("No answer", "Don't know", "Other party"),
                                rep = c("Strong republican", "Not str republican"),
                                ind = c("Ind,near rep", "Independent", "Ind,near dem"),
                                dem = c("Not str democrat", "Strong democrat")
  )) %>%
  count(partyid)

gss_cat %>%
  mutate(relig = fct_lump(relig)) %>%
  count(relig)

gss_cat %>%
  mutate(relig = fct_lump(relig, n = 10)) %>%
  count(relig, sort = TRUE) %>%
  print(n = Inf)