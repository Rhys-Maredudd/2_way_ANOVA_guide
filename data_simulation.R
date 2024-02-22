# Simulating data for 2 way ANOVA script
library(tidyverse)
set.seed(123)


### Human (highest group)
set.seed(12)
Franchise_enjoyment <-  as.integer(rnorm(100,c(50, 80),sd=5))
summary(Franchise_enjoyment)
test <- as.data.frame(Franchise_enjoyment)
human <- test %>% mutate(
  fan_status = case_when(Franchise_enjoyment >= 64 ~ "loyal_fan",
             Franchise_enjoyment < 63 ~ "non_fan")) 

                                               
                                               
summary(as.factor(human$fan_status))
t.test(Franchise_enjoyment ~ fan_status, humans)

### Martians (mid group)

set.seed(123)
Franchise_enjoyment <-  as.integer(rnorm(80,c(40, 60),sd=10))
summary(Franchise_enjoyment)
test <- as.data.frame(Franchise_enjoyment)
martian <- test %>% mutate(fan_status = case_when(Franchise_enjoyment > 48.5 ~ "loyal_fan",
                                                 Franchise_enjoyment < 48.5 ~ "non_fan"))
summary(as.factor(martian$fan_status))
t.test(Franchise_enjoyment ~ fan_status, martian)


### Androids (low group)

set.seed(123)
Franchise_enjoyment <-  as.integer(rnorm(60,c(20, 50),sd=7))
summary(Franchise_enjoyment)
test <- as.data.frame(Franchise_enjoyment)
android <- test %>% mutate(fan_status = case_when(Franchise_enjoyment > 33 ~ "loyal_fan",
                                                   Franchise_enjoyment < 32 ~ "non_fan"))

t.test(Franchise_enjoyment ~ fan_status, android)

data <- data.table::rbindlist(list(human, martian, android), idcol = TRUE)
summary(data)

# inspecting simulation 

data %>% 
  group_by(.id, fan_status) %>%
  summarise(mean_enjoy = mean(Franchise_enjoyment),
            sd_enjoy = sd(Franchise_enjoyment))

# creating population group variable 

data2 <- data %>% 
  mutate( population_group = case_when(.id == "1" ~ "human",
                                       .id == "2" ~ "martian",
                                       .id == "3" ~ "android"),
          population_group = as.factor(population_group),
          fan_status = as.factor(fan_status)) %>% 
  select(population_group, fan_status, Franchise_enjoyment)

summary(data2)

# testing data + analyses

data2 %>% 
  group_by(population_group, fan_status) %>%
  summarise(mean_enjoy = mean(Franchise_enjoyment),
            sd_enjoy = sd(Franchise_enjoyment))

m <- aov(Franchise_enjoyment ~ population_group , data2)
summary(m)
DescTools::PostHocTest(m, method = "bonferroni")
m <- aov(Franchise_enjoyment ~ population_group * fan_status, data2)
summary(m)

DescTools::PostHocTest(m, method = "bonferroni")

m <- lm(Franchise_enjoyment ~ population_group * fan_status + 0, data2)
summary(m)

ggplot(data2,
       aes(fill = population_group, y = Franchise_enjoyment, x = population_group)) +
  geom_violin() +
  geom_boxplot(alpha = .7, width = .3) +
  theme(legend.position = "bottom") +
  facet_wrap(~fan_status)

# Saving data

write.csv(data2, "~Star_battle.csv")

