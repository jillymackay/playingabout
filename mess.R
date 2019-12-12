# I need a distraction today


library(tidyverse)
library(scales)
library(ggwordcloud)



jobs_gender <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-03-05/jobs_gender.csv")
earnings_female <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-03-05/earnings_female.csv") 
employed_gender <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-03-05/employed_gender.csv") 





employed_gender %>%
  gather(key = category, value = employed, - year) %>% 
  mutate(gender = case_when(str_detect(category, "female") ~ "female",
                            str_detect(category, "male") ~ "male",
                            str_detect(category, "total") ~ "total"),
         time = case_when(str_detect(category, "full_time") ~ "full time",
                          str_detect(category, "part_time") ~ "part time")) %>% 
  ggplot(aes(x = year, y = employed)) +
  geom_point(aes(shape = time, colour = gender))



earnings_female %>% 
  ggplot(aes(x = Year, y = percent )) +
  geom_point(aes(colour = group)) +
  labs(y = "Female salary percent of male salary")



jobs_gender %>% 
  ggplot(aes(x = as.factor(year), y = percent_female)) +
  geom_boxplot() +
  geom_jitter(aes(shape = major_category))

jobs_gender %>% 
  ggplot(aes(x = total_earnings_female, y = total_earnings_male, 
             color = major_category, shape = major_category)) +
  geom_point() + 
  facet_wrap(~year)

jobs_gender %>% 
  ggplot(aes(label = major_category, n = wage_percent_of_male)) +
  geom_text_wordcloud() +
  theme_classic()










library(gganimate)


employed_gender %>%
  gather(key = category, value = employed, - year) %>% 
  mutate(gender = case_when(str_detect(category, "female") ~ "female",
                            str_detect(category, "male") ~ "male",
                            str_detect(category, "total") ~ "total"),
         time = case_when(str_detect(category, "full_time") ~ "full time",
                          str_detect(category, "part_time") ~ "part time")) %>% 
  ggplot(aes(x = year, y = employed)) +
  geom_point(aes(colour = gender, shape = time, size = 5))+
  geom_line(aes(colour = gender, shape = time)) +
  theme_classic() +
  theme(legend.position = "bottom") +
  transition_reveal(year)


employed_gender %>%
  gather(key = category, value = employed, - year) %>% 
  mutate(gender = case_when(str_detect(category, "female") ~ "female",
                            str_detect(category, "male") ~ "male",
                            str_detect(category, "total") ~ "total"),
    category = case_when(category == "full_time_female" ~ "Female (Full Time)",
                              category == "full_time_male" ~ "Male (Full Time)",
                              category == "total_full_time" ~ "Full Time Employment",
                              category == "part_time_female" ~ "Female (Part Time)",
                              category == "part_time_male" ~ "Male (Part Time)",
                              category == "total_part_time" ~ "Part Time Employment")) %>% 
  ggplot(aes(x = year, y = employed)) +
  geom_line(aes(color = category, shape = gender)) +
  geom_point(aes(shape = gender, color = category)) +
  theme_classic() +
  labs(x = "Year", y = "Proportion of Population\nEmployed")+
  scale_y_continuous(labels = percent_format(scale =1)) +
  theme(legend.position = "bottom") +
  transition_reveal(year)





earnings_female %>% 
  ggplot(aes(x = Year, y = percent )) +
  geom_line(aes(colour = group)) +
  labs(y = "Female salary percent of male salary") +
  theme_classic() +
  geom_point(aes(color = group)) +
  theme(legend.position = "top") +
  labs(colour = "Age Group", y = "Avg Female Salary\nAs % of Male Salary") +
  scale_color_ochre(palette = "williams_pilbara")+
  transition_reveal(Year)

                     