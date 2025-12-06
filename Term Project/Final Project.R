library(tidyverse)

people <- read_csv("Term Project/hormone-diversity-individual.csv")
team <- read_csv("Term Project/hormone-diversity-teams.csv")

full <- people %>%
  full_join(team, by = "team.id")

mean_by_team <- people %>%
  group_by(team.id) %>%
  summarize(mean_test = mean(Testosterone, na.rm = TRUE), .groups = "drop")


sorted <- team %>%
  left_join(mean_by_team, by = "team.id")

men_split <- 200
female_split <- 60

people_cleaned <- people %>%
  left_join(team, by = "team.id") %>% 
  mutate(
    Testosterone_level = case_when(
      Gender == "Male"   & Testosterone > men_split    ~ "High",
      Gender == "Female" & Testosterone > female_split ~ "High",
      TRUE                                                  ~ "Low"
    )
  )


people_cleaned %>%
  ggplot(aes(x = final.performance, color = Testosterone_level)) +
  geom_density()

people_cleaned %>%
  ggplot(aes(x = Testosterone_level)) +
  geom_bar()








