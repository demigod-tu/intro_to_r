library(ggplot2)
library(data.table)
library(reshape2)
library(dplyr)

load("suicides.rdata")
all_suicides <- copy(suicides)
suicides <- suicides %>% 
  group_by(year, state, means) %>% 
  mutate(deaths = sum(deaths))

bare <- ggplot(suicides)

aesthetic <- ggplot(suicides, aes(x=year, y=deaths))

scatter <- ggplot(suicides, aes(x=year, y=deaths)) +
  geom_point()

color_by_means <- ggplot(suicides, aes(x=year, y=deaths, color=means)) +
  geom_point(size=3)

scatter_by_state <- ggplot(suicides, aes(x=year, y=deaths, color=means)) +
  geom_point(size=3) +
  facet_wrap(~state, scales="free")

line_by_state <- ggplot(suicides, aes(x=year, y=deaths, color=means)) +
  geom_line(size=3) +
  facet_wrap(~state, scales="free")

bar_by_state <- ggplot(suicides, aes(x=year, y=deaths, color=means)) +
  geom_bar(aes(fill=means), stat="identity") +
  facet_wrap(~state, scales="free")

one_state <- all_suicides[all_suicides$state=="Haryana"] %>% 
  group_by(year, state, sex, age, means) %>% 
  mutate(deaths = sum(deaths))

density_plots <- ggplot(one_state, aes(x=deaths)) +
  geom_density(aes(color=means, fill=means), size=1, alpha=0.5) +
  facet_grid(age~sex, scales="free") +
  labs(title="Distribution of Suicides by Age, Sex, and Means, 2001-2010",
       x="Deaths",
       y="Density")

one_state <- all_suicides[all_suicides$state=="Haryana"] %>% 
  group_by(year, state, means) %>% 
  mutate(deaths = sum(deaths))

point_line <- ggplot(one_state, aes(x=year, y=deaths)) +
  geom_line(aes(color=means), size=2) +
  geom_point(aes(shape=means, color=means),  size=3)