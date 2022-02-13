library(tidyverse)
Total_Population <- read_csv("Total_Population.csv")

Urban_Pop_Graph <- Total_Population %>% 
  mutate(Urban_Pop = `Urban Pop.` / 1000000,
         New_censuses = as.character(Censuses),
         New_population = Population / 1000000) %>% 
  select(Urban_Pop, New_censuses, New_population)

gannett <- function() {
  theme(axis.title = element_text(size = 6, family = "serif"),
        plot.title = element_text(size = 8, family = "Puritan", hjust = .5, vjust = -.5),
        axis.title.y = element_text(hjust = .5),
        axis.text.y = element_text(size = 7, margin = margin(l = 20, r = -21)),
        axis.text.x = element_text(size = 6),
        axis.ticks = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        # Make the graph wider instead of taller
        aspect.ratio = .3,
        panel.border = element_rect(fill = NA, size = 1.5)
  )
}


ggplot(Urban_Pop_Graph) +
  # Total population
  geom_col(aes(y = New_censuses, x = New_population), width = .6, color = "black", fill = "white", alpha = 0) +
  # Urban Population overlaid on total population
  geom_col(aes(y = New_censuses, x = Urban_Pop), width = .6, fill = "#1a1e19") +
  labs(x = "[Millions of Inhabitants]", y = "", title = "Urban and Total Population at Each Census: 1790 to 1890",
       subtitle = "   The total length of each bar represents the aggregate population, while the black portion of each bar indicates the urban element--that is, the population contained in cities \nhaving 8000 inhabitants or more.") +
  scale_y_discrete(limits = rev) +
  # Add box to go around the census years
  coord_cartesian(xlim = c(.4, 62)) +
  scale_x_continuous(breaks = seq(0,60, by = 10), position = "top", limits = c(0,70)) +
  theme_linedraw() +
  theme(axis.title = element_text(size = 6, family = "serif"),
        plot.title = element_text(size = 8, family = "Puritan", hjust = .5, vjust = -10),
        plot.subtitle = element_text(size = 7.5, family = "Puritan", vjust = -120, hjust = 0),
        axis.title.y = element_text(hjust = .5),
        axis.text.y = element_text(size = 7, margin = margin(l = 20, r = -21)),
        axis.text.x = element_text(size = 6),
        axis.ticks = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        # Make the graph wider instead of taller
        aspect.ratio = .3,
        panel.border = element_rect(fill = NA, size = 1.25)
  )
  
