library(tidyverse)
library(showtext)
font_add_google("Puritan", "Puritan")
showtext_auto()
Total_Population <- read_csv("Total_Population.csv")

gannett <- function() {
  theme(axis.title = element_text(size = 9, family = "serif"),
        axis.text = element_text(size = 7, family = "serif"),
        plot.title = element_text(size = 10, family = "Puritan", hjust = .5, vjust = -3),
        axis.title.y = element_text(hjust = .5),
        axis.ticks = element_blank(),
        axis.text.y = element_text(margin = margin(l = 20, r = -20.5)),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        aspect.ratio = 1.2,
        panel.border = element_rect(fill = NA, size = 1.5)
  )
}
Mod_Total_Population <- Total_Population %>% 
  mutate(New_censuses = as.character(Censuses)) %>% 
  select(New_censuses, `Per sq. Mile`)

ggplot(Mod_Total_Population) +
  annotate("segment", x = seq(0,20,2), xend = seq(0,20,2), y = 0, yend = 11.38) +
  annotate("segment", x = -2, xend = 22, y = 11.38, yend = 11.38) +
  geom_col(aes(y = New_censuses, x = `Per sq. Mile`), width = .4, color = "#1a1e19", fill = "#100e05") +
  labs(y = NULL, x = NULL, title = "Number of Inhabitants to the Square Mile \nat Each Census: 1790 to 1890") +
  scale_y_discrete(limits = rev) +
  scale_x_continuous(breaks = seq(0,20,2), position = "top", limit = c(-2,22)) +
  theme_linedraw() +
  coord_cartesian(xlim = c(-.5,20.85), ylim = c(1, 11.15)) +
  theme(axis.title = element_text(size = 9, family = "serif"),
        axis.text = element_text(size = 7, family = "serif"),
        plot.title = element_text(size = 13, family = "Puritan", hjust = .5, vjust = -3),
        axis.title.y = element_text(hjust = .5),
        axis.ticks = element_blank(),
        axis.text.y = element_text(margin = margin(r = -20.5)),
        axis.text.x.top = element_text(margin = margin(b = -13, t = 15)),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        aspect.ratio = 1.2,
        panel.border = element_rect(fill = NA, size = 1.05)
  )
  

