
# Libraries

devtools::install_github("Ryo-N7/tvthemes")

library(tidyverse)
library(lubridate)
library(ggrepel)
library(png)
library(gridGraphics)
library(gganimate)
library(transformr)

# The data

tdf_winners <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-07/tdf_winners.csv')


View(tdf_winners)

# A feel of the data

glimpse(tdf_winners)

# Extracting the year they were born & died; lubridate package

tdf_winners$year_born <- year(tdf_winners$born)

tdf_winners$year_died <- year(tdf_winners$died)

tdf_winners$years_lived <- tdf_winners$year_died-tdf_winners$year_born

tdf_winners$start_year <- year(tdf_winners$start_date)

# Goal; A graph of age as winner vs year they were born

tdf_winners %>%
  ggplot(aes(x =years_lived, y = age)) +
  geom_line() +
  geom_label_repel(data = tdf_winners %>% sample_n(10), 
                   aes(label = winner_name), size = 2,  
                   nudge_y = -7, na.rm = TRUE,
                   segment.alpha = 0.2) + 
  xlab('Years Lived') + 
  ylab('Age') + 
  ggtitle('Years Lived vs Age as Winner') + 
  theme(legend.position = "none")

# The Bicyle Image

img_bike2 <- readPNG("C:/Users/kinyanjuim/Downloads/bike5.png")
fig_image2 <- rasterGrob(img_bike2, interpolate = TRUE)


# The earlier line graph


q <- ggplot(data = tdf_winners, mapping = aes(x =start_year,
                                                  y = distance)) +
  geom_path(color = "purple", size = 1.2)+
  geom_label_repel(data = tdf_winners %>% 
                     sample_n(10, replace = TRUE), 
                   aes(label = winner_name), size = 3,  
                   box.padding   = 0.5, 
                   point.padding = 0.5,
                   segment.color = 'grey50') +
  # add bike
  annotation_custom(grob=fig_image2,
                    xmin= 1905, xmax=1920, ymin=2500, ymax=3200) +
  annotation_custom(grob=fig_image2,
                    xmin= 1920, xmax=1940, ymin=3000, ymax=3700) +
  annotation_custom(grob=fig_image2,
                    xmin= 1950, xmax=1965, ymin=3300, ymax=4000) +
  annotation_custom(grob=fig_image2,
                    xmin= 1975, xmax=1990, ymin=2500, ymax=3200) +
  annotation_custom(grob=fig_image2,
                    xmin= 2000, xmax=2020, ymin=2300, ymax=3000) +
  xlab("Start Year of the Tour")+
  ylab("Distance Travelled")+
  labs(title = "Tour de France",
       subtitle = "Distance Traveled in km vs Start Year of the Tour", 
       caption = "Source: tdf package| plot by @magwanjiru")+
  theme(panel.background = element_rect(fill = 'white', color = 'lightblue', size = 0.5),
        # panel.grid.major = element_line(color = 'white', linetype = 'dashed'),
        legend.position="none",
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size=12, face = "italic", hjust = 0.5),
        plot.caption = element_text(size = 8, face = "italic", color = "blue"),
        axis.line = element_line(size = 0.1, linetype = "solid", colour = "black"))

q

ggsave("q.png")

q1 <- q+
  transition_reveal(start_year)

animate(q1, fps = 10, end_pause = 10)


anim_save("france_cycling2.gif")
