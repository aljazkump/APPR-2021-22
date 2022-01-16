library(rgdal);
library(tidyverse);
library(maptools);

obcine_odseljeni <- read.csv("podatki/obcine.csv");
obcine_prebivalstvo <- read.csv("podatki/obcine_prebivalstvo.csv");

a = as.integer(obcine_odseljeni[1:212,12]);
b = as.integer(obcine_prebivalstvo[1:212, 22]);

delez = round((a/b)*100, digits = 2);

df = data.frame(
  id = as.character(0 : 211),
  delez = (delez + 1)**(1/4)
);

SIob <- readOGR("podatki\\OB.shp", layer = "OB", encoding = "UTF-8");

SIob_fort <- SIob %>%
  fortify("region");

SIob_fort <- left_join(SIob_fort, df, by = "id");



# VIZUALIZACIJA SLOVENSKIH OBCIN

SIob_fort %>%
  ggplot(aes(long, lat, group = group)) +
  geom_path(size = 0.01) +
  geom_polygon(aes(fill = delez), color = "black", show.legend = FALSE) +
  scale_fill_gradient(low = "yellow", high = "red") +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.background = element_blank(),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.background = element_blank()
  );



