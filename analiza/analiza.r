# 4. faza: Napredna analiza podatkov

# Razvrscanje najvecjih skupin
# Pri moskih je srednjosolska

Najvecja_skupina_moski <- Starost_Izobrazba_Spol %>% filter(Izobrazba == "SS_moski")

data1 <- Najvecja_skupina_moski %>% select(Leto, Stevilo) %>% as.matrix() %>% scale()
D1 <- dist(data1)
model1 <- hclust(D1)
ddata1 <- dendro_data(model1, type = "rectangle")

ddata1$labels$label<- paste(Najvecja_skupina_moski$Starost[model1$order])
ddata1$labels$Starost <- Najvecja_skupina_moski$Starost[model1$order]

dendogram1 <- ggplot(segment(ddata1)) +
  geom_segment(aes(x = x, y = y, xend = xend, yend = yend )) +
  coord_flip() +
  scale_y_reverse(expand = c(0.2,0)) +
  geom_text(
    data = ddata1$labels,
    aes(x=x, y=y, label=label, color = Starost), hjust=0, size=4, nudge_y = 0.03) +
  theme(
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    panel.background = element_rect(fill = "white"),
    panel.grid = element_blank()
  )

skupine <- cutree(model1, k=3)

razvrstitev1 <- Najvecja_skupina_moski %>% 
  ggplot(aes(Leto, Stevilo, col = skupine)) +
  geom_point(size = 4) +
  theme_classic() +
  scale_x_continuous(breaks = seq(2011, 2020, 1))

# Pri zenskah je visjesolska

Najvecja_skupina_zenske <- Starost_Izobrazba_Spol %>% filter(Izobrazba == "VS_zenske")

data2 <- Najvecja_skupina_zenske %>% select(Leto, Stevilo) %>% as.matrix() %>% scale()
D2 <- dist(data2)
model2 <- hclust(D2)
ddata2 <- dendro_data(model2, type = "rectangle")

ddata2$labels$label<- paste(Najvecja_skupina_zenske$Starost[model2$order])
ddata2$labels$Starost <- Najvecja_skupina_zenske$Starost[model2$order]

dendogram2 <- ggplot(segment(ddata2)) +
  geom_segment(aes(x = x, y = y, xend = xend, yend = yend )) +
  coord_flip() +
  scale_y_reverse(expand = c(0.2,0)) +
  geom_text(
    data = ddata2$labels,
    aes(x=x, y=y, label=label, color = Starost), hjust=0, size=4, nudge_y = 0.03) +
  theme(
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    panel.background = element_rect(fill = "white"),
    panel.grid = element_blank()
  )

skupine <- cutree(model2, k=3)

razvrstitev2 <- Najvecja_skupina_zenske %>% 
  ggplot(aes(Leto, Stevilo, col = skupine)) +
  geom_point(size = 4) +
  theme_classic() +
  scale_x_continuous(breaks = seq(2011, 2020, 1))

# Napovedni model
# Za mosko populacijo

MA <- function(x, n) stats::filter(x, rep(1/n,n), sides = 1)

data3 <- Najvecja_skupina_moski %>% filter(Starost != "Stari")

napoved1 <- data3 %>%
  ggplot(aes(Leto, Stevilo))+
  geom_point(col = "green", size = 4) +
  stat_smooth(se = TRUE, size = 2) +
  geom_line(aes(Leto, MA(Stevilo, 3)), col = "red", size = 2) + 
  theme_classic() +
  scale_x_continuous(breaks = seq(2011, 2022, 1))

# Za zensko populacijo

data4 <- Najvecja_skupina_zenske %>% filter(Starost != "Stari")

napoved2 <- data4 %>%
  ggplot(aes(Leto, Stevilo))+
  geom_point(col = "green", size = 4) +
  stat_smooth(se = TRUE, size = 2) +
  geom_line(aes(Leto, MA(Stevilo, 3)), col = "red", size = 2) + 
  theme_classic() +
  scale_x_continuous(breaks = seq(2011, 2022, 1))
