# ---------------------------------------------------------------------------- # 

TTStemp = Tabela_Tujina_Starost %>%
  group_by(Drzava,Leto) %>%
  summarise(Stevilo = sum(Stevilo)) %>%
  filter(Leto == 2020)


# Line Chart
Line <- Tabela_Tujina_Starost %>%
  group_by(Drzava,Leto) %>%
  summarise(Stevilo = sum(Stevilo)) %>%
  ggplot(aes(Leto, Stevilo, col  = Drzava)) +
  geom_line(size = 1.5, alpha = 1) + 
  geom_point(size = 3, alpha = 1) +
  scale_x_continuous(breaks = seq(2011, 2020, 1)) +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "black", linetype = "dotted"),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.background = element_rect(color = "grey")) +
  geom_text_repel(aes(label = Drzava), data = TTStemp, size = 3)


# ---------------------------------------------------------------------------- #

SlopeChart <- function(Tabela, Kategorija) {
  Tabela %>%
    filter(Starost == Kategorija) %>%
    filter(Leto %in% c(2011, 2020)) %>%
    ggplot(aes(Leto, Stevilo, col = Drzava)) +
    geom_line(size = 1.5, alpha = 0.5) + 
    geom_point(size = 3, alpha = 1) +
    theme_bw() +
    theme(legend.position = "none", panel.border = element_blank(), 
          axis.title.y = element_blank(), axis.text.y = element_blank(),
          panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(),
          axis.title.x  = element_blank(), panel.grid.major.x = element_blank(),
          axis.text.x.top = element_text(size=12), axis.ticks = element_blank(),
          plot.title  = element_text(size=14, face = "bold", hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5), legend.title = element_blank())+
    scale_x_continuous(breaks = seq(2011, 2020,9)) +
    scale_y_continuous(breaks = seq(0, 850,25)) +
    geom_label_repel(aes(label = paste0(Stevilo," - ", Drzava)), box.padding = 1)
}

Slope1 <- SlopeChart(Tabela_Tujina_Starost, "Mladi")
Slope2 <- SlopeChart(Tabela_Tujina_Starost, "Zreli")


# ---------------------------------------------------------------------------- # 


Pie11 <- Tabela_Tujina_Izobrazba_Spol %>%
  filter(Izobrazba == "Osnovnošolska ali manj") %>%
  filter(Leto %in% c(2011)) %>%
  group_by(Drzava) %>%
  summarise(Stevilo = sum(Stevilo)) %>%
  ggplot(aes(x = "", y = Stevilo, fill = Drzava)) +
  geom_bar(stat="identity", width=1, color="white") +
  ggtitle("LETO 2011") +
  coord_polar("y", start=0) +
  theme_bw() +
  theme(
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.title = element_blank()
  )

Pie21 <- Tabela_Tujina_Izobrazba_Spol %>%
  filter(Izobrazba == "Srednješolska") %>%
  filter(Leto %in% c(2011)) %>%
  group_by(Drzava) %>%
  summarise(Stevilo = sum(Stevilo)) %>%
  ggplot(aes(x = "", y = Stevilo, fill = Drzava)) +
  geom_bar(stat="identity", width=1, color="white") +
  ggtitle("LETO 2011") +
  coord_polar("y", start=0) +
  theme_bw() +
  theme(
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.title = element_blank())

Pie31 <- Tabela_Tujina_Izobrazba_Spol %>%
  filter(Izobrazba == "Višješolska, visokošolska") %>%
  filter(Leto %in% c(2011)) %>%
  group_by(Drzava) %>%
  summarise(Stevilo = sum(Stevilo)) %>%
  ggplot(aes(x = "", y = Stevilo, fill = Drzava)) +
  geom_bar(stat="identity", width=1, color="white") +
  ggtitle("LETO 2011") +
  coord_polar("y", start=0) +
  theme_bw() +
  theme(
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.title = element_blank())

Pie12 <- Tabela_Tujina_Izobrazba_Spol %>%
  filter(Izobrazba == "Osnovnošolska ali manj") %>%
  filter(Leto %in% c(2020)) %>%
  group_by(Drzava) %>%
  summarise(Stevilo = sum(Stevilo)) %>%
  ggplot(aes(x = "", y = Stevilo, fill = Drzava)) +
  ggtitle("LETO 2020") +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  theme_bw() +
  theme(
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.title = element_blank())

Pie22 <- Tabela_Tujina_Izobrazba_Spol %>%
  filter(Izobrazba == "Srednješolska") %>%
  filter(Leto %in% c(2020)) %>%
  group_by(Drzava) %>%
  summarise(Stevilo = sum(Stevilo)) %>%
  ggplot(aes(x = "", y = Stevilo, fill = Drzava)) +
  geom_bar(stat="identity", width=1, color="white") +
  ggtitle("LETO 2020") +
  coord_polar("y", start=0) +
  theme_bw() +
  theme(
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.title = element_blank())

Pie32 <- Tabela_Tujina_Izobrazba_Spol %>%
  filter(Izobrazba == "Višješolska, visokošolska") %>%
  filter(Leto %in% c(2020)) %>%
  group_by(Drzava) %>%
  summarise(Stevilo = sum(Stevilo)) %>%
  ggplot(aes(x = "", y = Stevilo, fill = Drzava)) +
  geom_bar(stat="identity", width=1, color="white") +
  ggtitle("LETO 2020") +
  coord_polar("y", start=0) +
  theme_bw() +
  theme(
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.title = element_blank())

# ---------------------------------------------------------------------------- #

Fill1 <- Tabela_Tujina_Izobrazba_Spol %>%
  filter(Spol == "Moški") %>%
  group_by(Drzava, Leto) %>%
  summarise(Stevilo = sum(Stevilo)) %>%
  ggplot(aes(Leto, Stevilo, fill = Drzava)) +
  geom_bar(stat = "identity", position = "fill") +
  labs(y = "Delez", x = "Leto") +
  scale_x_continuous(breaks = seq(2011, 2020, 1)) +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "black", linetype = "dotted"),
        legend.title = element_blank());

Fill2 <- Tabela_Tujina_Izobrazba_Spol %>%
  filter(Spol == "Ženske") %>%
  group_by(Drzava, Leto) %>%
  summarise(Stevilo = sum(Stevilo)) %>%
  ggplot(aes(Leto, Stevilo, fill = Drzava)) +
  geom_bar(stat = "identity", position = "fill") +
  labs(y = "Delez", x = "Leto") +
  scale_x_continuous(breaks = seq(2011, 2020, 1)) +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "black", linetype = "dotted"),
        legend.title = element_blank());

# ---------------------------------------------------------------------------- #

Evropa <- ggplot(Evropa, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = Stevilo), color = "black") +
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

# ---------------------------------------------------------------------------- #

Aktivnost <- Tabela_Tujina_Aktivnost_Spol %>%
  group_by(Aktivnost, Leto) %>%
  summarise(Stevilo = sum(Stevilo)) %>%
  ggplot(aes(Leto, Stevilo, fill = Aktivnost)) +
  geom_bar(stat='identity', position='dodge') +
  scale_x_continuous(breaks = seq(2011, 2021, 1)) +
  scale_y_continuous(breaks = seq(0, 10000, 500)) + 
  labs(x = "Leto", y = "Stevilo") +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "black", linetype = "dotted"),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.background = element_rect(color = "grey"));
