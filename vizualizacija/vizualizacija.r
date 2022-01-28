# 3. faza: Vizualizacija podatkov

# ---------------------------------------------------------------------------- #
# -------------------------- SLOVENSKA STATISTIKA ---------------------------- #
# ---------------------------------------------------------------------------- #


# Za zacetek bomo z histogramom prikazali, katera starostna skupina prevladuje pri
# odseljevanju za vsak spol.


# IZSELJEVANJE MOSKEGA PREBIVALSTVA PO STAROSTI

starost_spol %>% select(Leto, Moski, Kategorija) %>%
  ggplot(aes(Leto, Moski, fill = Kategorija)) +
  geom_bar(stat = "identity", position = "fill") +
  labs(y = "STEVILO", x = "LETO") +
  scale_x_continuous(breaks = seq(2011, 2020, 1)) +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "black", linetype = "dotted"),
        legend.title = element_blank());


# IZSELJEVANJA ZENSKEGA PREBIVALSTVA PO STAROSTI

starost_spol %>% select(Leto, Zenske, Kategorija) %>%
  ggplot(aes(Leto, Zenske, fill = Kategorija)) +
  geom_bar(stat = "identity", position = "fill") +
  labs(y = "STEVILO", x = "LETO") +
  scale_x_continuous(breaks = seq(2011, 2020, 1)) +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "black", linetype = "dotted"),
        legend.title = element_blank());


# IZSELJEVANJE MLADEGA IN ZRELEGA PREBIVALSTVA

# Pri tem histogramu nas zanima predvsem kaksna je razlika med odseljevanjem 
# mladega in zrelega prebivalstva. To je predvsem pomembno ce se odseli vec mladih
# kot zrelih, saj je to zelo "negativen" trend.
 

starost_spol %>% filter(Kategorija %in% c("Mladi", "Zreli")) %>%
  ggplot(aes(Leto, Moski + Zenske, fill = Kategorija)) +
  geom_bar(stat='identity', position='dodge') +
  scale_x_continuous(breaks = seq(2011, 2021, 1)) +
  scale_y_continuous(breaks = seq(0, 10000, 500)) + 
  labs(x = "LETA", y = "STEVILA") +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "black", linetype = "dotted"),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.background = element_rect(color = "grey"));

# V naslednjem delu bomo analizirali vsako skupino glede na spol in izobrazbo
# izpustili bomo staro populacijo saj ni nobenega vidnega trenda
# zanima nas izbrazbena struktua odseljenega prebivalstva glede na spol in starost 


# IZSELJEVANJE MLADEGA MOSKEGA PREBIVALSTVA

starost_izobrazba_spol %>% filter(Kategorija == "Mladi") %>%
  ggplot() +
  geom_area(aes(Leto, SS_moski), size = 1.5, fill = "blue", alpha = 0.5, col = "black") + 
  geom_area(aes(Leto, VS_moski), size = 1.5, fill = "green", alpha = 0.6, col = "black") + 
  geom_area(aes(Leto, OS_moski), size = 1.5, fill = "red",alpha = 0.7, col = "black") + 
  labs( y = "STEVILO", x = "LETO") +
  scale_x_continuous(breaks = seq(2011, 2020, 1)) + 
  scale_y_continuous(breaks = seq(100, 1300, 100)) +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "black", linetype = "dotted"),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.background = element_rect(color = "grey"));


# IZSELJEVANJE MLADEGA ZENSKEGA PREBIVALSTVA

starost_izobrazba_spol %>% filter(Kategorija == "Mladi") %>%
  ggplot() +
  geom_area(aes(Leto, SS_zenske), size = 1.5, fill = "blue", alpha = 0.5, col = "black") + 
  geom_area(aes(Leto, VS_zenske), size = 1.5, fill = "green", alpha = 0.6, col = "black") + 
  geom_area(aes(Leto, OS_zenske), size = 1.5, fill = "red",alpha = 0.7, col = "black") + 
  labs( y = "STEVILO", x = "LETO") +
  scale_x_continuous(breaks = seq(2011, 2020, 1)) + 
  scale_y_continuous(breaks = seq(100, 1300, 100)) +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "black", linetype = "dotted"),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.background = element_rect(color = "grey"));


# IZSELJEVANJA ZRELEGA MOSKEGA PREBIVALSTVA

starost_izobrazba_spol %>% filter(Kategorija == "Zreli") %>%
  ggplot() +
  geom_area(aes(Leto, SS_moski), size = 1.5, fill = "blue", alpha = 0.5, col = "black") + 
  geom_area(aes(Leto, VS_moski), size = 1.5, fill = "green", alpha = 0.6, col = "black") + 
  geom_area(aes(Leto, OS_moski), size = 1.5, fill = "red",alpha = 0.7, col = "black") + 
  labs( y = "STEVILO", x = "LETO") +
  scale_x_continuous(breaks = seq(2011, 2020, 1)) + 
  scale_y_continuous(breaks = seq(100, 1300, 100)) +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "black", linetype = "dotted"),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.background = element_rect(color = "grey"));



# IZSELJEVANJA ZRELEGA ZENSKEGA PREBIVALSTVA

starost_izobrazba_spol %>% filter(Kategorija == "Zreli") %>%
  ggplot() +
  geom_area(aes(Leto, SS_zenske), size = 1.5, fill = "blue", alpha = 0.5, col = "black") + 
  geom_area(aes(Leto, VS_zenske), size = 1.5, fill = "green", alpha = 0.6, col = "black") + 
  geom_area(aes(Leto, OS_zenske), size = 1.5, fill = "red",alpha = 0.7, col = "black") + 
  labs( y = "STEVILO", x = "LETO") +
  scale_x_continuous(breaks = seq(2011, 2020, 1)) + 
  scale_y_continuous(breaks = seq(100, 1300, 100)) +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "black", linetype = "dotted"),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.background = element_rect(color = "grey"));


# V naslednje delu bomo na prvo mesto postavili izobrazbo in zeleli ugotoviti
# katera skupina in starosti in spolu prevladuje v vsaki izobrazbeni skupini.
# Rezultat bo prikazan s tortnim diagramom.


# IZSELJEVANJA PREBIVALSTVA Z VISOKOSOLSKO IZOBRAZBO V  ZADNJEM DESETLETJU

starost_izobrazba_spol %>% select(Leto, Kategorija,VS_moski, VS_zenske)



starost_izobrazba_spol %>% filter(Leto %in% c(2011, 2020)) %>%
  pivot_longer(cols = names(.)[-1:-2]) %>% arrange(name) %>%
  filter(name %in% c("VS_moski", "VS_zenske")) %>%
  unite(izobrazbe, c(name, Kategorija)) %>%
  ggplot(aes(Leto, value, group = izobrazbe)) +
  geom_line(aes(color = izobrazbe), size = 2 , alpha = 0.5) +
  geom_point(aes(color = izobrazbe), size = 4, alpha = 0.5) +
  theme_bw() +
  theme(legend.position = "bottom", panel.border = element_blank(), 
        axis.title.y = element_blank(), axis.text.y = element_blank(),
        panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(),
        axis.title.x  = element_blank(), panel.grid.major.x = element_blank(),
        axis.text.x.top = element_text(size=12), axis.ticks = element_blank(),
        plot.title  = element_text(size=14, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5), legend.title = element_blank())+
  scale_x_continuous(breaks = seq(2011, 2020,9)) +
  geom_text(aes(label = paste0(value)), fontface = "bold", size = 4) +
  ggtitle("VISOKOSOLSKA IZOBRAZBA")


# IZSELJEVANJA PREBIVALSTVA Z SREDNJOSOLSKO IZOBRAZBO V ZADNJEM DESETLETJU

starost_izobrazba_spol %>% filter(Leto %in% c(2011, 2020)) %>%
  pivot_longer(cols = names(.)[-1:-2]) %>% arrange(name) %>%
  filter(name %in% c("SS_moski", "SS_zenske")) %>%
  unite(izobrazbe, c(name, Kategorija)) %>%
  ggplot(aes(Leto, value, group = izobrazbe)) +
  geom_line(aes(color = izobrazbe), size = 2 , alpha = 0.5) +
  geom_point(aes(color = izobrazbe), size = 4, alpha = 0.5) +
  theme_bw() +
  theme(legend.position = "bottom", panel.border = element_blank(), 
        axis.title.y = element_blank(), axis.text.y = element_blank(),
        panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(),
        axis.title.x  = element_blank(), panel.grid.major.x = element_blank(),
        axis.text.x.top = element_text(size=12), axis.ticks = element_blank(),
        plot.title  = element_text(size=14, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5), legend.title = element_blank())+
  scale_x_continuous(breaks = seq(2011, 2020,9)) +
  geom_text(aes(label = paste0(value)), fontface = "bold", size = 4) +
  ggtitle("SREDNJESOLSKA IZOBRAZBA")


# IZSELJEVANJA PREBIVALSTVA Z OSNOVNOSOLSKO IZOBRAZBO V DESETLETJU

starost_izobrazba_spol %>% filter(Leto %in% c(2011, 2020)) %>%
  pivot_longer(cols = names(.)[-1:-2]) %>% arrange(name) %>%
  filter(name %in% c("OS_moski", "OS_zenske")) %>%
  unite(izobrazbe, c(name, Kategorija)) %>%
  ggplot(aes(Leto, value, group = izobrazbe)) +
  geom_line(aes(color = izobrazbe), size = 2 , alpha = 0.5) +
  geom_point(aes(color = izobrazbe), size = 4, alpha = 0.5) +
  theme_bw() +
  theme(legend.position = "bottom", panel.border = element_blank(), 
        axis.title.y = element_blank(), axis.text.y = element_blank(),
        panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(),
        axis.title.x  = element_blank(), panel.grid.major.x = element_blank(),
        axis.text.x.top = element_text(size=12), axis.ticks = element_blank(),
        plot.title  = element_text(size=14, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5), legend.title = element_blank())+
  scale_x_continuous(breaks = seq(2011, 2020,9)) +
  geom_text(aes(label = paste0(value)), fontface = "bold", size = 4) +
  ggtitle("OSNOVNOSOLSKA IZOBRAZBA")

# Na koncu slovenske statistike pa se sledi razlika po aktivnosti. Zanima nas
# ali se izseli vec ljudi, ki so ze delovno aktivni in iscejo boljso placo ali
# zato ker ne dobijo sluzbe. Med neaktivne lahko stejemo tudi studente, ki takoj
# po narejeni univerzi delajo v tujini. Locili bomo med mladimi in starimi.


# ZAPOSLENO PREBIVALSTVO

aktivnost %>% filter(STATUS == "Zaposleno") %>%
  pivot_longer(cols = names(.)[-1:-2]) %>% arrange(name) %>%
  ggplot() + geom_line(aes(Leto, value, color = name), size = 1.5, alpha = 0.75) +
  geom_point(aes(Leto, value, color = name), size = 4, alpha = 0.5) +
  labs( y = "STEVILO", x = "LETO") +
  scale_x_continuous(breaks = seq(2011, 2020, 1)) + 
  scale_y_continuous(breaks = seq(100, 1100, 100)) +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "black", linetype = "dotted"),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.background = element_rect(color = "grey"));

# NEZAPOSLENO PREBIVALSTVO

aktivnost %>% filter(STATUS == "Nezaposleno") %>%
  pivot_longer(cols = names(.)[-1:-2]) %>% arrange(name) %>%
  ggplot() + geom_line(aes(Leto, value, color = name), size = 1.5, alpha = 0.75) +
  geom_point(aes(Leto, value, color = name), size = 4, alpha = 0.5) +
  labs( y = "STEVILO", x = "LETO") +
  scale_x_continuous(breaks = seq(2011, 2020, 1)) + 
  scale_y_continuous(breaks = seq(0, 200, 25)) +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "black", linetype = "dotted"),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.background = element_rect(color = "grey"));


# ---------------------------------------------------------------------------- #
# -------------------------- EVROPSKA STATISTIKA ----------------------------- #
# ---------------------------------------------------------------------------- #


# VIZUALIZACIJA EVROPE

# Z zemljevidom Evrope bomo prikazali v katere drzave se izseli najvec Slovencev


mapdata = left_join(map_data("world"), evropa, by="region") %>%
  filter(!is.na(mapdata$SKUPNO));

ggplot(mapdata, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = stevilo), color = "black", show.legend = FALSE) + 
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


# PRISELJEVANJE RAZLICNIH STAROSTNIH SKUPIN V DRZAVE Z NAJVECJIM PRISELJEVANJEM

# Na podlagi zgornjega zamljevida smo izbrali 6 drzav, katere se izseljuje najvec
# Slovencev. To so Nemcija, Italija, Hrvaska, Svica, Zdruzeno Kraljestvo in Avstrija.
# Za vsako drzavo bomo prikazali starostno sestavo odseljevanja.
# Zelena <- mladi, Rdeca <- zreli, Modra <- stari


starost_evropa %>% pivot_longer(cols = names(.)[-1:-2]) %>% arrange(name) %>%
  ggplot() + geom_line(aes(Leto, value, col = name), size = 1.5, alpha = 0.75) +
  geom_point(aes(Leto, value, col = name), size = 3, alpha = 0.5) +
  facet_wrap(. ~ DRZAVE , ncol = 3) +
  scale_x_continuous(breaks = seq(2011, 2021, 2)) +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "black", linetype = "dotted"),
        legend.title = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank());


# ---------------------------------------------------------------------------- #
# ---------------------- SLOVENSKA OBCINSKA STATISTIKA ----------------------- #
# ---------------------------------------------------------------------------- #

# V zadnjem delu bomo se prikazali delez odseljenih glede na stevilo, prebivalstva
# za vsako obcino. Tako bomo videli katera regija se najbolj odseljuje.
# nimam .SHP file za statisticen regije :(

# VIZUALIZACIJA SLOVENSKIH OBCIN

# Traja nekaj sekund, da se nalozi 

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



