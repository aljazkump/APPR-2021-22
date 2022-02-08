# 3. faza: Vizualizacija podatkov

# ---------------------------------------------------------------------------- #
# -------------------------- SLOVENSKA STATISTIKA ---------------------------- #
# ---------------------------------------------------------------------------- #


# Za zacetek bomo z histogramom prikazali, katera starostna skupina prevladuje pri
# odseljevanju za vsak spol.


# IZSELJEVANJE MOSKEGA PREBIVALSTVA PO STAROSTI

Izseljevanje_Moski_Starost_Graf <- ggplot(Starost_Spol %>% filter(Spol == "Moski"), aes(Leto, Stevilo, fill = Starost)) +
geom_bar(stat = "identity", position = "fill") +
labs(y = "Delez", x = "Leto") +
ggtitle("IZSELJEVANJE MOSKEGA PREBIVALSTVA : Delezi starostnih skupin") +
scale_x_continuous(breaks = seq(2011, 2020, 1)) +
theme(panel.background = element_rect(fill = "white"),
      panel.grid.major = element_line(color = "black", linetype = "dotted"),
      legend.title = element_blank());


# IZSELJEVANJA ZENSKEGA PREBIVALSTVA PO STAROSTI

Izseljevanje_Zenske_Starost_Graf <- ggplot(Starost_Spol %>% filter(Spol == "Zenske"), aes(Leto, Stevilo, fill = Starost)) +
geom_bar(stat = "identity", position = "fill") +
labs(y = "Delez", x = "Leto") +
ggtitle("IZSELJEVANJE ZENSKEGA PREBIVALSTVA : Delezi starostnih skupin") +
scale_x_continuous(breaks = seq(2011, 2020, 1)) +
theme(panel.background = element_rect(fill = "white"),
      panel.grid.major = element_line(color = "black", linetype = "dotted"),
      legend.title = element_blank());


# IZSELJEVANJE MLADEGA IN ZRELEGA PREBIVALSTVA

# Pri tem histogramu nas zanima predvsem kaksna je razlika med odseljevanjem 
# mladega in zrelega prebivalstva. To je predvsem pomembno ce se odseli vec mladih
# kot zrelih, saj je to zelo "negativen" trend.
 
Izseljevanje_Mladi_Zreli_Graf <- Starost_Spol %>%
filter(Starost != "Stari") %>% unite(Kategorija, c(Spol, Starost)) %>%
ggplot(aes(Leto, Stevilo, fill = Kategorija)) +
geom_bar(stat='identity', position='dodge') +
scale_x_continuous(breaks = seq(2011, 2021, 1)) +
scale_y_continuous(breaks = seq(0, 10000, 500)) + 
ggtitle("IZSELJEVANJE MLADEGA IN ZRELEGA PREBIVALSTVA ZA OBA SPOLA") +
labs(x = "Leto", y = "Stevilo") +
scale_fill_discrete(labels = c("Mladi", "Zreli", "Mlade", "Zrele")) +
theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "black", linetype = "dotted"),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.background = element_rect(color = "grey"));

# V naslednjem delu bomo analizirali vsako skupino glede na spol in izobrazbo
# izpustili bomo staro populacijo saj ni nobenega vidnega trenda
# zanima nas izbrazbena struktua odseljenega prebivalstva glede na spol in starost

graf_area <- function(starost, spol, naslov){
  Starost_Izobrazba_Spol %>% filter(Starost == starost, grepl(spol, Izobrazba)) %>%
  ggplot() +
    geom_area(aes(Leto, Stevilo, fill = Izobrazba), position = "fill",alpha = 0.8, color = "black") + 
    labs(x = "Leto", y = "Delez") +
    coord_flip() +
    scale_x_continuous(breaks = seq(2011, 2020, 1)) +
    scale_fill_discrete(labels = c("OS", "SS", "VS")) +
    ggtitle(naslov) + 
    theme(panel.background = element_rect(fill = "white"),
          panel.grid.major = element_line(color = "black", linetype = "dotted"),
          legend.position = "bottom",
          legend.title = element_blank(),
          legend.background = element_rect(color = "grey"));  
}

# Starost_Izobrazba_Spol %>% filter(Starost == "Mladi", grepl("moski", Izobrazba)) %>% ggplot() + geom_area(aes(Leto, Stevilo, fill = Izobrazba), position = "fill",alpha = 0.8, color = "black")

Izseljevanje_Mladi_Moski_Graf <- graf_area("Mladi", "moski", "IZSELJEVANJE MLADEGA MOSKEGA PREBIVALSTVA") 
Izseljevanje_Mlade_Zenske_Graf <- graf_area("Mladi", "zenske", "IZSELJEVANJE MLADEGA ZENSKEGA PREBIVALSTVA") 
Izseljevanje_Zreli_Moski_Graf <- graf_area("Zreli", "moski", "IZSELJEVANJA ZRELEGA MOSKEGA PREBIVALSTVA")
Izseljevanje_Zrele_Zenske_Graf <- graf_area("Zreli", "zenske", "IZSELJEVANJA ZRELEGA ZENSKEGA PREBIVALSTVA")


# V naslednje delu bomo na prvo mesto postavili izobrazbo in zeleli ugotoviti
# katera skupina in starosti in spolu prevladuje v vsaki izobrazbeni skupini.
# Rezultat bo prikazan s tortnim diagramom.


# IZSELJEVANJA PREBIVALSTVA Z VISOKOSOLSKO IZOBRAZBO V  ZADNJEM DESETLETJU

graf <- function(izobrazba1, izobrazba2, naslov){
  Starost_Izobrazba_Spol %>% filter(Leto %in% c(2011, 2020)) %>%
    filter(Izobrazba  %in% c(izobrazba1, izobrazba2)) %>%
    unite(Kategorija, c(Izobrazba , Starost )) %>%
    ggplot(aes(Leto, Stevilo, group = Kategorija)) +
    geom_line(aes(color = Kategorija), size = 2 , alpha = 0.5) +
    geom_point(aes(color = Kategorija), size = 4, alpha = 0.5) +
    theme_bw() +
    theme(legend.position = "bottom", panel.border = element_blank(), 
          axis.title.y = element_blank(), axis.text.y = element_blank(),
          panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(),
          axis.title.x  = element_blank(), panel.grid.major.x = element_blank(),
          axis.text.x.top = element_text(size=12), axis.ticks = element_blank(),
          plot.title  = element_text(size=14, face = "bold", hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5), legend.title = element_blank())+
    scale_x_continuous(breaks = seq(2011, 2020,9)) +
    geom_text(aes(label = paste0(Stevilo)), fontface = "bold", size = 4) +
    ggtitle(naslov)
}



VS_Graf <- graf("VS_moski", "VS_zenske", "VISOKOŠOLSKA IZOBRAZBA") # IZSELJEVANJA PREBIVALSTVA Z VISJEŠOLSKO IZOBRAZBO V DESETLETJU
SS_Graf <- graf("SS_moski", "SS_zenske", "SREDNJEŠOLSKA IZOBRAZBA") # IZSELJEVANJA PREBIVALSTVA Z SREDNJOŠOLSKO IZOBRAZBO V ZADNJEM DESETLETJU
OS_Graf <- graf("OS_moski", "OS_zenske", "OSNOVNOŠOLSKA IZOBRAZBA") # IZSELJEVANJA PREBIVALSTVA Z OSNOVNOŠOLSKO IZOBRAZBO V DESETLETJU


# Na koncu slovenske statistike pa se sledi razlika po aktivnosti. Zanima nas
# ali se izseli vec ljudi, ki so ze delovno aktivni in iscejo boljso placo ali
# zato ker ne dobijo sluzbe. Med neaktivne lahko stejemo tudi studente, ki takoj
# po narejeni univerzi delajo v tujini. Locili bomo med mladimi in starimi.


# ZAPOSLENO PREBIVALSTVO

Aktivni_Graf <- Aktivnost %>%
  filter(Aktivnost == "Zaposleni") %>%
  ggplot() +
  geom_line(aes(Leto, Stevilo, color = Starost ), size = 1.5, alpha = 0.75) +
  geom_point(aes(Leto, Stevilo, color = Starost ), size = 4, alpha = 0.5) +
  ggtitle("ODSELJEVANJE ZAPOSLENEGA PREBIVALSTVA") +
  scale_x_continuous(breaks = seq(2011, 2020, 1)) + 
  scale_y_continuous(breaks = seq(100, 1100, 100)) +
  theme(panel.background = element_rect(fill = "white"),
          panel.grid.major = element_line(color = "black", linetype = "dotted"),
          legend.position = "bottom",
          legend.title = element_blank(),
          legend.background = element_rect(color = "white"));

# NEZAPOSLENO PREBIVALSTVO

Neaktivni_Graf <- Aktivnost %>% 
  filter(Aktivnost == "Nezaposleni") %>%
  ggplot() +
  geom_line(aes(Leto, Stevilo, color = Starost ), size = 1.5, alpha = 0.75) +
  geom_point(aes(Leto, Stevilo, color = Starost ), size = 4, alpha = 0.5) +
  ggtitle("ODSELJEVANJE NEZAPOSLENEGA PREBIVALSTVA") +
  scale_x_continuous(breaks = seq(2011, 2020, 1)) + 
  scale_y_continuous(breaks = seq(50, 200, 50)) +
    theme(panel.background = element_rect(fill = "white"),
          panel.grid.major = element_line(color = "black", linetype = "dotted"),
          legend.position = "bottom",
          legend.title = element_blank(),
          legend.background = element_rect(color = "white"));


# ---------------------------------------------------------------------------- #
# -------------------------- EVROPSKA STATISTIKA ----------------------------- #
# ---------------------------------------------------------------------------- #


# VIZUALIZACIJA EVROPE

# Z zemljevidom Evrope bomo prikazali v katere drzave se izseli najvec Slovencev


Evropa_map <- ggplot(mapdata, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = SKUPNO), color = "black") + 
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


Drzave_Graf <- Starost_Evropa %>% 
  ggplot() + geom_line(aes(Leto, Stevilo, col = Starost), size = 1.5, alpha = 0.75) +
  geom_point(aes(Leto, Stevilo, col = Starost), size = 3, alpha = 0.5) +
  facet_wrap(. ~ Drzava, ncol = 3) +
  scale_x_continuous(breaks = seq(2011, 2021, 2)) +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "black", linetype = "dotted"),
        legend.title = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank());






