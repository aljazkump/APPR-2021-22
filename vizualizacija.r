# 3. faza: Vizualizacija podatkov

# ---------------------------------------------------------------------------- #
# -------------------------- SLOVENSKA STATISTIKA ---------------------------- #
# ---------------------------------------------------------------------------- #

barve <- c(
  "Osnovna sola" = "red",
  "Srednja sola" = "green",
  "Visja sola" = "blue",
  "moski" = "blue",
  "zenske" = "red"
);

temp = c(
  "mladi" = "green",
  "zreli" = "blue",
  "stari" = "red"
);


# Za zacetek bomo z histogramom prikazali, katera starostna skupina prevladuje pri
# odseljevanju za vsak spol. 


# IZSELJEVANJE MOSKEGA PREBIVALSTVA PO STAROSTI

Moski_starost <- ggplot(DF_STAROST_SPOL) + 
  geom_bar(aes(leto, zreli_moski, fill = "zreli", group = 1), stat = "identity") +
  geom_bar(aes(leto, mladi_moski, fill = "mladi", group = 1), stat = "identity") +
  geom_bar(aes(leto, stari_moski, fill = "stari", group = 1), stat = "identity") +
  labs(y = "STEVILO", 
       x = "LETO", 
       temp = "Legend") +
  scale_x_continuous(breaks = seq(2011, 2021, 1)) +
  scale_y_continuous(breaks = seq(0, 8000, 500)) +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "black", linetype = "dotted"),
        legend.title = element_blank());



# IZSELJEVANJA ZENSKEGA PREBIVALSTVA PO STAROSTI

Zenske_starost <- ggplot(DF_STAROST_SPOL) + 
  geom_bar(aes(leto, mlade_zenske, fill = "mladi", group = 1), stat = "identity") +
  geom_bar(aes(leto, zrele_zenske, fill = "zreli", group = 1), stat = "identity") +
  geom_bar(aes(leto, stare_zenske, fill = "stari", group = 1), stat = "identity") +
  labs(y = "STEVILO", 
       x = "LETO", 
       temp = "Legend") +
  scale_x_continuous(breaks = seq(2011, 2021, 1)) +
  scale_y_continuous(breaks = seq(0, 4000, 500)) +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "black", linetype = "dotted"),
        legend.title = element_blank());


# IZSELJEVANJE MLADEGA IN ZRELEGA PREBIVALSTVA

# Pri tem histogramu nas zanima predvsem kaksna je razlika med odseljevanjem 
# mladega in zrelega prebivalstva. To je predvsem pomembno ce se odseli vec mladih
# kot zrelih, saj je to zelo "negativen" trend.
 

mladi = DF_STAROST_SPOL$mladi_moski + DF_STAROST_SPOL$mlade_zenske;
zreli = DF_STAROST_SPOL$zreli_moski + DF_STAROST_SPOL$zrele_zenske;
leta = 2011 : 2021;

df1 <- data.frame(mladi, zreli, leta);
df2 <- melt(df1, id.vars = "leta");

Mladi_Zreli <- ggplot(df2, aes(x=leta, y=value, fill=variable)) +
  geom_bar(stat='identity', position='dodge') +
  scale_x_continuous(breaks = seq(2011, 2021, 1)) +
  scale_y_continuous(breaks = seq(0, 10000, 500)) + 
  labs(x = "LETA", y = "STEVILA") +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "black", linetype = "dotted"),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.background = element_rect(color = "grey"));


# POVPRECNA STAROST IZSELJEVANJA MOSKIH IN ZENSK

# Pri tem grafu nas zanima, kdaj se v povprecju moski ali zenske odlocajo za
# odselitev in ali je med njima razlika, kateri prej, kateri kaseje.


Povprecje <- ggplot(DF_STAROST_SPOL) +
  geom_line(aes(leto, povprecna_starost_moskih, group = 1, color = "moski"), size = 1.5) +
  geom_point(aes(leto, povprecna_starost_moskih, group = 1, color = "moski"), size = 5, alpha = 0.5) +
  geom_line(aes(leto, povprecna_starost_zensk, group = 1, color = "zenske"), size = 1.5) +
  geom_point(aes(leto, povprecna_starost_zensk, group = 1, color = "zenske"), size = 5, alpha = 0.5) +
  labs(x = "LETO", y = "STAROST") + 
  scale_x_continuous(breaks = seq(2011, 2021, 1)) + 
  scale_y_continuous(breaks = seq(30, 40, 1)) + 
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "black", linetype = "dotted"),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.background = element_rect(color = "grey"));


# V naslednjem delu bomo analizirali vsako skupino glede na spol in izobrazbo
# izpustili bomo staro populacijo saj ni nobenega vidnega trenda
# zanima nas izbrazbena struktua odseljenega prebivalstva glede na spol in starost 


# IZSELJEVANJE MLADEGA MOSKEGA PREBIVALSTVA

Mladi_moski <- ggplot(razvrstitev_po_izobrazbi_spolu_starosti) +
  geom_line(aes(leto, mladi_moski_OS, group = 1, color = "Osnovna sola"), size = 1.5) +
  geom_point(aes(leto, mladi_moski_OS, group = 1, color = "Osnovna sola"), size = 5, alpha = 0.5) +
  geom_line(aes(leto, mladi_moski_SS, group = 1, color = "Srednja sola"), size = 1.5) +
  geom_point(aes(leto, mladi_moski_SS, group = 1, color = "Srednja sola"), size = 5, alpha = 0.5) +
  geom_line(aes(leto, mladi_moski_VS, group = 1, color = "Visja sola"), size = 1.5) +
  geom_point(aes(leto, mladi_moski_VS, group = 1, color = "Visja sola"), size = 5, alpha = 0.5) +
  labs( y = "Stevilo", 
       x = "LETO", 
       title = "MOSKI",
       barve = "Legend") +
  scale_x_continuous(breaks = seq(2011, 2020, 1)) + 
  scale_y_continuous(breaks = seq(100, 1300, 100)) +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "black", linetype = "dotted"),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.background = element_rect(color = "grey"));


# IZSELJEVANJE MLADEGA ZENSKEGA PREBIVALSTVA

Mlade_zenske <- ggplot(razvrstitev_po_izobrazbi_spolu_starosti) +
  geom_line(aes(leto, mlade_zenske_OS, group = 1, color = "Osnovna sola"), size = 1.5) +
  geom_point(aes(leto, mlade_zenske_OS, group = 1, color = "Osnovna sola"), size = 5, alpha = 0.5) +
  geom_line(aes(leto, mlade_zenske_SS, group = 1, color = "Srednja sola"), size = 1.5) +
  geom_point(aes(leto, mlade_zenske_SS, group = 1, color = "Srednja sola"), size = 5, alpha = 0.5) +
  geom_line(aes(leto, mlade_zenske_VS, group = 1, color = "Visja sola"), size = 1.5) +
  geom_point(aes(leto, mlade_zenske_VS, group = 1, color = "Visja sola"), size = 5, alpha = 0.5) +
  labs(y = "STEVILO", 
       x = "LETO", 
       title = "ZENSKE",
       barve = "Legend") +
  scale_x_continuous(breaks = seq(2011, 2020, 1)) + 
  scale_y_continuous(breaks = seq(100, 1300, 100)) +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "black", linetype = "dotted"),
        legend.position = "bottom",
        legend.title = element_blank());


# IZSELJEVANJA ZRELEGA MOSKEGA PREBIVALSTVA

Zreli_moski <- ggplot(razvrstitev_po_izobrazbi_spolu_starosti) +
  geom_line(aes(leto, zreli_moski_OS, group = 1, color = "Osnovna sola"), size = 1.5) +
  geom_point(aes(leto, zreli_moski_OS, group = 1, color = "Osnovna sola"), size = 5, alpha = 0.5) +
  geom_line(aes(leto, zreli_moski_SS, group = 1, color = "Srednja sola"), size = 1.5) +
  geom_point(aes(leto, zreli_moski_SS, group = 1, color = "Srednja sola"), size = 5, alpha = 0.5) +
  geom_line(aes(leto, zreli_moski_VS, group = 1, color = "Visja sola"), size = 1.5) +
  geom_point(aes(leto, zreli_moski_VS, group = 1, color = "Visja sola"), size = 5, alpha = 0.5) +
  labs(title = " MOSKI", 
       y = "STEVILO", 
       x = "LETO", 
       barve = "Legend") +
  scale_x_continuous(breaks = seq(2011, 2020, 1)) + 
  scale_y_continuous(breaks = seq(100, 1300, 100)) +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "black", linetype = "dotted"),
        legend.position = "bottom",
        legend.title = element_blank());


# IZSELJEVANJA ZRELEGA ZENSKEGA PREBIVALSTVA

Zrele_zenske <- ggplot(razvrstitev_po_izobrazbi_spolu_starosti) +
  geom_line(aes(leto, zrele_zenske_OS, group = 1, color = "Osnovna sola"), size = 1.5) +
  geom_point(aes(leto, zrele_zenske_OS, group = 1, color = "Osnovna sola"), size = 5, alpha = 0.5) +
  geom_line(aes(leto, zrele_zenske_SS, group = 1, color = "Srednja sola"), size = 1.5) +
  geom_point(aes(leto, zrele_zenske_SS, group = 1, color = "Srednja sola"), size = 5, alpha = 0.5) +
  geom_line(aes(leto, zrele_zenske_VS, group = 1, color = "Visja sola"), size = 1.5) +
  geom_point(aes(leto, zrele_zenske_VS, group = 1, color = "Visja sola"), size = 5, alpha = 0.5) +
  labs(title = "ZENSKE", 
       y = "STEVILO", 
       x = "LETO", 
       barve = "Legend") +
  scale_x_continuous(breaks = seq(2011, 2020, 1)) + 
  scale_y_continuous(breaks = seq(100, 1300, 100)) +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "black", linetype = "dotted"),
        legend.position = "bottom",
        legend.title = element_blank());


# V naslednje delu bomo na prvo mesto postavili izobrazbo in zeleli ugotoviti
# katera skupina in starosti in spolu prevladuje v vsaki izobrazbeni skupini.
# Rezultat bo prikazan s tortnim diagramom.


# IZSELJEVANJA PREBIVALSTVA Z VISOKOSOLSKO IZOBRAZBO V  ZADNJEM DESETLETJU

VS <- df_VS %>% pivot_longer(cols = names(.)) %>% 
  ggplot(aes(x = "", y = value, fill = name)) +
  geom_col(color = "black") +
  coord_polar("y", start = 0) +
  geom_text(aes(label = paste0(round(value/sum(df_VS[1, 1:6]) * 100), "%")),
            position = position_stack(vjust = 0.5)) +
  ggtitle("VISOKOSOLSKA IZOBRAZBA") +
  theme(
    panel.background = element_blank(),
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 13),
    panel.border = element_blank(),
    panel.grid = element_blank()
  );


# IZSELJEVANJA PREBIVALSTVA Z SREDNJOSOLSKO IZOBRAZBO V ZADNJEM DESETLETJU

SS <- df_SS %>% pivot_longer(cols = names(.)) %>% 
  ggplot(aes(x = "", y = value, fill = name)) +
  geom_col(color = "black") +
  coord_polar("y", start = 0) +
  geom_text(aes(label = paste0(round(value/sum(df_SS[1, 1:6]) * 100), "%")),
            position = position_stack(vjust = 0.5)) +
  ggtitle("SREDNJOSOLSKA IZOBRAZBA") +
  theme(
    panel.background = element_blank(),
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 13),
    panel.border = element_blank(),
    panel.grid = element_blank()
  );


# IZSELJEVANJA PREBIVALSTVA Z OSNOVNOSOLSKO IZOBRAZBO V DESETLETJU

OS <- df_OS %>% pivot_longer(cols = names(.)) %>% 
  ggplot(aes(x = "", y = value, fill = name)) +
  geom_col(color = "black") +
  coord_polar("y", start = 0) +
  geom_text(aes(label = paste0(round(value/sum(df_OS[1, 1:6]) * 100), "%")),
            position = position_stack(vjust = 0.5)) +
  ggtitle("OSNOVNOSOLSKA IZOBRAZBA") +
  theme(
    panel.background = element_blank(),
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 13),
    panel.border = element_blank(),
    panel.grid = element_blank()
  );


# Na koncu slovenske statistike pa se sledi razlika po aktivnosti. Zanima nas
# ali se izseli vec ljudi, ki so ze delovno aktivni in iscejo boljso placo ali
# zato ker ne dobijo sluzbe. Med neaktivne lahko stejemo tudi studente, ki takoj
# po narejeni univerzi delajo v tujini. Locili bomo med mladimi in starimi.


# RAZLIKA MED AKTIVNIM IN NEAKTIVNIM ZRELIM PREBIVALSTVOM

razlika_zreli = razvrstitev_po_aktivnosti_spolu$zreli_zaposleni - 
  razvrstitev_po_aktivnosti_spolu$zreli_nezaposleni;

razlika_zrele = razvrstitev_po_aktivnosti_spolu$zrele_zaposlene - 
  razvrstitev_po_aktivnosti_spolu$zrele_nezaposlene;

plot1 <- ggplot(aktivnost) +
  geom_line(aes(Leto, razlika_zrele + razlika_zreli), color = "green", size = 1.5) +
  geom_point(aes(Leto, razlika_zrele + razlika_zreli), color = "green", size = 3, alpha = 0.5) +
  scale_x_continuous(breaks = seq(2011, 2020, 1)) +
  ggtitle("RAZLIKA MED AKTIVNIM IN NEAKTIVNIM ZRELIM PREBIVALSTVOM") +
  labs(y = "RAZLIKA", x = "LETO") +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "black", linetype = "dotted"),
        legend.position = "bottom",
        legend.title = element_blank());

# RAZLIKA MED AKTIVNIM IN NEAKTIVNIM MLADIM PREBIVALSTVOM

razlika_mladi = razvrstitev_po_aktivnosti_spolu$mladi_zaposleni - 
  razvrstitev_po_aktivnosti_spolu$mladi_nezaposleni;

razlika_mlade = razvrstitev_po_aktivnosti_spolu$mlade_zaposlene - 
  razvrstitev_po_aktivnosti_spolu$mlade_nezaposlene;

plot2 <- ggplot(aktivnost) +
  geom_line(aes(Leto, razlika_mladi + razlika_mlade), color = "red", size = 1.5) +
  geom_point(aes(Leto, razlika_mladi + razlika_mlade), color = "red", size = 3, alpha = 0.5) +
  scale_x_continuous(breaks = seq(2011, 2020, 1)) +
  ggtitle("RAZLIKA MED AKTIVNIM IN NEAKTIVNIM MLADIM PREBIVALSTVOM") +
  labs(y = "RAZLIKA", x = "LETO") +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "black", linetype = "dotted"),
        legend.position = "bottom",
        legend.title = element_blank());

# ZN <- grid.arrange(plot1, plot2,ncol = 2, nrow = 1);


# ---------------------------------------------------------------------------- #
# -------------------------- EVROPSKA STATISTIKA ----------------------------- #
# ---------------------------------------------------------------------------- #


# VIZUALIZACIJA EVROPE

# Z zemljevidom Evrope bomo prikazali v katere drzave se izseli najvec Slovencev

mapdata <- left_join(map_data("world"), skupno_evropa, by="region");

mapdata_temp = mapdata %>% filter(!is.na(mapdata$stevilo));

Evropa_map <- ggplot(mapdata_temp, aes(x = long, y = lat, group = group)) +
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


plot3 <- ggplot(DF_NEM) +
  geom_line(aes(Leta, mladi), color = "red", size = 1.5) +
  geom_point(aes(Leta, mladi), color = "red", size = 3, alpha = 0.5) +
  geom_line(aes(Leta, zreli), color = "green", size = 1.5) +
  geom_point(aes(Leta, zreli),color = "green", size = 3, alpha = 0.5) +
  geom_line(aes(Leta, stari), color = "blue", size = 1.5) +
  geom_point(aes(Leta, stari), color = "blue", size = 3, alpha = 0.5) +
  ggtitle("Nemcija") +
  scale_x_continuous(breaks = seq(2011, 2021, 1)) +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "black", linetype = "dotted"),
        legend.title = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank());

plot4 <- ggplot(DF_CH) +
  geom_line(aes(Leta, mladi), color = "red", size = 1.5) +
  geom_point(aes(Leta, mladi), color = "red", size = 3, alpha = 0.5) +
  geom_line(aes(Leta, zreli), color = "green", size = 1.5) +
  geom_point(aes(Leta, zreli),color = "green", size = 3, alpha = 0.5) +
  geom_line(aes(Leta, stari), color = "blue", size = 1.5) +
  geom_point(aes(Leta, stari), color = "blue", size = 3, alpha = 0.5) +
  ggtitle("Svica") +
  scale_x_continuous(breaks = seq(2011, 2021, 1)) +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "black", linetype = "dotted"),
        legend.title = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank());

plot5 <- ggplot(DF_UK) +
  geom_line(aes(Leta, mladi), color = "red", size = 1.5) +
  geom_point(aes(Leta, mladi), color = "red", size = 3, alpha = 0.5) +
  geom_line(aes(Leta, zreli), color = "green", size = 1.5) +
  geom_point(aes(Leta, zreli),color = "green", size = 3, alpha = 0.5) +
  geom_line(aes(Leta, stari), color = "blue", size = 1.5) +
  geom_point(aes(Leta, stari), color = "blue", size = 3, alpha = 0.5) +
  ggtitle("Velika Britanija") +
  scale_x_continuous(breaks = seq(2011, 2021, 1)) +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "black", linetype = "dotted"),
        legend.title = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank());

plot6 <- ggplot(DF_ITA) +
  geom_line(aes(Leta, mladi), color = "red", size = 1.5) +
  geom_point(aes(Leta, mladi), color = "red", size = 3, alpha = 0.5) +
  geom_line(aes(Leta, zreli), color = "green", size = 1.5) +
  geom_point(aes(Leta, zreli),color = "green", size = 3, alpha = 0.5) +
  geom_line(aes(Leta, stari), color = "blue", size = 1.5) +
  geom_point(aes(Leta, stari), color = "blue", size = 3, alpha = 0.5) +
  ggtitle("Italija") +
  scale_x_continuous(breaks = seq(2011, 2021, 1)) +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "black", linetype = "dotted"),
        legend.title = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank());

plot7 <- ggplot(DF_HRT) +
  geom_line(aes(Leta, mladi), color = "red", size = 1.5) +
  geom_point(aes(Leta, mladi), color = "red", size = 3, alpha = 0.5) +
  geom_line(aes(Leta, zreli), color = "green", size = 1.5) +
  geom_point(aes(Leta, zreli),color = "green", size = 3, alpha = 0.5) +
  geom_line(aes(Leta, stari), color = "blue", size = 1.5) +
  geom_point(aes(Leta, stari), color = "blue", size = 3, alpha = 0.5) +
  ggtitle("Hrvaska") +
  scale_x_continuous(breaks = seq(2011, 2021, 1)) +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "black", linetype = "dotted"),
        legend.title = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank());

plot8 <- ggplot(DF_AU) +
  geom_line(aes(Leta, mladi), color = "red", size = 1.5) +
  geom_point(aes(Leta, mladi), color = "red", size = 3, alpha = 0.5) +
  geom_line(aes(Leta, zreli), color = "green", size = 1.5) +
  geom_point(aes(Leta, zreli),color = "green", size = 3, alpha = 0.5) +
  geom_line(aes(Leta, stari), color = "blue", size = 1.5) +
  geom_point(aes(Leta, stari), color = "blue", size = 3, alpha = 0.5) +
  ggtitle("Avstrija") +
  scale_x_continuous(breaks = seq(2011, 2021, 1)) +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "black", linetype = "dotted"),
        legend.title = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank());


# Drzave <- grid.arrange(plot3, plot4, plot5, plot6, plot7, plot8, ncol = 3, nrow = 2);


# ---------------------------------------------------------------------------- #
# ---------------------- SLOVENSKA OBCINSKA STATISTIKA ----------------------- #
# ---------------------------------------------------------------------------- #

# V zadnjem delu bomo se prikazali delez odseljenih glede na stevilo, prebivalstva
# za vsako obcino. Tako bomo videli katera regija se najbolj odseljuje.
# nimam .SHP file za statisticen regije :(

# VIZUALIZACIJA SLOVENSKIH OBCIN

# Traja nekaj sekund, da se nalozi 

Obcine <- SIob_fort %>%
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

