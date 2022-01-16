library(tidyverse);
library(reshape);

Starost_Izobrazba = read.csv("Podatki\\Starost_Izobrazba_splosno.csv");
Starost_spol = read.csv("Podatki\\starost_spol.csv");
aktivnost = read.csv("Podatki\\aktivnost_splosno.csv");

razvrstitev_po_izobrazbi_spolu_starosti = data.frame(
  
  leto = 2011 : 2020,
  
  mladi_moski_OS = Starost_Izobrazba[1:10, 2],
  zreli_moski_OS = Starost_Izobrazba[1:10, 3],
  stari_moski_OS = Starost_Izobrazba[1:10, 4],
  
  mladi_moski_SS = Starost_Izobrazba[11:20, 2],
  zreli_moski_SS = Starost_Izobrazba[11:20, 3],
  stari_moski_SS = Starost_Izobrazba[11:20, 4],
  
  mladi_moski_VS = Starost_Izobrazba[21:30, 2],
  zreli_moski_VS = Starost_Izobrazba[21:30, 3],
  stari_moski_VS = Starost_Izobrazba[21:30, 4],
  
  mlade_zenske_OS = Starost_Izobrazba[31:40, 2],
  zrele_zenske_OS = Starost_Izobrazba[31:40, 3],
  stare_zenske_OS = Starost_Izobrazba[31:40, 4],
  
  mlade_zenske_SS = Starost_Izobrazba[41:50, 2],
  zrele_zenske_SS = Starost_Izobrazba[41:50, 3],
  stare_zenske_SS = Starost_Izobrazba[41:50, 4],
  
  mlade_zenske_VS = Starost_Izobrazba[51:60, 2],
  zrele_zenske_VS = Starost_Izobrazba[51:60, 3],
  stare_zenske_VS = Starost_Izobrazba[51:60, 4],
  
  mladi_moski = Starost_spol[1:10, 2],
  zreli_moski = Starost_spol[1:10, 3],
  stari_moski = Starost_spol[1:10, 4],
  
  mlade_zenske = Starost_spol[11:20, 2],
  zrele_zenske = Starost_spol[11:20, 3],
  stare_zenske = Starost_spol[11:20, 4],
  
  povprecna_starost_moskih = Starost_spol[1:10, 5],
  povprecna_starost_zensk = Starost_spol[11:20, 5],
  
  moski_skupaj = Starost_spol[1:10, 6],
  zenske_skupaj = Starost_spol[11:20, 6]
  
);



# IZSELJEVANJE MLADEGA IN ZRELEGA PREBIVALSTVA

mladi = razvrstitev_po_izobrazbi_spolu_starosti$mladi_moski +
  razvrstitev_po_izobrazbi_spolu_starosti$mlade_zenske;
zreli = razvrstitev_po_izobrazbi_spolu_starosti$zreli_moski +
  razvrstitev_po_izobrazbi_spolu_starosti$zrele_zenske;
leta = 2011 : 2020;

df1 <- data.frame(mladi, zreli, leta);
df2 <- melt(df1, id.vars = "leta");

ggplot(df2, aes(x=leta, y=value, fill=variable)) +
  geom_bar(stat='identity', position='dodge') +
  scale_x_continuous(breaks = seq(2011, 2020, 1)) +
  scale_y_continuous(breaks = seq(0, 7500, 500)) + 
  ggtitle("IZSELJEVANJE MLADEGA IN ZRELEGA PREBIVALSTVA") +
  labs(x = "LETA", y = "STEVILO") +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "black", linetype = "dotted"),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.background = element_rect(color = "grey"));



# POVPRECNA STAROST IZSELJEVANJA MOSKIH IN ZENSK

barve <- c(
  "Osnovna sola" = "blue",
  "Srednja sola" = "red",
  "Visja sola" = "green",
  "moski" = "blue",
  "zenske" = "red"
);

ggplot(razvrstitev_po_izobrazbi_spolu_starosti) +
  geom_line(aes(leto, povprecna_starost_moskih, group = 1, color = "moski"), size = 1.5) +
  geom_point(aes(leto, povprecna_starost_moskih, group = 1, color = "moski"), size = 5, alpha = 0.5) +
  geom_line(aes(leto, povprecna_starost_zensk, group = 1, color = "zenske"), size = 1.5) +
  geom_point(aes(leto, povprecna_starost_zensk, group = 1, color = "zenske"), size = 5, alpha = 0.5) +
  labs(title = "POVPRECNA STAROST IZSELJEVANJA", x = "Leto", y = "Starost") + 
  scale_x_continuous(breaks = seq(2011, 2020, 1)) + 
  scale_y_continuous(breaks = seq(30, 40, 1)) + 
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "black", linetype = "dotted"),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.background = element_rect(color = "grey"));



# IZSELJEVANJE MLADEGA MOSKEGA PREBIVALSTVA

ggplot(razvrstitev_po_izobrazbi_spolu_starosti) +
  geom_line(aes(leto, mladi_moski_OS, group = 1, color = "Osnovna sola"), size = 1.5) +
  geom_point(aes(leto, mladi_moski_OS, group = 1, color = "Osnovna sola"), size = 5, alpha = 0.5) +
  geom_line(aes(leto, mladi_moski_SS, group = 1, color = "Srednja sola"), size = 1.5) +
  geom_point(aes(leto, mladi_moski_SS, group = 1, color = "Srednja sola"), size = 5, alpha = 0.5) +
  geom_line(aes(leto, mladi_moski_VS, group = 1, color = "Visja sola"), size = 1.5) +
  geom_point(aes(leto, mladi_moski_VS, group = 1, color = "Visja sola"), size = 5, alpha = 0.5) +
  labs(title = "IZSELJEVANJE MLADEGA MOSKEGA PREBIVALSTVA", 
       y = "STEVILO", 
       x = "LETO", 
       barve = "Legend") +
  scale_x_continuous(breaks = seq(2011, 2020, 1)) + 
  scale_y_continuous(breaks = seq(100, 1300, 100)) +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "black", linetype = "dotted"),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.background = element_rect(color = "grey"));



# IZSELJEVANJE MLADEGA ZENSKEGA PREBIVALSTVA

ggplot(razvrstitev_po_izobrazbi_spolu_starosti) +
  geom_line(aes(leto, mlade_zenske_OS, group = 1, color = "Osnovna sola"), size = 1.5) +
  geom_point(aes(leto, mlade_zenske_OS, group = 1, color = "Osnovna sola"), size = 5, alpha = 0.5) +
  geom_line(aes(leto, mlade_zenske_SS, group = 1, color = "Srednja sola"), size = 1.5) +
  geom_point(aes(leto, mlade_zenske_SS, group = 1, color = "Srednja sola"), size = 5, alpha = 0.5) +
  geom_line(aes(leto, mlade_zenske_VS, group = 1, color = "Visja sola"), size = 1.5) +
  geom_point(aes(leto, mlade_zenske_VS, group = 1, color = "Visja sola"), size = 5, alpha = 0.5) +
  labs(title = "IZSELJEVANJE MLADEGA ZENSKEGA PREBIVALSTVA", 
       y = "STEVILO", 
       x = "LETO", 
       barve = "Legend") +
  scale_x_continuous(breaks = seq(2011, 2020, 1)) + 
  scale_y_continuous(breaks = seq(100, 1300, 100)) +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "black", linetype = "dotted"),
        legend.position = "bottom",
        legend.title = element_blank());



# IZSELJEVANJA ZRELEGA MOSKEGA PREBIVALSTVA

ggplot(razvrstitev_po_izobrazbi_spolu_starosti) +
  geom_line(aes(leto, zreli_moski_OS, group = 1, color = "Osnovna sola"), size = 1.5) +
  geom_point(aes(leto, zreli_moski_OS, group = 1, color = "Osnovna sola"), size = 5, alpha = 0.5) +
  geom_line(aes(leto, zreli_moski_SS, group = 1, color = "Srednja sola"), size = 1.5) +
  geom_point(aes(leto, zreli_moski_SS, group = 1, color = "Srednja sola"), size = 5, alpha = 0.5) +
  geom_line(aes(leto, zreli_moski_VS, group = 1, color = "Visja sola"), size = 1.5) +
  geom_point(aes(leto, zreli_moski_VS, group = 1, color = "Visja sola"), size = 5, alpha = 0.5) +
  labs(title = "IZSELJEVANJE ZRELEGA MOSKEGA PREBIVALSTVA", 
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

ggplot(razvrstitev_po_izobrazbi_spolu_starosti) +
  geom_line(aes(leto, zrele_zenske_OS, group = 1, color = "Osnovna sola"), size = 1.5) +
  geom_point(aes(leto, zrele_zenske_OS, group = 1, color = "Osnovna sola"), size = 5, alpha = 0.5) +
  geom_line(aes(leto, zrele_zenske_SS, group = 1, color = "Srednja sola"), size = 1.5) +
  geom_point(aes(leto, zrele_zenske_SS, group = 1, color = "Srednja sola"), size = 5, alpha = 0.5) +
  geom_line(aes(leto, zrele_zenske_VS, group = 1, color = "Visja sola"), size = 1.5) +
  geom_point(aes(leto, zrele_zenske_VS, group = 1, color = "Visja sola"), size = 5, alpha = 0.5) +
  labs(title = "IZSELJEVANJE ZRELEGA ZENSKEGA PREBIVALSTVA", 
       y = "STEVILO", 
       x = "LETO", 
       barve = "Legend") +
  scale_x_continuous(breaks = seq(2011, 2020, 1)) + 
  scale_y_continuous(breaks = seq(100, 1300, 100)) +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "black", linetype = "dotted"),
        legend.position = "bottom",
        legend.title = element_blank());



# IZSELJEVANJE MOSKEGA PREBIVALSTVA PO STAROSTI

temp = c(
  "mladi" = "green",
  "zreli" = "blue",
  "stari" = "red"
)

ggplot(razvrstitev_po_izobrazbi_spolu_starosti) + 
  geom_bar(aes(leto, zreli_moski, fill = "zreli", group = 1), stat = "identity") +
  geom_bar(aes(leto, mladi_moski, fill = "mladi", group = 1), stat = "identity") +
  geom_bar(aes(leto, stari_moski, fill = "stari", group = 1), stat = "identity") +
  labs(title = "IZSELJEVANJE MOSKEGA PREBIVALSTVA PO STAROSTI", 
       y = "STEVILO", 
       x = "LETO", 
       temp = "Legend") +
  scale_x_continuous(breaks = seq(2011, 2020, 1)) + 
  scale_x_continuous(breaks = seq(2011, 2020, 1)) +
  scale_y_continuous(breaks = seq(0, 6000, 500)) +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "black", linetype = "dotted"),
        legend.title = element_blank());



# IZSELJEVANJA ZENSKEGA PREBIVALSTVA PO STAROSTI

ggplot(razvrstitev_po_izobrazbi_spolu_starosti) + 
  geom_bar(aes(leto, mlade_zenske, fill = "mladi", group = 1), stat = "identity") +
  geom_bar(aes(leto, zrele_zenske, fill = "zreli", group = 1), stat = "identity") +
  geom_bar(aes(leto, stare_zenske, fill = "stari", group = 1), stat = "identity") +
  labs(title = "IZSELJEVANJA ZENSKEGA PREBIVALSTVA PO STAROSTI", 
       y = "STEVILO", 
       x = "LETO", 
       temp = "Legend") +
  scale_x_continuous(breaks = seq(2011, 2020, 1)) +
  scale_y_continuous(breaks = seq(0, 3500, 500)) +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "black", linetype = "dotted"),
        legend.title = element_blank());



# IZSELJEVANJA PREBIVALSTVA Z VISOKOSOLSKO IZOBRAZBO V  ZADNJEM DESETLETJU

df_VS = data.frame(
  
  mladi_moski = sum(Starost_Izobrazba[21:30, 2]),
  zreli_moski = sum(Starost_Izobrazba[21:30, 3]),
  stari_moski = sum(Starost_Izobrazba[21:30, 4]),
  
  mlade_zenske = sum(Starost_Izobrazba[51:60, 2]),
  zrele_zenske = sum(Starost_Izobrazba[51:60, 3]),
  stare_zenske = sum(Starost_Izobrazba[51:60, 4])
  
);

df_VS %>% pivot_longer(cols = names(.)) %>% 
  ggplot(aes(x = "", y = value, fill = name)) +
  geom_col(color = "black") +
  coord_polar("y", start = 0) +
  geom_text(aes(label = paste0(round(value/sum(df_VS[1, 1:6]) * 100), "%")),
             position = position_stack(vjust = 0.5)) +
  ggtitle("IZSELJEVANJA PREBIVALSTVA Z VISOKOSOLSKO IZOBRAZBO V DESETLETJU") +
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

df_SS = data.frame(
  
  mladi_moski = sum(Starost_Izobrazba[11:20, 2]),
  zreli_moski = sum(Starost_Izobrazba[11:20, 3]),
  stari_moski = sum(Starost_Izobrazba[11:20, 4]),
  
  mlade_zenske = sum(Starost_Izobrazba[41:50, 2]),
  zrele_zenske = sum(Starost_Izobrazba[41:50, 3]),
  stare_zenske = sum(Starost_Izobrazba[41:50, 4])
  
);

df_SS %>% pivot_longer(cols = names(.)) %>% 
  ggplot(aes(x = "", y = value, fill = name)) +
  geom_col(color = "black") +
  coord_polar("y", start = 0) +
  geom_text(aes(label = paste0(round(value/sum(df_SS[1, 1:6]) * 100), "%")),
            position = position_stack(vjust = 0.5)) +
  ggtitle("IZSELJEVANJA PREBIVALSTVA Z SREDNJOSOLSKO IZOBRAZBO V DESETLETJU") +
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

df_OS = data.frame(
  
  mladi_moski = sum(Starost_Izobrazba[1:10, 2]),
  zreli_moski = sum(Starost_Izobrazba[1:10, 3]),
  stari_moski = sum(Starost_Izobrazba[1:10, 4]),
  
  mlade_zenske = sum(Starost_Izobrazba[31:40, 2]),
  zrele_zenske = sum(Starost_Izobrazba[31:40, 3]),
  stare_zenske = sum(Starost_Izobrazba[31:40, 4])
  
);

df_OS %>% pivot_longer(cols = names(.)) %>% 
  ggplot(aes(x = "", y = value, fill = name)) +
  geom_col(color = "black") +
  coord_polar("y", start = 0) +
  geom_text(aes(label = paste0(round(value/sum(df_OS[1, 1:6]) * 100), "%")),
            position = position_stack(vjust = 0.5)) +
  ggtitle("IZSELJEVANJA PREBIVALSTVA Z OSNOVNOSOLSKO IZOBRAZBO V DESETLETJU") +
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



# RAZLIKA MED AKTIVNIM IN NEAKTIVNIM ZRELIM PREBIVALSTVOM

razlika_zreli = aktivnost$zreli_z - aktivnost$zreli_n;
razlika_zrele = aktivnost$zrele_z - aktivnost$zrele_n;
            
plot1 <- ggplot(aktivnost) +
  geom_line(aes(Leto, razlika_zrele + razlika_zreli), color = "green", size = 1.5) +
  geom_point(aes(Leto, razlika_zrele + razlika_zreli), color = "green", size = 3, alpha = 0.5) +
  scale_x_continuous(breaks = seq(2011, 2020, 1)) +
  ggtitle("RAZLIKA MED AKTIVNIM IN NEAKTIVNIM ZRELIM PREBIVALSTVOM") +
  labs(y = "RAZLIKA") +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "black", linetype = "dotted"),
        legend.position = "bottom",
        legend.title = element_blank());



# RAZLIKA MED AKTIVNIM IN NEAKTIVNIM MLADIM PREBIVALSTVOM

razlika_mladi = aktivnost$mladi_z - aktivnost$mladi_n;
razlika_mlade = aktivnost$mlade_z - aktivnost$mlade_n;

plot2 <- ggplot(aktivnost) +
  geom_line(aes(Leto, razlika_mladi + razlika_mlade), color = "red", size = 1.5) +
  geom_point(aes(Leto, razlika_mladi + razlika_mlade), color = "red", size = 3, alpha = 0.5) +
  scale_x_continuous(breaks = seq(2011, 2020, 1)) +
  ggtitle("RAZLIKA MED AKTIVNIM IN NEAKTIVNIM MLADIM PREBIVALSTVOM") +
  labs(y = "RAZLIKA") +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "black", linetype = "dotted"),
        legend.position = "bottom",
        legend.title = element_blank());

grid.arrange(plot1, plot2,ncol = 2, nrow = 1);


