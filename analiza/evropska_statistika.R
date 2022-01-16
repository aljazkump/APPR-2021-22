library(tidyverse);
library(gridExtra);

starost <- read.csv("podatki\\starost_tujina.csv");
izobrazba <- read.csv("podatki\\Spol_Izobrazba_tujina.csv");
aktivnost <- read.csv("podatki\\aktivnost_tujina.csv");
EU <- read.csv("podatki\\starost_EU.csv");

# PODATKI ZA DRZAVE Z VECJIM PRISELJEVANJEM

BIH = data.frame(Leta = 2011 : 2020,
                 mladi = starost[1:10,2],
                 zreli = starost[11:20,2],
                 stari = starost[21:30,2],
                 moski_OS = izobrazba[1:10,2],
                 moski_SS = izobrazba[11:20,2],
                 moski_VS = izobrazba[21:30,2],
                 zenske_OS = izobrazba[1:10,18],
                 zenske_SS = izobrazba[11:20,18],
                 zenske_VS = izobrazba[21:30,18],
                 zaposleni = aktivnost[1:10,2],
                 nezaposleni = aktivnost[11:20,2]
                 );

SRB = data.frame(Leta = 2011 : 2020,
                 mladi = starost[1:10,6],
                 zreli = starost[11:20,6],
                 stari = starost[21:30,6],
                 moski_OS = izobrazba[1:10,6],
                 moski_SS = izobrazba[11:20,6],
                 moski_VS = izobrazba[21:30,6],
                 zenske_OS = izobrazba[1:10,22],
                 zenske_SS = izobrazba[11:20,22],
                 zenske_VS = izobrazba[21:30,22],
                 zaposleni = aktivnost[1:10,6],
                 nezaposleni = aktivnost[11:20,6]
                 );

AU = data.frame(Leta = 2011 : 2020,
                mladi = starost[1:10,7],
                zreli = starost[11:20,7],
                stari = starost[21:30,7],
                moski_OS = izobrazba[1:10,7],
                moski_SS = izobrazba[11:20,7],
                moski_VS = izobrazba[21:30,7],
                zenske_OS = izobrazba[1:10,23],
                zenske_SS = izobrazba[11:20,23],
                zenske_VS = izobrazba[21:30,23],
                zaposleni = aktivnost[1:10,7],
                nezaposleni = aktivnost[11:20,7]
                );


FRA = data.frame(Leta = 2011 : 2020,
                 mladi = starost[1:10,9],
                 zreli = starost[11:20,9],
                 stari = starost[21:30,9],
                 moski_OS = izobrazba[1:10,9],
                 moski_SS = izobrazba[11:20,9],
                 moski_VS = izobrazba[21:30,9],
                 zenske_OS = izobrazba[1:10,25],
                 zenske_SS = izobrazba[11:20,25],
                 zenske_VS = izobrazba[21:30,25],
                 zaposleni = aktivnost[1:10,9],
                 nezaposleni = aktivnost[11:20,9]
                 );

HRT = data.frame(Leta = 2011 : 2020,
                mladi = starost[1:10,10],
                zreli = starost[11:20,10],
                stari = starost[21:30,10],
                moski_OS = izobrazba[1:10,10],
                moski_SS = izobrazba[11:20,10],
                moski_VS = izobrazba[21:30,10],
                zenske_OS = izobrazba[1:10,26],
                zenske_SS = izobrazba[11:20,26],
                zenske_VS = izobrazba[21:30,26],
                zaposleni = aktivnost[1:10,10],
                nezaposleni = aktivnost[11:20,10]
                );

ITA = data.frame(Leta = 2011 : 2020,
                 mladi = starost[1:10,11],
                 zreli = starost[11:20,11],
                 stari = starost[21:30,11],
                 moski_OS = izobrazba[1:10,11],
                 moski_SS = izobrazba[11:20,11],
                 moski_VS = izobrazba[21:30,11],
                 zenske_OS = izobrazba[1:10,27],
                 zenske_SS = izobrazba[11:20,27],
                 zenske_VS = izobrazba[21:30,27],
                 zaposleni = aktivnost[1:10,11],
                 nezaposleni = aktivnost[11:20,11]
                 );

NEM = data.frame(Leta = 2011 : 2020,
                mladi = starost[1:10,12],
                zreli = starost[11:20,12],
                stari = starost[21:30,12],
                moski_OS = izobrazba[1:10,12],
                moski_SS = izobrazba[11:20,12],
                moski_VS = izobrazba[21:30,12],
                zenske_OS = izobrazba[1:10,28],
                zenske_SS = izobrazba[11:20,28],
                zenske_VS = izobrazba[21:30,28],
                zaposleni = aktivnost[1:10,12],
                nezaposleni = aktivnost[11:20,12]
                );


UK = data.frame(Leta = 2011 : 2020,
                mladi = starost[1:10,14],
                zreli = starost[11:20,14],
                stari = starost[21:30,14],
                moski_OS = izobrazba[1:10,14],
                moski_SS = izobrazba[11:20,14],
                moski_VS = izobrazba[21:30,14],
                zenske_OS = izobrazba[1:10,30],
                zenske_SS = izobrazba[11:20,30],
                zenske_VS = izobrazba[21:30,30],
                zaposleni = aktivnost[1:10,14],
                nezaposleni = aktivnost[11:20,14]
                );

CH = data.frame(Leta = 2011 : 2020,
                mladi = starost[1:10,16],
                zreli = starost[11:20,16],
                stari = starost[21:30,16],
                moski_OS = izobrazba[1:10,16],
                moski_SS = izobrazba[11:20,16],
                moski_VS = izobrazba[21:30,16],
                zenske_OS = izobrazba[1:10,32],
                zenske_SS = izobrazba[11:20,32],
                zenske_VS = izobrazba[21:30,32],
                zaposleni = aktivnost[1:10,16],
                nezaposleni = aktivnost[11:20,16]
                );



# VIZUALIZACIJA EVROPE

skupno_evropa = data.frame(
  region = EU[1:34, 1],
  stevilo = (EU[1:34, 12])**(1/(1.5))
);

mapdata <- left_join(map_data("world"), skupno_evropa, by="region");

mapdata_temp = mapdata %>% filter(!is.na(mapdata$stevilo));

ggplot(mapdata_temp, aes(x = long, y = lat, group = group)) +
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

plot1 <- ggplot(NEM) +
  geom_line(aes(Leta, mladi), color = "red", size = 1.5) +
  geom_point(aes(Leta, mladi), color = "red", size = 3, alpha = 0.5) +
  geom_line(aes(Leta, zreli), color = "green", size = 1.5) +
  geom_point(aes(Leta, zreli),color = "green", size = 3, alpha = 0.5) +
  geom_line(aes(Leta, stari), color = "blue", size = 1.5) +
  geom_point(aes(Leta, stari), color = "blue", size = 3, alpha = 0.5) +
  ggtitle("Nemcija") +
  scale_x_continuous(breaks = seq(2011, 2020, 1)) +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "black", linetype = "dotted"),
        legend.title = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank());

plot2 <- ggplot(CH) +
  geom_line(aes(Leta, mladi), color = "red", size = 1.5) +
  geom_point(aes(Leta, mladi), color = "red", size = 3, alpha = 0.5) +
  geom_line(aes(Leta, zreli), color = "green", size = 1.5) +
  geom_point(aes(Leta, zreli),color = "green", size = 3, alpha = 0.5) +
  geom_line(aes(Leta, stari), color = "blue", size = 1.5) +
  geom_point(aes(Leta, stari), color = "blue", size = 3, alpha = 0.5) +
  ggtitle("Svica") +
  scale_x_continuous(breaks = seq(2011, 2020, 1)) +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "black", linetype = "dotted"),
        legend.title = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank());

plot3 <- ggplot(UK) +
  geom_line(aes(Leta, mladi), color = "red", size = 1.5) +
  geom_point(aes(Leta, mladi), color = "red", size = 3, alpha = 0.5) +
  geom_line(aes(Leta, zreli), color = "green", size = 1.5) +
  geom_point(aes(Leta, zreli),color = "green", size = 3, alpha = 0.5) +
  geom_line(aes(Leta, stari), color = "blue", size = 1.5) +
  geom_point(aes(Leta, stari), color = "blue", size = 3, alpha = 0.5) +
  ggtitle("Anglija") +
  scale_x_continuous(breaks = seq(2011, 2020, 1)) +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "black", linetype = "dotted"),
        legend.title = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank());

plot4 <- ggplot(ITA) +
  geom_line(aes(Leta, mladi), color = "red", size = 1.5) +
  geom_point(aes(Leta, mladi), color = "red", size = 3, alpha = 0.5) +
  geom_line(aes(Leta, zreli), color = "green", size = 1.5) +
  geom_point(aes(Leta, zreli),color = "green", size = 3, alpha = 0.5) +
  geom_line(aes(Leta, stari), color = "blue", size = 1.5) +
  geom_point(aes(Leta, stari), color = "blue", size = 3, alpha = 0.5) +
  ggtitle("Italija") +
  scale_x_continuous(breaks = seq(2011, 2020, 1)) +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "black", linetype = "dotted"),
        legend.title = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank());

plot5 <- ggplot(HRT) +
  geom_line(aes(Leta, mladi), color = "red", size = 1.5) +
  geom_point(aes(Leta, mladi), color = "red", size = 3, alpha = 0.5) +
  geom_line(aes(Leta, zreli), color = "green", size = 1.5) +
  geom_point(aes(Leta, zreli),color = "green", size = 3, alpha = 0.5) +
  geom_line(aes(Leta, stari), color = "blue", size = 1.5) +
  geom_point(aes(Leta, stari), color = "blue", size = 3, alpha = 0.5) +
  ggtitle("Hrvaska") +
  scale_x_continuous(breaks = seq(2011, 2020, 1)) +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "black", linetype = "dotted"),
        legend.title = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank());

plot6 <- ggplot(AU) +
  geom_line(aes(Leta, mladi), color = "red", size = 1.5) +
  geom_point(aes(Leta, mladi), color = "red", size = 3, alpha = 0.5) +
  geom_line(aes(Leta, zreli), color = "green", size = 1.5) +
  geom_point(aes(Leta, zreli),color = "green", size = 3, alpha = 0.5) +
  geom_line(aes(Leta, stari), color = "blue", size = 1.5) +
  geom_point(aes(Leta, stari), color = "blue", size = 3, alpha = 0.5) +
  ggtitle("Avstrija") +
  scale_x_continuous(breaks = seq(2011, 2020, 1)) +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "black", linetype = "dotted"),
        legend.title = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank());


grid.arrange(plot1, plot2, plot3, plot4, plot5, plot6, ncol = 3, nrow = 2);

