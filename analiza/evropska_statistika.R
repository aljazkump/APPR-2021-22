library(tidyverse);
library(gridExtra);

# setwd()

starost <- read.csv("podatki/starost_tujina.csv");
izobrazba <- read.csv("podatki/Spol_Izobrazba_tujina.csv");
EU <- read.csv("podatki/starost_EU.csv");

# PODATKI ZA DRZAVE Z VECJIM PRISELJEVANJEM

AU = data.frame(Leta = 2011 : 2020,
                mladi = starost[1:10,7],
                zreli = starost[11:20,7],
                stari = starost[21:30,7]
);

HRT = data.frame(Leta = 2011 : 2020,
                 mladi = starost[1:10,10],
                 zreli = starost[11:20,10],
                 stari = starost[21:30,10]
);

ITA = data.frame(Leta = 2011 : 2020,
                 mladi = starost[1:10,11],
                 zreli = starost[11:20,11],
                 stari = starost[21:30,11]
);

NEM = data.frame(Leta = 2011 : 2020,
                 mladi = starost[1:10,12],
                 zreli = starost[11:20,12],
                 stari = starost[21:30,12]
);


UK = data.frame(Leta = 2011 : 2020,
                mladi = starost[1:10,14],
                zreli = starost[11:20,14],
                stari = starost[21:30,14]
);

CH = data.frame(Leta = 2011 : 2020,
                mladi = starost[1:10,16],
                zreli = starost[11:20,16],
                stari = starost[21:30,16]
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


# NAPOVED ZA LETO 2021 Z METODO NAJMANJSIH KVADRATOV

A <- rbind(c(0,0,1),
           c(1,1,1),
           c(4,2,1));

B_NEM_mladi <- c(starost$Nemèija[8], starost$Nemèija[9], starost$Nemèija[10]);
B_NEM_zreli <- c(starost$Nemèija[18], starost$Nemèija[19], starost$Nemèija[20]);
B_NEM_stari <- c(starost$Nemèija[28], starost$Nemèija[29], starost$Nemèija[30]);

B_CH_mladi <- c(starost$Svica[8], starost$Svica[9], starost$Svica[10]);
B_CH_zreli <- c(starost$Svica[18], starost$Svica[19], starost$Svica[20]);
B_CH_stari <- c(starost$Svica[28], starost$Svica[29], starost$Svica[30]);

B_UK_mladi <- c(starost$Zdruzeno.kraljestvo[8], starost$Zdruzeno.kraljestvo[9], starost$Zdruzeno.kraljestvo[10]);
B_UK_zreli <- c(starost$Zdruzeno.kraljestvo[18], starost$Zdruzeno.kraljestvo[19], starost$Zdruzeno.kraljestvo[20]);
B_UK_stari <- c(starost$Zdruzeno.kraljestvo[28], starost$Zdruzeno.kraljestvo[29], starost$Zdruzeno.kraljestvo[30]);

B_ITA_mladi <- c(starost$Italija[8], starost$Italija[9], starost$Italija[10]);
B_ITA_zreli <- c(starost$Italija[18], starost$Italija[19], starost$Italija[20]);
B_ITA_stari <- c(starost$Italija[28], starost$Italija[29], starost$Italija[30]);

B_HRT_mladi <- c(starost$Hrvaska[8], starost$Hrvaska[9], starost$Hrvaska[10]);
B_HRT_zreli <- c(starost$Hrvaska[18], starost$Hrvaska[19], starost$Hrvaska[20]);
B_HRT_stari <- c(starost$Hrvaska[28], starost$Hrvaska[29], starost$Hrvaska[30]);

B_AU_mladi <- c(starost$Avstrija[8], starost$Avstrija[9], starost$Avstrija[10]);
B_AU_zreli <- c(starost$Avstrija[18], starost$Avstrija[19], starost$Avstrija[20]);
B_AU_stari <- c(starost$Avstrija[28], starost$Avstrija[29], starost$Avstrija[30]);

y = c(9,3,1);

x1 = solve(A,B_NEM_mladi);
x2 = solve(A,B_NEM_zreli);
x3 = solve(A,B_NEM_stari);

x4 = solve(A,B_CH_mladi);
x5 = solve(A,B_CH_zreli);
x6 = solve(A,B_CH_stari);

x7 = solve(A,B_UK_mladi);
x8 = solve(A,B_UK_zreli);
x9 = solve(A,B_UK_stari);

x10 = solve(A,B_ITA_mladi);
x11 = solve(A,B_ITA_zreli);
x12 = solve(A,B_ITA_stari);

x13 = solve(A,B_HRT_mladi);
x14 = solve(A,B_HRT_zreli);
x15 = solve(A,B_HRT_stari);

x16 = solve(A,B_AU_mladi);
x17 = solve(A,B_AU_zreli);
x18 = solve(A,B_AU_stari);


r1 = sum(x1*y);
r2 = sum(x2*y);
r3 = sum(x3*y);

df1 = data.frame(Leta = 2021, mladi = r1, zreli = r2, stari = r3);
DF1 = rbind(NEM, df1);

r4 = sum(x4*y);
r5 = sum(x5*y);
r6 = sum(x6*y);

df2 = data.frame(Leta = 2021, mladi = r4, zreli = r5, stari = r6);
DF2 = rbind(CH, df2);

r7 = sum(x7*y);
r8 = sum(x8*y);
r9 = sum(x9*y);

df3 = data.frame(Leta = 2021, mladi = r7, zreli = r8, stari = r9);
DF3 = rbind(UK, df3);

r10 = sum(x10*y);
r11 = sum(x11*y);
r12 = sum(x12*y);

df4 = data.frame(Leta = 2021, mladi = r10, zreli = r11, stari = r12);
DF4 = rbind(ITA, df4);

r13 = sum(x13*y);
r14 = sum(x14*y);
r15 = sum(x15*y);

df5 = data.frame(Leta = 2021, mladi = r13, zreli = r14, stari = r15);
DF5 = rbind(HRT, df5);

r16 = sum(x16*y);
r17 = sum(x17*y);
r18 = sum(x18*y);

df6 = data.frame(Leta = 2021, mladi = r16, zreli = r17, stari = r18);
DF6 = rbind(AU, df6);


# PRISELJEVANJE RAZLICNIH STAROSTNIH SKUPIN V DRZAVE Z NAJVECJIM PRISELJEVANJEM

plot1 <- ggplot(DF1) +
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

plot2 <- ggplot(DF2) +
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

plot3 <- ggplot(DF3) +
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

plot4 <- ggplot(DF4) +
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

plot5 <- ggplot(DF5) +
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

plot6 <- ggplot(DF6) +
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


grid.arrange(plot1, plot2, plot3, plot4, plot5, plot6, ncol = 3, nrow = 2);



