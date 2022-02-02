# 4. faza: Napredna analiza podatkov

# ---------------------------------------------------------------------------- #

data1 <- Izseljevanje_Moski_Starost %>% filter(Kategorija == "Mladi")
model1 = lm(Moski ~ Leto + I(Leto^2), data1)

napoved1 <- data.frame(
  Leto = 2011:2023,
  Drugo = predict(model1, data.frame(Leto = 2011:2023))
)

napoved1<- full_join(napoved1, data1)

ggplot(napoved1) +
  geom_point(aes(Leto, Moski), col = "black", size = 3, alpha= 0.75) +
  geom_line(aes(Leto, Drugo), col = "red", size = 1.5, alpha= 0.75) +
  scale_x_continuous(breaks = seq(2011, 2023, 2)) +
  scale_y_continuous(breaks = seq(0, 10000, 500)) + 
  labs(x = "LETO", y = "ŠTEVILO") +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "black", linetype = "dotted"),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.background = element_rect(color = "grey"));


# ---------------------------------------------------------------------------- #

data2 <- Izseljevanje_Moski_Starost %>% filter(Kategorija == "Zreli")
model2 = lm(Moski ~ Leto + I(Leto^2), data2)

napoved2 <- data.frame(
  Leto = 2011:2023,
  Drugo = predict(model2, data.frame(Leto = 2011:2023))
)

napoved2 <- full_join(napoved2, data2)

ggplot(napoved2) +
  geom_point(aes(Leto, Moski), col = "black", size = 3, alpha= 0.75) +
  geom_line(aes(Leto, Drugo), col = "red", size = 1.5, alpha= 0.75) +
  scale_x_continuous(breaks = seq(2011, 2023, 2)) +
  scale_y_continuous(breaks = seq(0, 10000, 500)) + 
  labs(x = "LETO", y = "ŠTEVILO") +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "black", linetype = "dotted"),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.background = element_rect(color = "grey"));

# ---------------------------------------------------------------------------- #

data3 <- Izseljevanje_Zenske_Starost %>% filter(Kategorija == "Mladi")
model3 = lm(Zenske ~ Leto + I(Leto^2), data3)

napoved3 <- data.frame(
  Leto = 2011:2023,
  Drugo = predict(model3, data.frame(Leto = 2011:2023))
)

napoved3<- full_join(napoved3, data3)

ggplot(napoved3) +
  geom_point(aes(Leto, Zenske), col = "black", size = 3, alpha= 0.75) +
  geom_line(aes(Leto, Drugo), col = "red", size = 1.5, alpha= 0.75) +
  scale_x_continuous(breaks = seq(2011, 2023, 2)) +
  scale_y_continuous(breaks = seq(0, 10000, 500)) + 
  labs(x = "LETO", y = "ŠTEVILO") +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "black", linetype = "dotted"),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.background = element_rect(color = "grey"));


# ---------------------------------------------------------------------------- #

data4 <- Izseljevanje_Zenske_Starost %>% filter(Kategorija == "Zreli")
model4 = lm(Zenske ~ Leto + I(Leto^2), data4)

napoved4 <- data.frame(
  Leto = 2011:2023,
  Drugo = predict(model4, data.frame(Leto = 2011:2023))
)

napoved4 <- full_join(napoved4, data4)

ggplot(napoved4) +
  geom_point(aes(Leto, Zenske), col = "black", size = 3, alpha= 0.75) +
  geom_line(aes(Leto, Drugo), col = "red", size = 1.5, alpha= 0.75) +
  scale_x_continuous(breaks = seq(2011, 2023, 2)) +
  scale_y_continuous(breaks = seq(0, 2500, 250)) + 
  labs(x = "LETO", y = "ŠTEVILO") +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "black", linetype = "dotted"),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.background = element_rect(color = "grey"));

# ---------------------------------------------------------------------------- #

starost_izobrazba_spol$skupajVS <- starost_izobrazba_spol$VS_moski + starost_izobrazba_spol$VS_zenske
starost_izobrazba_spol$skupajSS <- starost_izobrazba_spol$SS_moski + starost_izobrazba_spol$SS_zenske
starost_izobrazba_spol$skupajOS <- starost_izobrazba_spol$OS_moski + starost_izobrazba_spol$OS_zenske

# ---------------------------------------------------------------------------- #

data5 <- starost_izobrazba_spol %>% pivot_longer(cols = names(.)[-1:-2]) %>% filter(name == "skupajVS") %>% filter(Kategorija == "Mladi")
data6 <- starost_izobrazba_spol %>% pivot_longer(cols = names(.)[-1:-2]) %>% filter(name == "skupajVS") %>% filter(Kategorija == "Zreli")
data7 <- starost_izobrazba_spol %>% pivot_longer(cols = names(.)[-1:-2]) %>% filter(name == "skupajVS") %>% filter(Kategorija == "Stari")
data7$VS <- data5$value + data6$value + data7$value

model5 <- lm(VS ~ Leto, data7)

napoved5 <- data.frame(Leto = 2011 : 2023,
                       Drugo = predict(model5, data.frame(Leto = 2011:2023))
                       )

napoved5 <- full_join(napoved5, data7)

ggplot(napoved5) +
  geom_point(aes(Leto, VS), col = "black", size = 3, alpha= 0.75) +
  geom_line(aes(Leto, Drugo), col = "red", size = 1.5, alpha= 0.75) +
  scale_x_continuous(breaks = seq(2011, 2023, 2)) +
  scale_y_continuous(breaks = seq(0, 5000, 500)) + 
  labs(x = "LETO", y = "ŠTEVILO") +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "black", linetype = "dotted"),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.background = element_rect(color = "grey"));

# ---------------------------------------------------------------------------- #

data8 <- starost_izobrazba_spol %>% pivot_longer(cols = names(.)[-1:-2]) %>% filter(name == "skupajSS") %>% filter(Kategorija == "Mladi")
data9 <- starost_izobrazba_spol %>% pivot_longer(cols = names(.)[-1:-2]) %>% filter(name == "skupajSS") %>% filter(Kategorija == "Zreli")
data10 <- starost_izobrazba_spol %>% pivot_longer(cols = names(.)[-1:-2]) %>% filter(name == "skupajSS") %>% filter(Kategorija == "Stari")
data10$SS <- data8$value + data9$value + data10$value

model6 <- lm(SS ~ Leto, data10)

napoved6 <- data.frame(Leto = 2011 : 2023,
                       Drugo = predict(model6, data.frame(Leto = 2011:2023))
)

napoved6 <- full_join(napoved6, data10)

ggplot(napoved6) +
  geom_point(aes(Leto, SS), col = "black", size = 3, alpha= 0.75) +
  geom_line(aes(Leto, Drugo), col = "red", size = 1.5, alpha= 0.75) +
  scale_x_continuous(breaks = seq(2011, 2023, 2)) +
  scale_y_continuous(breaks = seq(0, 5000, 500)) + 
  labs(x = "LETO", y = "ŠTEVILO") +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "black", linetype = "dotted"),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.background = element_rect(color = "grey"));

# ---------------------------------------------------------------------------- #

data11 <- starost_izobrazba_spol %>% pivot_longer(cols = names(.)[-1:-2]) %>% filter(name == "skupajOS") %>% filter(Kategorija == "Mladi")
data12 <- starost_izobrazba_spol %>% pivot_longer(cols = names(.)[-1:-2]) %>% filter(name == "skupajOS") %>% filter(Kategorija == "Zreli")
data13 <- starost_izobrazba_spol %>% pivot_longer(cols = names(.)[-1:-2]) %>% filter(name == "skupajOS") %>% filter(Kategorija == "Stari")
data13$OS <- data11$value + data12$value + data13$value

model7 <- lm(OS ~ Leto, data13)

napoved7 <- data.frame(Leto = 2011 : 2023,
                       Drugo = predict(model7, data.frame(Leto = 2011:2023))
)

napoved7 <- full_join(napoved7, data13)

ggplot(napoved7) +
  geom_point(aes(Leto, OS), col = "black", size = 3, alpha= 0.75) +
  geom_line(aes(Leto, Drugo), col = "red", size = 1.5, alpha= 0.75) +
  scale_x_continuous(breaks = seq(2011, 2023, 2)) +
  scale_y_continuous(breaks = seq(0, 2000, 250)) + 
  labs(x = "LETO", y = "ŠTEVILO") +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "black", linetype = "dotted"),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.background = element_rect(color = "grey"));

