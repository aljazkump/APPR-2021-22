library(tidyverse)

# Change first row data as column names in R stack overflow
header.true <- function(df) {
  names(df) <- as.character(unlist(df[1,]))
  df[-1,]
}

Tujina_Starost = read.csv(
  file = 'Podatki/Tujina_Starost.csv',
  header = FALSE,
  skip = 1
)

Tujina_Starost$V1 = NULL
Tujina_Starost[2:17, 1] = sub("\\....", "", Tujina_Starost[2:17, 1])
Tujina_Starost1 = header.true(Tujina_Starost)

Tabela_Tujina_Starost = pivot_longer(
  Tujina_Starost1,
  cols = colnames(Tujina_Starost1)[-1],
  names_to = "Leto.Starost",
  values_to = "Stevilo"
) %>% separate(
  col = Leto.Starost,
  into = c("Leto", "Starost"),
  sep = " "
)

Tabela_Tujina_Starost$`Starost`[Tabela_Tujina_Starost$`Starost` %in% c("0-14","15-19", "20-24", "25-29", "30-34")] = "Mladi"
Tabela_Tujina_Starost$`Starost`[Tabela_Tujina_Starost$`Starost` %in% c("35-39", "40-44", "45-49", "50-54", "55-59", "60-64")] = "Zreli"
Tabela_Tujina_Starost$`Starost`[Tabela_Tujina_Starost$`Starost` %in% c("65")] = "Stari"

colnames(Tabela_Tujina_Starost)[1] = "Drzava"

Tabela_Tujina_Starost$Stevilo = as.numeric(Tabela_Tujina_Starost$Stevilo)
Tabela_Tujina_Starost$Leto = as.numeric(Tabela_Tujina_Starost$Leto)

Tabela_Tujina_Starost = Tabela_Tujina_Starost %>%
  group_by(Drzava,Leto, Starost) %>%
  summarise(Stevilo = sum(Stevilo))

# ---------------------------------------------------------------------------- # 

Tujina_Izobrazba_Spol = read.csv(
  file = "Podatki/Tujina_Izobrazba_Spol.csv",
  header = FALSE,
  skip =  1,
  locale = locale(encoding = "Windows-1250")
)

Tujina_Izobrazba_Spol[2:33, 2] = sub("\\....", "", Tujina_Izobrazba_Spol[2:33, 2])
Tujina_Izobrazba_Spol1 = header.true(Tujina_Izobrazba_Spol)

Tabela1 = pivot_longer(
  Tujina_Izobrazba_Spol1,
  cols = colnames(Tujina_Izobrazba_Spol1)[3:32],
  names_to = "Leto.Drzavljani.Drzava.Izobrazba",
  values_to = "Število"
) %>% separate(
  col = Leto.Drzavljani.Drzava.Izobrazba,
  into = c("Leto","Drzavljani","Drzava","Izobrazba"),
  sep = " "
)

Tabela_Tujina_Izobrazba_Spol = Tabela1[, -which(names(Tabela1) %in% c("Drzavljani", "Drzava"))]
colnames(Tabela_Tujina_Izobrazba_Spol)[1] = "Spol"
colnames(Tabela_Tujina_Izobrazba_Spol)[2] = "Drzava"
colnames(Tabela_Tujina_Izobrazba_Spol)[5] = "Stevilo"

Tabela_Tujina_Izobrazba_Spol$Stevilo = as.numeric(Tabela_Tujina_Izobrazba_Spol$Stevilo)
Tabela_Tujina_Izobrazba_Spol$Leto = as.numeric(Tabela_Tujina_Izobrazba_Spol$Leto)

# ---------------------------------------------------------------------------- # 

Tujina_Aktivnost_Spol = read.csv(
  file = "Podatki/Tujina_Aktivnost_Spol.csv",
  header = FALSE,
  skip = 1,
  locale = locale(encoding = "Windows-1250")
)

Tujina_Aktivnost_Spol[2:33, 2] = sub("\\....", "", Tujina_Aktivnost_Spol[2:33, 2])
Tujina_Aktivnost_Spol1 = header.true(Tujina_Aktivnost_Spol)

Tabela2 = pivot_longer(
  Tujina_Aktivnost_Spol1,
  cols = colnames(Tujina_Aktivnost_Spol1)[3:22],
  names_to = "Leto.Drzavljani.Drzava.Izobrazba",
  values_to = "Število"
) %>% separate(
  col = Leto.Drzavljani.Drzava.Izobrazba,
  into = c("Leto","Drzavljani","Drzava","Izobrazba"),
  sep = " "
)

Tabela_Tujina_Aktivnost_Spol = Tabela2[, -which(names(Tabela2) %in% c("Drzavljani", "Drzava"))]
colnames(Tabela_Tujina_Aktivnost_Spol)[1] = "Spol"
colnames(Tabela_Tujina_Aktivnost_Spol)[2] = "Drzava"
colnames(Tabela_Tujina_Aktivnost_Spol)[5] = "Stevilo"

Tabela_Tujina_Aktivnost_Spol$Stevilo = as.numeric(Tabela_Tujina_Aktivnost_Spol$Stevilo)
Tabela_Tujina_Aktivnost_Spol$Leto = as.numeric(Tabela_Tujina_Aktivnost_Spol$Leto)

