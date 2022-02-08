# 2. faza: Uvoz podatkov

valueForPath <- function(row, path) { 
  row %>% html_node(xpath = path) %>% html_text()
}

# Tabela, ki prikazuje stevilke odseljevanja v Evropske drzave 
# glede na starost(mladi, zreli, stari)

Starost_Evropa <- read.csv(
  "podatki/starost_tujina.csv"
)

Starost_Evropa <- Starost_Evropa[, c(1,7, 10,11,12,14,16)] %>%
  mutate(Starost = rep(c("Mladi", "Zreli", "Stari"), 
                       each = length(unique(Leto)))) %>%
  pivot_longer(-c("Leto", "Starost"), names_to = "Drzava", values_to = "Stevilo")

# Tabela, ki prikazuje stevilke odseljevanja v Evropske drzave 
# v tem primeru skoraj za vsako drzavo Evrope                                        

Evropa <- read.csv(
  "podatki/starost_EU.csv",
  fileEncoding = "Windows-1250"
)

evropa <- Evropa %>% select(X, Skupno)
colnames(evropa) <- c("region", "SKUPNO")

mapdata = left_join(map_data("world"), evropa, by="region") %>%
  filter(!is.na(SKUPNO));


# Tabela, ki prikazuje stevilke odseljevanja iz Slovenije 
# glede na izobrazbo, spol in starost 


Starost_Izobrazba_Spol <- "podatki/starost_izobrazba_splosno.xml" %>%
  read_xml(encoding = "Windows-1250") %>% html_nodes(xpath = "/root/row")

Starost_Izobrazba_Spol <- tibble(
  Leto = as.integer(Starost_Izobrazba_Spol %>% sapply(function(row) {valueForPath(row, "Leto")})),
  Mladi = as.integer(Starost_Izobrazba_Spol %>% sapply(function(row) {valueForPath(row, "Mladi")})),
  Zreli = as.integer(Starost_Izobrazba_Spol %>% sapply(function(row) {valueForPath(row, "Zreli")})),
  Stari = as.integer(Starost_Izobrazba_Spol %>% sapply(function(row) {valueForPath(row, "Stari")})),
) 

Starost_Izobrazba_Spol <- Starost_Izobrazba_Spol %>%
  mutate(Izobrazba = rep(c("OS_moski", "SS_moski", "VS_moski", "OS_zenske", "SS_zenske", "VS_zenske"), 
  each = length(unique(Leto)))) %>%
  pivot_longer(-c("Leto", "Izobrazba"), names_to = "Starost", values_to = "Stevilo")


# Tabela, ki prikazuje stevilke odseljevanja iz Slovenije 
# glede na starost in spol    

Starost_Spol <- "podatki/starost_spol.xml" %>%
  read_xml(encoding = "Windows-1250") %>% html_nodes(xpath = "/root/row")

Starost_Spol <- tibble(
  Leto = as.integer(Starost_Spol %>% sapply(function(row) {valueForPath(row, "Leto")})),
  Mladi = as.integer(Starost_Spol %>% sapply(function(row) {valueForPath(row, "Mladi")})),
  Zreli = as.integer(Starost_Spol %>% sapply(function(row) {valueForPath(row, "Zreli")})),
  Stari = as.integer(Starost_Spol %>% sapply(function(row) {valueForPath(row, "Stari")}))
) %>% 
  mutate(Spol = rep(c("Moski", "Zenske"), 
  each = length(unique(Leto)))) %>%
  pivot_longer(-c("Leto", "Spol"), names_to = "Starost", values_to = "Stevilo")


# Tabela, ki prikazuje stevilke odseljevanja iz Slovenije
# glede na aktivnost(zaposlen, nezaposlen) in spol

Aktivnost <- "podatki/aktivnost_splosno.xml" %>%
  read_xml(encoding = "Windows-1250") %>% html_nodes(xpath = "/root/row")

Aktivnost <- tibble(
  Leto = as.integer(Aktivnost %>% sapply(function(row) {valueForPath(row, "Leto")})),
  Zaposleni = as.integer(Aktivnost %>% sapply(function(row) {valueForPath(row, "Zaposleni")})),
  Nezaposleni = as.integer(Aktivnost %>% sapply(function(row) {valueForPath(row, "Nezaposleni")}))
) %>%
  mutate(Starost = rep(c("Mladi", "Zreli", "Mlade", "Zrele"), 
  each = length(unique(Leto)))) %>%
  pivot_longer(-c("Leto", "Starost"), names_to = "Aktivnost", values_to = "Stevilo")





