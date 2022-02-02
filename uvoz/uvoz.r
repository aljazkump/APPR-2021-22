# 2. faza: Uvoz podatkov

sl <- locale("sl", decimal_mark=",", grouping_mark=".");

# Tabela, ki prikazuje stevilke odseljevanja v Evropske drzave 
# glede na starost(mladi, zreli, stari)

Starost_Evropa <- read.csv(
  "podatki/starost_tujina.csv",
  encoding = "Windows-1250"
)

drzave = c("Avstrija", "Hrvaska", "Nemcija", "Italija", "Svica", "Zdruzeno.kraljestvo")

Starost_Evropa <- Starost_Evropa %>% select(Leto, drzave)

Tabela_EU_Mladi <- function(drzava) {
  Starost_Evropa[1:10, 1:length(Starost_Evropa)] %>% 
    pivot_longer(cols = colnames(Starost_Evropa[1:10, 1:length(Starost_Evropa)])[-1],
                 names_to = "DRZAVE", values_to = "MLADI") %>% filter(DRZAVE == drzava)
}

Tabela_EU_Zreli <- function(drzava) {
  Starost_Evropa[11:20, 1:length(Starost_Evropa)] %>% 
    pivot_longer(cols = colnames(Starost_Evropa[11:20, 1:length(Starost_Evropa)])[-1],
                 names_to = "DRZAVE", values_to = "ZRELI") %>% filter(DRZAVE == drzava)
}

Tabela_EU_Stari <- function(drzava) {
  Starost_Evropa[21:30, 1:length(Starost_Evropa)] %>% 
    pivot_longer(cols = colnames(Starost_Evropa[11:30, 1:length(Starost_Evropa)])[-1],
                 names_to = "DRZAVE", values_to = "STARI") %>% filter(DRZAVE == drzava)
}

f1 = full_join(Tabela_EU_Mladi("Avstrija"), Tabela_EU_Mladi("Hrvaska"))
f2 = full_join(Tabela_EU_Mladi("Nemcija"), Tabela_EU_Mladi("Italija"))
f3 = full_join(Tabela_EU_Mladi("Svica"), Tabela_EU_Mladi("Zdruzeno.kraljestvo"))
w1 = full_join(full_join(f1, f2), f3)

f4 = full_join(Tabela_EU_Zreli("Avstrija"), Tabela_EU_Zreli("Hrvaska"))
f5 = full_join(Tabela_EU_Zreli("Nemcija"), Tabela_EU_Zreli("Italija"))
f6 = full_join(Tabela_EU_Zreli("Svica"), Tabela_EU_Zreli("Zdruzeno.kraljestvo"))
w2 = full_join(full_join(f4, f5), f6)

f7 = full_join(Tabela_EU_Stari("Avstrija"), Tabela_EU_Stari("Hrvaska"))
f8 = full_join(Tabela_EU_Stari("Nemcija"), Tabela_EU_Stari("Italija"))
f9 = full_join(Tabela_EU_Stari("Svica"), Tabela_EU_Stari("Zdruzeno.kraljestvo"))
w3 = full_join(full_join(f7, f8), f9)

starost_evropa <- full_join(full_join(w1,w2),w3)

# Tabela, ki prikazuje stevilke odseljevanja v Evropske drzave 
# v tem primeru skoraj za vsako drzavo Evrope                                        
                      
Evropa <- read.csv(
  "podatki/starost_EU.csv",
  encoding = "Windows-1250"
)

evropa <- Evropa %>% select(X, Skupno)
colnames(evropa) <- c("region", "SKUPNO")

mapdata = left_join(map_data("world"), evropa, by="region") %>%
  filter(!is.na(SKUPNO));

 
# Tabela, ki prikazuje stevilke odseljevanja iz vsake obcine
 
Obcine_Odseljeni <- read.csv(
  "podatki/obcine.csv", 
  encoding = "Windows-1250"
)

Obcine_Odseljeni %>% select(X, Skupaj)
colnames(Obcine_Odseljeni) <- c("OBCINE", "SKUPAJ")
Obcine_Odseljeni <- Obcine_Odseljeni[1:212, 1:2]

delez = df


# Tabela za kasnejso vizualizacijo obcin na zemljevidu 

SIob <- readOGR("podatki/OB.shp", layer = "OB", encoding = "Windows-1250");

SIob_fort <- SIob %>%
  fortify("region");

SIob_fort <- left_join(SIob_fort, delez, by = "id");
                                
                                
# Tabela, ki prikazuje stevilke odseljevanja iz Slovenije 
# glede na izobrazbo, spol in starost 

Starost_Izobrazba_Spol = read.csv(
  "podatki/Starost_Izobrazba_splosno.csv",
  encoding = "Windows-1250"
)

a1 = full_join(
  Tabela(1,10, "OS_moski", Starost_Izobrazba_Spol),
  Tabela(11,20, "SS_moski", Starost_Izobrazba_Spol)
);

a2 = full_join(
  Tabela(21,30, "VS_moski", Starost_Izobrazba_Spol),
  Tabela(31,40, "OS_zenske", Starost_Izobrazba_Spol)
);

a3 = full_join(
  Tabela(41,50, "SS_zenske", Starost_Izobrazba_Spol),
  Tabela(51,60, "VS_zenske", Starost_Izobrazba_Spol)
);

starost_izobrazba_spol = full_join(full_join(a1,a2),a3)

# Tabele za vizualizacijo

Tabela <- function(a, b, value, detoteka) {
  detoteka[a:b,1:length(detoteka)] %>%
    pivot_longer(
      cols = colnames(detoteka[a:b, 1:length(detoteka)])[-1],
      names_to = "Kategorija",
      values_to = value
    ) %>%
    filter(Kategorija %in% c("Mladi", "Zreli", "Stari")) %>%
    arrange(Kategorija, Leto)
}

Izseljevanje_Mladi_Moski <- starost_izobrazba_spol %>%
  pivot_longer(cols = names(.)[-1:-2]) %>% filter(Kategorija == "Mladi") %>%
  filter(name %in% c("OS_moski","SS_moski","VS_moski"))

Izseljevanje_Mlade_Zenske <- starost_izobrazba_spol %>%
  pivot_longer(cols = names(.)[-1:-2]) %>% filter(Kategorija == "Mladi") %>%
  filter(name %in% c("OS_zenske","SS_zenske","VS_zenske"))

Izseljevanje_Zreli_Moski <- starost_izobrazba_spol %>%
  pivot_longer(cols = names(.)[-1:-2]) %>% filter(Kategorija == "Zreli") %>%
  filter(name %in% c("OS_moski","SS_moski","VS_moski"))

Izseljevanje_Zrele_Zenske <- starost_izobrazba_spol %>%
  pivot_longer(cols = names(.)[-1:-2]) %>% filter(Kategorija == "Zreli") %>%
  filter(name %in% c("OS_zenske","SS_zenske","VS_zenske"))
                                

# Tabela, ki prikazuje stevilke odseljevanja iz Slovenije 
# glede na starost in spol    

Starost_Spol = read_csv(
  "podatki/starost_spol.csv",
  locale = sl
)

# Tabele za vizualizacijo

Izseljevanje_Moski_Starost  <- Tabela(1,10, "Moski", Starost_Spol)

Izseljevanje_Zenske_Starost <- Tabela(11,20, "Zenske", Starost_Spol)

Izseljevanje_Mladi_Zreli <- full_join(Izseljevanje_Moski_Starost, 
                                      Izseljevanje_Zenske_Starost) %>%
  filter(Kategorija %in% c("Mladi", "Zreli"))


# Tabela, ki prikazuje stevilke odseljevanja iz Slovenije
# glede na aktivnost(zaposlen, nezaposlen) in spol

Aktivnost = read.csv(
  "podatki/aktivnost_splosno.csv",
  encoding = "Windows-1250"
);

Zaposleni <- function(a,b,value) {
  Aktivnost[a:b, 1:length(Aktivnost)] %>% select(Leto,Zaposleno) %>% 
    pivot_longer(cols = colnames(Aktivnost[a:b, 1:2])[-1], names_to = "STATUS", values_to = value)          
}

Nezaposleni <- function(a,b,value) {
  Aktivnost[a:b, 1:length(Aktivnost)] %>% select(Leto,Nezaposleno) %>% 
    pivot_longer(cols = colnames(Aktivnost[a:b, seq(1,3,2)])[-1], names_to = "STATUS", values_to = value)          
}

Mladi_zaposleni = Zaposleni(1, 10, "MLADI")
Zreli_zaposleni = Zaposleni(11, 20, "ZRELI")
Mlade_zaposlene = Zaposleni(21, 30, "MLADE")
Zrele_zaposlene = Zaposleni(31, 40, "ZRELE")

aktivnost_temp1 = full_join(full_join(Mladi_zaposleni, Zreli_zaposleni) , full_join(Mlade_zaposlene, Zrele_zaposlene))

Mladi_nezaposleni = Nezaposleni(1, 10, "MLADI")
Zreli_nezaposleni = Nezaposleni(11, 20, "ZRELI")
Mlade_nezaposlene = Nezaposleni(21, 30, "MLADE")
Zrele_nezaposlene = Nezaposleni(31, 40, "ZRELE")

aktivnost_temp2 = full_join(full_join(Mladi_nezaposleni, Zreli_nezaposleni) , full_join(Mlade_nezaposlene, Zrele_nezaposlene))

aktivnost = full_join(akti_temp1, akti_temp2)

                                
