# 2. faza: Uvoz podatkov

sl <- locale("sl", decimal_mark=",", grouping_mark=".");

# Tabela, ki prikazuje stevilke odseljevanja v Evropske drzave 
# glede na starost(mladi, zreli, stari)

starost_Evropa <- read.csv("podatki/starost_tujina.csv", encoding = "UTF-8");

                          
# Tabela, ki prikazuje stevilke odseljevanja v Evropske drzave 
# glede na izobrazbo(osnovnosolka, srednjesolska, visokosolska)

izobrazba_Evropa <- read.csv("podatki/Spol_Izobrazba_tujina.csv", encoding = "UTF-8");

                                            
# Tabela, ki prikazuje stevilke odseljevanja v Evropske drzave 
# v tem primeru skoraj za vsako drzavo Evrope                                        
                      
Evropa <- read.csv("podatki/starost_EU.csv", encoding = "UTF-8");


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
 
                                            
# Tabela, ki prikazuje stevilke odseljevanja iz vsake obcine
 
obcine_odseljeni <- read.csv("podatki/obcine.csv", encoding = "UTF-8");


# Tabela, ki prikazuje stevilo prebivalcev vsake obcine

obcine_prebivalstvo <- read.csv("podatki/obcine_prebivalstvo.csv", encoding = "UTF-8");
                                
                                
# Tabela, ki prikazuje stevilke odseljevanja iz Slovenije 
# glede na izobrazbo, spol in starost                              
                                
starost_izobrazba = read.csv("podatki/Starost_Izobrazba_splosno.csv", encoding = "UTF-8");

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
  stare_zenske_VS = Starost_Izobrazba[51:60, 4]

);


# Tabela, ki prikazuje stevilke odseljevanja iz Slovenije 
# glede na starost in spol    

Starost_spol = read.csv("podatki/starost_spol.csv", encoding = "UTF-8");
                        
razvrstitev_po_starosti_spolu = data.frame(
  
  leto = 2011 : 2020,
  
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


# Tabela, ki prikazuje stevilke odseljevanja iz Slovenije
# glede na aktivnost(zaposlen, nezaposlen) in spol

aktivnost = read.csv("podatki/aktivnost_splosno.csv", encoding = "UTF-8");

razvrstitev_po_aktivnosti_spolu = data.frame(
  
  leto = 2011 : 2020,
  
  mladi_zaposleni = aktivnost[1:10, 2],
  mlade_zaposlene = aktivnost[1:10, 6],
  
  zreli_zaposleni = aktivnost[1:10, 3],
  zrele_zaposlene = aktivnost[1:10, 7],
  
  mladi_nezaposleni = aktivnost[1:10, 4],
  mlade_nezaposlene = aktivnost[1:10, 8],
  
  zreli_nezaposleni = aktivnost[1:10, 5],
  zrele_nezaposlene = aktivnost[1:10, 9]
  
);
                                
