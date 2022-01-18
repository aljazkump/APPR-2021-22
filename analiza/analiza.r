# 4. faza: Napredna analiza podatkov

# Analiziral bom podatke tako, da bom predvidel na podlagi metode najmanjsih 
# kvadratov izracunal podatke za leto 2021.
# Prestavil bom samo podatke za vecje trende, kot so spol in starost.
# Za manjse trende se nisem odlocil, kot so izobrazba ali aktivnost, saj so prevec
# odvisni od gospodarstva in povprasevanja.

A <- rbind(c(0,0,1),
           c(1,1,1),
           c(4,2,1)
           );

y = c(9,3,1);

# NAPOVED ZA ODSELJEVANJE V EVROPSKE DRZAVE

B_NEM_mladi <- c(starost_Evropa$Nem.e8.ija[8], starost_Evropa$Nem.e8.ija[9], starost_Evropa$Nem.e8.ija[10]);
B_NEM_zreli <- c(starost_Evropa$Nem.e8.ija[18], starost_Evropa$Nem.e8.ija[19], starost_Evropa$Nem.e8.ija[20]);
B_NEM_stari <- c(starost_Evropa$Nem.e8.ija[28], starost_Evropa$Nem.e8.ija[29], starost_Evropa$Nem.e8.ija[30]);

B_CH_mladi <- c(starost_Evropa$Svica[8], starost_Evropa$Svica[9], starost_Evropa$Svica[10]);
B_CH_zreli <- c(starost_Evropa$Svica[18], starost_Evropa$Svica[19], starost_Evropa$Svica[20]);
B_CH_stari <- c(starost_Evropa$Svica[28], starost_Evropa$Svica[29], starost_Evropa$Svica[30]);

B_UK_mladi <- c(starost_Evropa$Zdruzeno.kraljestvo[8], starost_Evropa$Zdruzeno.kraljestvo[9], starost_Evropa$Zdruzeno.kraljestvo[10]);
B_UK_zreli <- c(starost_Evropa$Zdruzeno.kraljestvo[18], starost_Evropa$Zdruzeno.kraljestvo[19], starost_Evropa$Zdruzeno.kraljestvo[20]);
B_UK_stari <- c(starost_Evropa$Zdruzeno.kraljestvo[28], starost_Evropa$Zdruzeno.kraljestvo[29], starost_Evropa$Zdruzeno.kraljestvo[30]);

B_ITA_mladi <- c(starost_Evropa$Italija[8], starost_Evropa$Italija[9], starost_Evropa$Italija[10]);
B_ITA_zreli <- c(starost_Evropa$Italija[18], starost_Evropa$Italija[19], starost_Evropa$Italija[20]);
B_ITA_stari <- c(starost_Evropa$Italija[28], starost_Evropa$Italija[29], starost_Evropa$Italija[30]);

B_HRT_mladi <- c(starost_Evropa$Hrvaska[8], starost_Evropa$Hrvaska[9], starost_Evropa$Hrvaska[10]);
B_HRT_zreli <- c(starost_Evropa$Hrvaska[18], starost_Evropa$Hrvaska[19], starost_Evropa$Hrvaska[20]);
B_HRT_stari <- c(starost_Evropa$Hrvaska[28], starost_Evropa$Hrvaska[29], starost_Evropa$Hrvaska[30]);

B_AU_mladi <- c(starost_Evropa$Avstrija[8], starost_Evropa$Avstrija[9], starost_Evropa$Avstrija[10]);
B_AU_zreli <- c(starost_Evropa$Avstrija[18], starost_Evropa$Avstrija[19], starost_Evropa$Avstrija[20]);
B_AU_stari <- c(starost_Evropa$Avstrija[28], starost_Evropa$Avstrija[29], starost_Evropa$Avstrija[30]);

# Nemcija

x1 = solve(A,B_NEM_mladi); x2 = solve(A,B_NEM_zreli); x3 = solve(A,B_NEM_stari);
r1 = sum(x1*y); r2 = sum(x2*y); r3 = sum(x3*y);
df_NEM = data.frame(Leta = 2021, mladi = r1, zreli = r2, stari = r3);
DF_NEM = rbind(NEM, df_NEM);

# Svica

x4 = solve(A,B_CH_mladi); x5 = solve(A,B_CH_zreli); x6 = solve(A,B_CH_stari);
r4 = sum(x4*y); r5 = sum(x5*y); r6 = sum(x6*y);
df_CH = data.frame(Leta = 2021, mladi = r4, zreli = r5, stari = r6);
DF_CH = rbind(CH, df_CH);

# Zdruzeno Kraljestvo

x7 = solve(A,B_UK_mladi); x8 = solve(A,B_UK_zreli); x9 = solve(A,B_UK_stari);
r7 = sum(x7*y); r8 = sum(x8*y); r9 = sum(x9*y);
df_UK = data.frame(Leta = 2021, mladi = r7, zreli = r8, stari = r9);
DF_UK = rbind(UK, df_UK);

# Italija

x10 = solve(A,B_ITA_mladi); x11 = solve(A,B_ITA_zreli); x12 = solve(A,B_ITA_stari);
r10 = sum(x10*y); r11 = sum(x11*y); r12 = sum(x12*y);
df_ITA = data.frame(Leta = 2021, mladi = r10, zreli = r11, stari = r12);
DF_ITA = rbind(ITA, df_ITA);

# Hrvaska

x13 = solve(A,B_HRT_mladi); x14 = solve(A,B_HRT_zreli); x15 = solve(A,B_HRT_stari);
r13 = sum(x13*y); r14 = sum(x14*y); r15 = sum(x15*y);
df_HRT = data.frame(Leta = 2021, mladi = r13, zreli = r14, stari = r15);
DF_HRT = rbind(HRT, df_HRT);

# Avstrija

x16 = solve(A,B_AU_mladi); x17 = solve(A,B_AU_zreli); x18 = solve(A,B_AU_stari);
r16 = sum(x16*y); r17 = sum(x17*y); r18 = sum(x18*y);
df_AU = data.frame(Leta = 2021, mladi = r16, zreli = r17, stari = r18);
DF_AU = rbind(AU, df_AU);


# NAPOVED IZSELJEVANJE MOSKEGA, ZENSKEGA PREBIVALSTVA PO STAROSTI IN POVPRECNE
# STAROSTI ZA OBA SPOLA

Mladi = c(razvrstitev_po_starosti_spolu$mladi_moski[8], razvrstitev_po_starosti_spolu$mladi_moski[9], razvrstitev_po_starosti_spolu$mladi_moski[10]);
Zreli = c(razvrstitev_po_starosti_spolu$zreli_moski[8], razvrstitev_po_starosti_spolu$zreli_moski[9], razvrstitev_po_starosti_spolu$zreli_moski[10]);
Stari = c(razvrstitev_po_starosti_spolu$stari_moski[8], razvrstitev_po_starosti_spolu$stari_moski[9], razvrstitev_po_starosti_spolu$stari_moski[10]);

Mlade = c(razvrstitev_po_starosti_spolu$mlade_zenske[8], razvrstitev_po_starosti_spolu$mlade_zenske[9], razvrstitev_po_starosti_spolu$mlade_zenske[10]);
Zrele = c(razvrstitev_po_starosti_spolu$zrele_zenske[8], razvrstitev_po_starosti_spolu$zrele_zenske[9], razvrstitev_po_starosti_spolu$zrele_zenske[10]);
Stare = c(razvrstitev_po_starosti_spolu$stare_zenske[8], razvrstitev_po_starosti_spolu$stare_zenske[9], razvrstitev_po_starosti_spolu$stare_zenske[10]);

Povprecje_moski = c(razvrstitev_po_starosti_spolu$povprecna_starost_moskih[8], razvrstitev_po_starosti_spolu$povprecna_starost_moskih[9], razvrstitev_po_starosti_spolu$povprecna_starost_moskih[10]);
Povprecje_zenske = c(razvrstitev_po_starosti_spolu$povprecna_starost_zensk[8], razvrstitev_po_starosti_spolu$povprecna_starost_zensk[9], razvrstitev_po_starosti_spolu$povprecna_starost_zensk[10]);

df_STAROST_SPOL = data.frame(
  leto = 2021, 
  mladi_moski = sum(solve(A, Mladi) * y),
  zreli_moski = sum(solve(A, Zreli) * y),
  stari_moski = sum(solve(A, Stari) * y),
  mlade_zenske = sum(solve(A, Mlade) * y),
  zrele_zenske= sum(solve(A, Zrele) * y),
  stare_zenske = sum(solve(A, Stare) * y),
  povprecna_starost_moskih = sum(solve(A, Povprecje_moski) * y),
  povprecna_starost_zensk = sum(solve(A, Povprecje_zenske) * y),
  moski_skupaj = 0,
  zenske_skupaj = 0
  );

DF_STAROST_SPOL = rbind(razvrstitev_po_starosti_spolu, df_STAROST_SPOL);




