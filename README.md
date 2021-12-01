# Analiza podatkov s programom R - 2021/22

Vzorčni repozitorij za projekt pri predmetu APPR v študijskem letu 2021/22. 

## Analiza odseljevanja iz Slovenije

V svoji seminarski nalogi bom analiziral, kako se je spreminjalo odseljevanje
iz Slovenije v tujino skozi leta. Podatke sem izbral iz SI-Stata. Za spremelnljive
sem si izbral:
  - `Spol`
  - `Starost`
  - `Geografske regije` 
  - `Izobrazba`
  - `Delovna aktivnost`
  - `Drzava priseljevanja`

</br>

**SPOL**: moski in zenske </br>
**STAROST** : otroci in mladostniki (0-18 let), mladi (18-30 let), zrela populacija (30-64 let), stari (64+) </br>
**GEOGRAFKSE REGIJE** : 12 uradnih geografskih regij </br>
**IZOBRAZBA** : osnovnosolska ali manj, srednjesolska, visjesolska in visokosolska </br>
**DELOVNA AKTIVNOST** : Zaposleni in brezposelni </br>

</br>

Vsako spremenljivko bom graficno prikazal tako, da bojo cim bolj razvidni trendi ali
vzorci, ki so se pojavljali skozi leta. Te bom tudi komentiral in obrazlozil, kaj je povzoricilo
spremembo. Na podlagi tega, bom lahko napovedal, kako se bodo vrednosti gibale v prihodnosti.

Spremenljivke bom tudi med seboj komibiniral in prikazal kako se skupno obnasajo. Npr ce si izberemo
spola. Ali se razlikujeta po povprecni izobrazbeni sestavi, starosti, delovni aktivnosti itd. Ker si
takih kombinacij lahko izbremo zelo veliko, bom izbral le tiste, ki so bolj aktualne v danasnem casu.


## Program

Glavni program in poročilo se nahajata v datoteki `projekt.Rmd`.
Ko ga prevedemo, se izvedejo programi, ki ustrezajo drugi, tretji in četrti fazi projekta:

* obdelava, uvoz in čiščenje podatkov: `uvoz/uvoz.r`
* analiza in vizualizacija podatkov: `vizualizacija/vizualizacija.r`
* napredna analiza podatkov: `analiza/analiza.r`

Vnaprej pripravljene funkcije se nahajajo v datotekah v mapi `lib/`.
Potrebne knjižnice so v datoteki `lib/libraries.r`
Podatkovni viri so v mapi `podatki/`.
Zemljevidi v obliki SHP, ki jih program pobere,
se shranijo v mapo `../zemljevidi/` (torej izven mape projekta).
