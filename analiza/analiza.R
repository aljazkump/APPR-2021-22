U1 = Tabela_Tujina_Izobrazba_Spol %>% 
  filter(Spol == "Moški") %>%
  group_by(Drzava) %>%
  summarise(SteviloM = sum(Stevilo))

U2 = Tabela_Tujina_Izobrazba_Spol %>% 
  filter(Spol != "Moški") %>%
  group_by(Drzava) %>%
  summarise(SteviloZ = sum(Stevilo))

NaprednaTabela = full_join(U1, U2)

NaprednaTabela %>% ggplot(aes(SteviloM, SteviloZ, col = Drzava)) + 
  geom_point(size = 4)

Drzave = NaprednaTabela[, -1] %>%
  dist() %>%
  hclust()

Dendogram <- plot(
  Drzave,
  labels = NaprednaTabela$Drzava,
  ylab = "Stevilo",
  main = NULL
)

tibble(
  k = 15:1,
  visina = Drzave$height
) %>%
  ggplot() +
  geom_line(
    mapping = aes(x = k, y = visina),
    color = "red",
    size = 1.25
  ) +
  geom_point(
    mapping = aes(x = k, y = visina),
    color = "red",
    size = 3
  ) +
  scale_x_continuous(
    breaks = 15:1
  ) +
  labs(
    x = "število skupin (k)",
    y = "višina združevanja"
  ) +
  theme_classic()

hc.kolena = function(Drzave, od = 1, do = NULL, eps = 0.5) {
  n = length(Drzave$height) + 1
  if (is.null(do)) {
    do = n - 1
  }

  k.visina = tibble(
    k = as.ordered(od:do),
    visina = Drzave$height[do:od]
  ) %>%
    mutate(
      dvisina = visina - lag(visina)
    ) %>%
    mutate(
      koleno = lead(dvisina) - dvisina > eps
    )
  k.visina
}


hc.kolena.k = function(k.visina) {
  k.visina %>%
    filter(koleno) %>%
    select(k) %>%
    unlist() %>%
    as.character() %>%
    as.integer()
}

r = hc.kolena(Drzave)

diagram.kolena = function(k.visina) {
  k.visina %>% ggplot() +
    geom_point(
      mapping = aes(x = k, y = visina),
      color = "red",
      size = 3
    ) +
    geom_line(
      mapping = aes(x = as.integer(k), y = visina),
      color = "red",
      size = 1.25
    ) +
    geom_point(
      data = k.visina %>% filter(koleno),
      mapping = aes(x = k, y = visina),
      color = "blue", size = 3
    ) +
    ggtitle(paste("Kolena:", paste(hc.kolena.k(k.visina), collapse = ", "))) +
    xlab("število skupin (k)") +
    ylab("razdalja pri združevanju skupin") +
    theme_classic()
}

kol <- diagram.kolena(r)

# KOLENA : 2, 3, 4, 6, 9, 10, 11 ,13
skupine.2 = Drzave %>% cutree(k = 2) %>% as.ordered()
skupine.3 = Drzave %>% cutree(k = 3) %>% as.ordered()
skupine.4 = Drzave %>% cutree(k = 4) %>% as.ordered()
skupine.6 = Drzave %>% cutree(k = 6) %>% as.ordered()
skupine.9 = Drzave %>% cutree(k = 9) %>% as.ordered()
skupine.10 = Drzave %>% cutree(k = 10) %>% as.ordered()
skupine.11 = Drzave %>% cutree(k = 11) %>% as.ordered()
skupine.13 = Drzave %>% cutree(k = 13) %>% as.ordered()



diagram.skupine = function(podatki, drzave, skupine, k) {
  podatki = podatki %>%
    bind_cols(skupine) %>%
    dplyr::rename(skupina = ...4)
  
  d = podatki %>%
    ggplot(
      mapping = aes(
        x = SteviloM , y = SteviloZ, color = skupina
      )
    ) +
    geom_point() +
    geom_label_repel(label = drzave, size = 2.5) +
    scale_color_hue() +
    theme_classic()
  
  for (i in 1:k) {
    d = d + geom_encircle(
      data = podatki %>%
        filter(skupina == i)
    )
  }
  d
}

diag <- diagram.skupine(NaprednaTabela, NaprednaTabela$Drzava, skupine.4, 4)




