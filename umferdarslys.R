library(tidyverse)
library(rvest)
library(purrr)
library(metill)
library(ggh4x)
library(hagstofa)
theme_set(theme_metill())
Sys.setlocale("LC_ALL", "is_IS.UTF-8")


months_is <- month(clock::date_build(2023, 1:12), label = TRUE, abbr = FALSE)
month_vec <- 1:12
names(month_vec) <- as.character(months_is)

d <- read_html("https://island.is/umferdarslys-slysatoelur")

d_tab <- d |> html_table(header = TRUE)

d_2024 <- tibble(
  dags = clock::date_build(2024, 1:6),
  banaslys = c(3, 2, 1, 1, 0, 1),
  latnir = c(5, 2, 1, 2, 0, 1)
)

pop <- hg_data("https://px.hagstofa.is:443/pxis/api/v1/is/Ibuar/mannfjoldi/1_yfirlit/arsfjordungstolur/MAN10001.px")  |> 
  filter(
    Sveitarfélag == "Alls",
    `Kyn og ríkisfang` == "Alls"
  ) |> 
  collect() |> 
  janitor::clean_names() |> 
  rename(pop = 4) |> 
  janitor::remove_constant() |> 
  separate(arsfjordungur, into = c("ar", "man"), sep = "Á", convert = TRUE) |> 
  mutate(
    dags = clock::date_build(ar, 1 + (man - 1) * 3)
  ) |> 
  select(dags, pop)

d1 <- d_tab[seq(1, 29, by = 2)] |> 
  imap_dfr(\(x, i) x |> janitor::clean_names() |>  mutate(ar = 2024 - i)) |> 
  drop_na() |> 
  filter(x != "Samtals") |> 
  mutate(
    man = month_vec[x],
    dags = clock::date_build(ar, man)
  ) |> 
  select(dags, banaslys:samtals) |> 
  rename(
    samtals_slys = samtals
  )

d2 <-  d_tab[seq(2, 30, by = 2)] |> 
  imap_dfr(\(x, i) x |> janitor::clean_names() |>  mutate(ar = 2024 - i)) |> 
  drop_na() |> 
  filter(x != "Samtals") |> 
  mutate(
    man = month_vec[x],
    dags = clock::date_build(ar, man)
  ) |> 
  select(dags, latnir:samtals) |> 
  rename(samtals_slasadir = samtals)

d_tot <- d1 |> 
  inner_join(
    d2,
    by = join_by(dags)
  ) |> 
  bind_rows(
    d_2024
  ) |> 
  arrange(dags)  |> 
  left_join(
    pop,
    by = join_by(dags)
  ) |> 
  mutate(
    pop = zoo::na.approx(pop, na.rm = FALSE)
  ) |> 
  fill(pop, .direction = "updown")

notkun <- read_html(
  "https://island.is/oennur-toelfraedi-samgoengustofu"
) |> 
  html_table()


notkun_later <- notkun[1:5] |> 
  imap_dfr(
    \(x, i) x |> 
      janitor::clean_names() |> 
      rename(tegund = 1) |>  
      mutate_all(as.character) |>  
      mutate(ar = 2023 - i)
  ) |> 
  mutate(
    index = row_number(),
    .by = ar
  ) |> 
  mutate(
    alls = index[tegund == "Alls"],
    .by = ar
  ) |> 
  filter(index >= alls) |> 
  select(-index, -alls) |> 
  drop_na() |> 
  mutate(
    fjoldi_okutaekja = str_replace(fjoldi_okutaekja, "287\\.55", "287\\.550")
  ) |> 
  filter(tegund == "Fólksbifreiðar") |> 
  mutate_at(
    vars(fjoldi_okutaekja:medalakstur_a_ari),
    \(x) parse_number(x, locale = locale(decimal_mark = ",", grouping_mark = "."))
  ) |> 
  mutate(
    weight = fjoldi_okutaekja / sum(fjoldi_okutaekja),
    .by = ar
  ) |> 
  summarise(
    medalakstur = sum(medalakstur_a_dag * weight),
    fjoldi_total = sum(fjoldi_okutaekja),
    fjoldi_skrad = sum(fjoldi_i_urtaki),
    .by = ar
  )

tmp <- notkun[-(1:5)] |> 
  imap_dfr(
    \(x, i) x |> 
      janitor::clean_names() |> 
      rename(tegund = 1) |>  
      mutate_all(as.character) |>  
      mutate(ar = 2018 - i) 
  )


tmp[c(2, 4), 2:5] <- tmp[c(2, 4), 1:4]


notkun_earlier <- tmp |> 
  mutate_at(
    vars(fjoldi_sem_telur_1:medalakstur_a_ari),
    \(x) parse_number(x, locale = locale(decimal_mark = ",", grouping_mark = "."))
  ) |> 
  filter(x == "Fólksbílar") |> 
  mutate(
    weight = fjoldi_sem_telur_1 / sum(fjoldi_sem_telur_1),
    .by = ar
  ) |> 
  summarise(
    medalakstur = sum(medalakstur_a_dag * weight),
    fjoldi_total = sum(fjoldi_sem_telur_1) * 2,
    .by = ar
  )




notkun_tot <- notkun_earlier |> 
  bind_rows(notkun_later) |> 
  bind_rows(
    notkun_later |> 
      filter(ar >= 2021) |> 
      mutate(ar = ar + 2)
  ) |> 
  arrange(ar) |> 
  mutate(
    medalakstur = medalakstur * 365.25 / 12
  )


p <- d_tot |> 
  mutate(ar = year(dags)) |> 
  inner_join(
    notkun_tot,
    by = join_by(ar)
  ) |> 
  mutate(
    value = slider::slide_dbl(latnir / (fjoldi_total * medalakstur) * 1e6, sum, .before = 11)
  ) |> 
  filter(year(dags) >= 2010) |> 
  ggplot(aes(dags, value)) +
  geom_col(width = 31, position = "identity", col = "grey50", fill = "grey50") +
  scale_x_date(
    breaks = breaks_width("year"),
    labels = label_date_short(),
    guide = guide_axis_truncated()
  ) +
  scale_y_continuous(
    limits = c(0, NA),
    expand = expansion(c(0, 0.05)),
    guide = guide_axis_truncated()
  ) +
  labs(
    x = NULL, 
    y = NULL,
    title = "Látnir í umferðinni á milljón klukkustundir af akstri innanlands (2010 - 2024)",
    subtitle = "Reiknað sem heildarfjöldi látinna á undanförnu ári  hverju sinni",
    caption = "Reiknað út frá gögnum Samgöngustofu um banaslys í umferð, meðalakstur fólksbifreiða og fjölda fólksbifreiða í umferð"
  )


ggsave(
  plot = p,
  filename = "Figures/latnir_akstur.png",
  width = 8, height = 0.5 * 8, scale = 1.3
)




p <- d_tot |> 
  mutate(ar = year(dags)) |> 
  inner_join(
    notkun_tot,
    by = join_by(ar)
  ) |> 
  mutate(
    value = slider::slide_dbl(latnir / (fjoldi_total * medalakstur), sum, .before = 11),
    value = 1 / value / pop
  ) |> 
  filter(year(dags) >= 2010) |> 
  ggplot(aes(dags, value)) +
  geom_col(width = 31, position = "identity", col = "grey50", fill = "grey50") +
  scale_x_date(
    breaks = breaks_width("year"),
    labels = label_date_short(),
    guide = guide_axis_truncated()
  ) +
  scale_y_continuous(
    limits = c(0, NA),
    expand = expansion(c(0, 0.05)),
    guide = guide_axis_truncated(),
    labels = label_number(suffix = " klst/íbúa")
  ) +
  coord_cartesian(
    ylim = c(0, 200)
  ) +
  labs(
    x = NULL, 
    y = NULL,
    title = "Hvað eru margar klukkustundir af akstri á bak við hvert andlát í umferðinni",
    caption = "Reiknað út frá gögnum Samgöngustofu um banaslys í umferð, meðalakstur fólksbifreiða og fjölda fólksbifreiða í umferð"
  )


ggsave(
  plot = p,
  filename = "Figures/hve_mikinn_akstur_latinn.png",
  width = 8, height = 0.5 * 8, scale = 1.3
)



p <- d_tot |> 
  mutate(
    value = slider::slide_dbl(latnir / pop * 1e5, sum, .before = 11)
  ) |> 
  filter(year(dags) >= 2010) |> 
  ggplot(aes(dags, value)) +
  geom_col(width = 31, col = "grey50", fill = "grey50") +
  scale_x_date(
    breaks = clock::date_build(2010:2024),
    labels = label_date_short(),
    guide = guide_axis_truncated()
  ) +
  scale_y_continuous(
    limits = c(0, NA),
    expand = expansion(c(0, 0.05)),
    guide = guide_axis_truncated()
  ) +
  labs(
    x = NULL, 
    y = NULL,
    title = "Látnir í umferðinni (2010 - 2024)",
    subtitle = "Sýnt sem fjöldi á hverja 100.000 íbúa landsins | Reiknað sem heildarfjöldi látinna á undanförnu ári hverju sinni"
  )


ggsave(
  plot = p,
  filename = "Figures/latnir_a_ibua.png",
  width = 8, height = 0.5 * 8, scale = 1.3
)
