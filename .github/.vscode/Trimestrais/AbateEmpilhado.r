#
library(tidyverse)

# Obtendo os dados
todos_dados <- sidrar::get_sidra(api = "/t/1092/n1/all/v/284,285/p/all/c12716/115236/c18/55,56,111734,111735/c12529/118225") |>
    janitor::clean_names() |>
    tibble::as_tibble() |>
    dplyr::glimpse() |>
    dplyr::select(valor, variavel, trimestre_codigo, trimestre) |>
    dplyr::relocate(trimestre, trimestre_codigo, variavel, valor)

#ultimo_2t24 <- sidrar::get_sidra(api = "/t/6829/n1/all/v/all/p/last%201/c12716/115236/c79/41107") |>
#    janitor::clean_names() |>
#    tibble::as_tibble() |>
#    dplyr::glimpse() |>
#    dplyr::select(valor, variavel, trimestre_codigo, trimestre) |>
#    dplyr::relocate(trimestre, trimestre_codigo, variavel, valor)

# Manipulando os dados

abate_carcaca <- todos_dados |>
    #dplyr::full_join(ultimo_2t24) |>
    dplyr::mutate(
        ano = stringr::str_extract(trimestre, "\\d{4}") |> as.numeric(),
        trimestre = stringr::str_remove(trimestre, "\\s+\\d{4}$"),
        trimestre = factor(trimestre, levels = unique(trimestre)),
        trimestre = forcats::fct_relevel(
        trimestre, "4º trimestre", "3º trimestre", "2º trimestre","1º trimestre" )
    )

font <- "Josefin Sans"
font2 <- "Open Sans"
font3 <- "Permanent Marker"
sysfonts::font_add_google(family = font, font, db_cache = FALSE)
sysfonts::font_add_google(family = font2, font2, db_cache = FALSE)
sysfonts::font_add_google(family = font3, font3, db_cache = FALSE)
sysfonts::font_add(family = "Font Awesome 6 Brands", regular = "C:/Users/italo/AppData/Local/Microsoft/Windows/Fonts/Font Awesome 6 Brands-Regular-400.otf")
showtext::showtext_opts(dpi = 300)
showtext::showtext_auto(enable = TRUE)

github_icon <- "&#xf09b"
linkedin_icon <- "&#xf0e1"
x_icon <- "&#xf099"
instagram_icon <- "&#xf16d"
github_username <- "italomarquesmonteiro"
linkedin_username <- "italomarquesmonteiro"
x_username <- "italommonteiro"
instagram_username <- "italo.m.m"

bg <- "white"
txt_col <- "grey70"
colors <- "gray40"
title_text <- glue::glue("Abate Bovino:")
subtitle_text <- glue::glue("Evolução anual de abate empilhado por trimestre ao longo da série histórica")
caption_text <- glue::glue(
  "**Dados:**  IBGE [Diretoria de Pesquisas Agropecuárias, Coordenação de Agropecuária, Pesquisa Trimestral do Abate de Animais(2024)]<br>",
  "**Nota:** *Até o terceiro trimestre de 2024.<br>",
  "**Plot:** Ítalo Marques-Monteiro <br><br>",
  "<span style='font-family:\"Font Awesome 6 Brands\"; color: white;'>{github_icon};</span> 
  <span style='color: grey70'>{github_username}</span><br>",
  "<span style='font-family:\"Font Awesome 6 Brands\"; color: skyblue;'>{linkedin_icon};</span> 
  <span style='color: grey70'>{linkedin_username}</span><br>",
  "<span style='font-family:\"Font Awesome 6 Brands\"; color: steelblue;'>{x_icon};</span>
  <span style='color: grey70'>{x_username}</span><br>",
  "<span style='font-family:\"Font Awesome 6 Brands\"; color: red;'>{instagram_icon};</span>
  <span style='color: grey70'>{instagram_username}</span>"
)



all_abate <- abate_carcaca |>
    dplyr::filter(variavel == "Animais abatidos") |>
    dplyr::mutate(ano_data = ymd(paste0(ano, "-01-01"))) |>
    ggplot(aes(x = ano_data, y = valor/1000000, fill = trimestre)) +
    geom_col() +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    scale_fill_manual(values = c(
        "1º trimestre" = "#d45087",
        "2º trimestre" = "#f95d6a",
        "3º trimestre" = "#ff7c43",
        "4º trimestre" = "#ffa600"
    )) +
  labs(
    title = title_text,
    subtitle = subtitle_text,
    caption = caption_text,
    x = "Ano",
    y = "Milhoes de Cab.",
    fill = "Trimestre"
  ) +
    theme(
      plot.title = ggtext::element_markdown(face = "bold", family = font3, size = 40, color = "gray80", hjust = 0.1),
      plot.subtitle = element_text(face = "italic", family = font2, size = 18, color = "gray70", hjust = 0.1),
      plot.caption = ggtext::element_markdown(face = "bold", family = font, size = 9, color = txt_col, hjust = 0, margin = margin(10, 20, 20, 20),  lineheight = 1.2),
      panel.background = element_rect(fill = "grey25", color = "grey25"),
      panel.grid = element_line(color = "grey25"),
      panel.grid.major.y = element_line(color = "grey50", size = 0.5, linetype = 3),
      panel.grid.minor.y = element_line(color = "grey60", size = 0.5, linetype = 3),
      plot.background = element_rect(fill = "grey25"),
      legend.position = "right",
      legend.background = element_blank(),
        legend.key = element_rect(fill = "grey95", color = "grey95"),
        legend.text = element_text(color = "grey40", size = 12),
        legend.title = element_text(face = "bold", color = "grey40"),
        axis.title.x = element_blank(),
        axis.title.y = ggtext::element_markdown(face = "bold", family = font, size = 15, color = "gray40"),
        axis.text.y = ggtext::element_markdown(face = "bold", family = font2, size = 10, color = "gray40"),
        axis.text.x = ggtext::element_markdown(face = "bold", family = font2, size = 13, color = "gray40", angle = 90, hjust = 1, vjust = 0.5),
        axis.ticks.length.x = unit(-0.8, "cm"),
        axis.ticks.x = element_line(color = "grey40", size = 1)
    ) +
    annotate(
        "text",
        x = ymd("2024-01-01"), # Posição no eixo x para o ano de 2024
        y = max(abate_carcaca$valor) / 100000000 + 13, # Posição no eixo y ajustada para o topo
        label = "*", # Símbolo a ser exibido
        size = 8,    # Tamanho do texto
        color = "gray80" # Cor do símbolo
    ) 
    #guides(fill = "none")
ggsave(
    ".github\.vscode\Trimestrais\\all_abate.png",
    plot = all_abate,
    width = 9,
    height = 10,
    dpi = 300)

abate_carcaca |>
  dplyr::filter(variavel == "Animais abatidos") |>
  dplyr::group_by(ano) |>
  dplyr::summarise(total_ano = sum(valor)) |> dplyr::arrange(desc(total_ano)) |>print(n = 28)