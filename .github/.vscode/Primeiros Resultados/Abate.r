#
library(tidyverse)

test <- sidrar::get_sidra(
    api = "/t/6829/n1/all/v/all/p/last%2014/c12716/115236/c79/41107" ) |>
    janitor::clean_names("snake") |>
    tibble::as.tibble() |>
    dplyr::glimpse()


font <- "Josefin Sans"
font2 <- "Open Sans"
font3 <- "Permanent Marker"
sysfonts::font_add_google(family=font, font, db_cache = FALSE)
sysfonts::font_add_google(family=font2, font2, db_cache = FALSE)
sysfonts::font_add_google(family=font3, font3, db_cache = FALSE)
sysfonts::font_add(family = "Font Awesome 6 Brands", regular = "C:/Users/italo/AppData/Local/Microsoft/Windows/Fonts/Font Awesome 6 Brands-Regular-400.otf") # nolint
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
txt_col <- "black"
colors <- "gray40"
fundo <- "white"

title_text <- glue::glue('**Abate bovino**') # nolint
title_text1 <- glue::glue('**ABATE BOVINO**') # nolint
subtitle_text <- glue::glue("**Primeiros resultados do 2º trimestre de 2024**")
caption_text <- glue::glue(
  "**Dados:**  IBGE [Diretoria de Pesquisas Agropecuárias, Coordenação de Agropecuária, Pesquisa Trimestral do Abate de Animais(2024)]<br>", # nolint
  #"**Nota:** As Unidades da Federação, Amapá, Distrito Federal e Paraíba não apresentaram dados<br>",
  "**Plot:** Ítalo Marques-Monteiro <br><br>",
  "<span style='font-family:\"Font Awesome 6 Brands\"; color: black;'>{github_icon};</span> 
  <span style='color: black'>{github_username}</span><br>",
  "<span style='font-family:\"Font Awesome 6 Brands\"; color: dodgerblue4;'>{linkedin_icon};</span> 
  <span style='color: black'>{linkedin_username}</span><br>",
  "<span style='font-family:\"Font Awesome 6 Brands\"; color: steelblue;'>{x_icon};</span>
  <span style='color: black'>{x_username}</span><br>",
  "<span style='font-family:\"Font Awesome 6 Brands\"; color: red;'>{instagram_icon};</span>
  <span style='color: black'>{instagram_username}</span>"
)


n_abate <- test |>
    dplyr::select(valor, variavel, trimestre_codigo, trimestre) |>
    dplyr::relocate(trimestre, trimestre_codigo, variavel, valor) |>
    dplyr::filter(variavel == "Animais abatidos") |>
    dplyr::mutate(trimestre = str_replace_all(trimestre, c(
        "1º trimestre 20" = "1T",
        "2º trimestre 20" = "2T",
        "3º trimestre 20" = "3T",
        "4º trimestre 20" = "4T"
    ))) 

# Filtrar tempo
#nome_novo <- nome_base |> dplyr::filter(nisso_daqui >="isso")
# Calcular a média do período
media_periodo <- mean(n_abate$valor, na.rm = TRUE)


plot_2t24_abate <- n_abate |>
    ggplot(aes(trimestre, valor)) +
    ggchicklet::geom_chicklet(
        aes(
            fill = case_when(
                trimestre == "2T24" ~ "ultimo",
                trimestre == "1T24" ~ "penultimo",
                trimestre == "2T23" ~ "anterior",
                TRUE ~ "default"
            )
        ),
        radius = grid::unit(3, "mm"),
        colour = "grey40",
        size = 2
    ) +
    geom_hline(yintercept = media_periodo, lty = 3, color = "black") +
    scale_fill_manual(
        values = c(
            ultimo = "dodgerblue",
            penultimo = "grey60",
            anterior = "grey60",
            default = "grey90"
        )
    ) +
    guides(fill = "none") +
    scale_x_discrete(
        limits = c(
            "1T21", "2T21", "3T21", "4T21",
            "1T22", "2T22", "3T22", "4T22",
            "1T23", "2T23", "3T23", "4T23",
            "1T24", "2T24"
        )
    ) +
    labs(
        title = title_text,
        subtitle = subtitle_text,
        caption = caption_text
    ) +
    theme(
        plot.title = ggtext::element_markdown(face = "bold", family = font3, size = 35, hjust = 0, color = "grey30"),
        plot.subtitle = ggtext::element_markdown(face = "italic", family = font, size = 25, hjust = 0, color = "dodgerblue"),
        plot.caption = ggtext::element_markdown(family = font, hjust = 0, margin = margin(10,0,0,0), size = 12, color = txt_col, lineheight = 1.2),
        panel.background = element_rect(fill = "white", color = "grey90"),
        plot.background = element_rect(fill = "white"),
        panel.grid = element_line(color = "grey90"),
        legend.background = element_blank(),
        legend.position = c(0.7, 0.8),
        legend.key = element_rect(fill = "grey95", color = "grey95"),
        legend.text = element_text(color = "grey40", size = 12),
        legend.title = element_text(face = "bold", color = "grey40"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = ggtext::element_markdown(face = "bold", family = font2, size = 13, color = "gray40", angle = 0),
        axis.ticks.length.x = unit(-0.8, "cm"),
        axis.ticks.x = element_line(color = "grey40", size = 1)
    ) +
    annotate(
        "text",
        y = 10500000,
        x = 14,
        label = "9.94\nmilhões de cab.",
        fontface = "bold",
        family = font,
        hjust = 0.6,
        size = 5, color = "dodgerblue"
    ) +
    annotate(
        "text", 
        y = 9600000,
        x = 13,
        label = "9.30",
        fontface = "bold",
        family = font,
        size = 4.5,
        color = "grey40"
    ) +
    annotate(
        "text",
        y =  8650000,
        x = 10,
        label = "8.47",
        fontface = "bold",
        family = font,
        size = 4.5,
        color = "grey40"
    )

ggsave(
    ".github\\.vscode\\Images\\Temporary\\abate_preliminar_2tri24.png",
    plot = plot_2t24_abate,
    dpi = 300)