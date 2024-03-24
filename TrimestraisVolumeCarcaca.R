# Biblioteca
library(tidyverse)

# Obtendo dados da pesquisa de abate
abate_br  <- sidrar::get_sidra(
    api = "/t/1092/n1/all/v/284,285/p/last%2021/c12716/115236/c18/992/c12529/118225"
    ) |>
    janitor::clean_names("snake") |>
    dplyr::glimpse()

# Manipulação e formatação dos dados
abate <- abate_br |>
    dplyr::select(
        valor,
        #tipo_de_rebanho,
        data_trimestre = trimestre_codigo,
        trimestre,
        variavel
    ) |>
    dplyr::mutate(
        data_trimestre = lubridate::yq(data_trimestre),
        ano = lubridate::year(data_trimestre)
    ) |>
    tidyr::pivot_wider(
        names_from = variavel,
        values_from = valor
    ) |>
    dplyr::rename(
        cabeca = `Animais abatidos`,
        carcaca = `Peso total das carcaças`
    ) |>
    dplyr::mutate(
        peso_car = carcaca / cabeca
    ) |>
    dplyr::glimpse()

# Filtrando e organizando dados específicos
abate |>
    dplyr::filter(
        #tipo_de_rebanho == "Bovinos" & 
        trimestre %in% c(
            "4º trimestre 2022", "3º trimestre 2023","4º trimestre 2023")
    ) |> 
    dplyr::arrange(
        desc(
            carcaca)
    )

font <- "Josefin Sans"
font2 <- "Open Sans"

# Use the font_add_google() function to load fonts from the web
sysfonts::font_add_google(family = font, font, db_cache = FALSE)
sysfonts::font_add_google(family = font2, font2, db_cache = FALSE)

sysfonts::font_add(family = "Font Awesome 6 Brands", regular = "C:/Users/italo/AppData/Local/Microsoft/Windows/Fonts/Font Awesome 6 Brands-Regular-400.otf") # nolint

theme_set(theme_minimal(base_family = font2, base_size = 3))

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

# Configurando cores, título e subtítulo para o gráfico
bg <- "white"
txt_col <- "black"
fundo <- ""
colors <- c("#929d37", "#064a81")
title_text <- glue::glue('O maior volume de <span style = "color:{colors[1]}">**toneladas de carcaça**</span><br> bovina produzidas na história')
subtitle_text <- glue::glue("")
caption_text <- glue::glue(
  "**Dados:**  IBGE [Diretoria de Pesquisas Agropecuárias, Coordenação de Agropecuária, Pesquisa Trimestral do Abate de Animais(2024)]<br>", # nolint
  "**Nota:** Os dados do ano de 2023 são preliminares até a divulgação dos dados do 1º trimestre de 2024. **Linha pontilhada** representa o valor médio do período<br>",
  "**Plot:** Ítalo Marques-Monteiro <br><br>",
  "<span style='font-family:\"Font Awesome 6 Brands\"; color: black;'>{github_icon};</span> 
  <span style='color: black'>{github_username}</span><br>",
  "<span style='font-family:\"Font Awesome 6 Brands\"; color: #1a96fc;'>{linkedin_icon};</span> 
  <span style='color: black'>{linkedin_username}</span><br>",
  "<span style='font-family:\"Font Awesome 6 Brands\"; color: steelblue;'>{x_icon};</span>
  <span style='color: black'>{x_username}</span><br>",
  "<span style='font-family:\"Font Awesome 6 Brands\"; color: #fd5257;'>{instagram_icon};</span>
  <span style='color: black'>{instagram_username}</span>"
)

"#333334"
# Criando o gráfico
grafico_carcaca <- abate |>
    #dplyr::filter(tipo_de_rebanho == "Bovinos") |>
    #dplyr::summarise(media = mean(carcaca), mediana = median(carcaca)) |>
    ggplot2::ggplot( aes(x = trimestre, y = carcaca)
    ) +
    ggchicklet::geom_chicklet(
        aes(fill = ifelse(trimestre %in% c(
                    "4º trimestre 2022",
                    "3º trimestre 2023",
                    "4º trimestre 2023"),
                "high", "default")),
        radius = grid :: unit(3, "mm")
    ) +
    scale_fill_manual(
        values = c(
            high = "#929d37",
            default = "grey70"
        )
    ) +
     geom_hline(yintercept = 2025629030., lty = 3, color = "gray30") +
    scale_y_continuous(labels = scales::comma) +
    annotate("text", label = "2,43\nmilhões/ton.", x = 20.5, y = 2550000000, size = 4.5, family = font, fontface = "bold", colour = "#929d37") +
    annotate("text", label = "2,39", x = 20, y = 2350000000, size = 3, colour = "white", family = font, fontface = "bold") +
    annotate("text", label = "2,03", x = 17, y = 1990000000, size = 3, colour = "white", family = font, fontface = "bold") +
    scale_x_discrete(
        limits = c(
            "4º trimestre 2018",
            "1º trimestre 2019",
            "2º trimestre 2019",
            "3º trimestre 2019",
            "4º trimestre 2019",
            "1º trimestre 2020",
            "2º trimestre 2020",
            "3º trimestre 2020",
            "4º trimestre 2020",
            "1º trimestre 2021",
            "2º trimestre 2021",
            "3º trimestre 2021",
            "4º trimestre 2021",
            "1º trimestre 2022",
            "2º trimestre 2022",
            "3º trimestre 2022",
            "4º trimestre 2022",
            "1º trimestre 2023",
            "2º trimestre 2023",
            "3º trimestre 2023",
            "4º trimestre 2023"
            )
        ) +
    guides(
        fill = "none"
    ) +
    labs(
        title = title_text,
        subtitle = subtitle_text,
        caption = caption_text
    ) +
    theme(
        legend.position = "none",
        plot.title = ggtext::element_markdown(face = "bold", family = font, color = "gray40", size = 40, hjust = 0.1),
        plot.subtitle = element_text(family = font2, size = 15, color = "gray40", hjust = 0.1),
        plot.caption = ggtext::element_markdown(family = font2, hjust = 0, margin = margin(10,0,0,0), size = 8, color = txt_col, lineheight = 1.2), # nolint
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = ggtext::element_markdown(face = "bold", family = font2, size = 8, color = "gray50", angle = 90, hjust = 1, vjust = 1), # nolint
        axis.title.y = element_blank(),
        plot.margin = margin(rep(15, 4)),
        panel.background = element_rect(fill =  "white", color =  "white"),
        #panel.grid = element_line(color = "grey70", linetype = 3, size = 0.5),
        plot.background = element_rect(fill = "white")
        )
grafico_carcaca

# Salvando o gráfico
ggsave(
    ".vscode/Images/Trimestrais/carcaca_4tri23.png",
    plot = grafico_carcaca,
    dpi = 300,
    height = 9,
    width =  15
    )
