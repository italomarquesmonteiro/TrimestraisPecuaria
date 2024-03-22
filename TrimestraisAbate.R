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
            cabeca)
    )


font <- "Josefin Sans"
font2 <- "Open Sans"

# Use the font_add_google() function to load fonts from the web
sysfonts::font_add_google(family=font, font, db_cache = FALSE)
sysfonts::font_add_google(family=font2, font2, db_cache = FALSE)

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
colors <- c("#929d37", "#1a96fc")  # Corrigindo a inicialização do vetor colors
title_text <- glue::glue('Quantitativo de <span style="color:{colors[2]}">**cabeças bovinas**</span> guiadas<br>para abate no 4º trimestre de 2023') # nolint
subtitle_text <- glue::glue("")
caption_text <- glue::glue(
  "**Dados:**  IBGE [Diretoria de Pesquisas Agropecuárias, Coordenação de Agropecuária, Pesquisa Trimestral do Abate de Animais(2024)]<br>", # nolint
  "**Nota:** Os dados do ano de 2023 são preliminares até a divulgação dos dados do 1º trimestre de 2024. **Linha contínua** representa o valor médio do período<br>",
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

bg <- "white"
txt_col <- "black"
fundo <- "grey20"


# Criando o gráfico
grafico_abt <- abate |>
  #dplyr::filter(
   # tipo_de_rebanho == "Bovinos"
 # ) |>
  #dplyr::summarise(media = mean(cabeca), mediana = median(cabeca)) |>
  ggplot2::ggplot(
    aes(
      x = cabeca,
      y = trimestre
    )
  ) +
  geom_col(
     aes(
        fill = ifelse(
            trimestre %in% c(
                "4º trimestre 2022",
                "3º trimestre 2023",
                "4º trimestre 2023"),
            "high", "default"
        )
    ), color = "grey50", size = 1, width = 0.8
  ) +
   scale_fill_manual(
      values = c(
          high = "#1a96fc",
          default = "grey90"
      )
   ) +
  annotate(
    "segment",
    x = 9153384,
    xend = 9153384,
    y = 0, yend = 21,
    linetype = 3,
    size = 0.5,
    color = "#1a96fc"
    ) +
  annotate(
    "segment",
    x = 8988798,
    xend = 8988798,
    y = 0,
    yend = 20,
    linetype = 3,
    size = 0.5,
    color = "#1a96fc"
  ) +

  annotate(
    "segment",
    x = 7544411,
    xend = 7544411,
    y = 0,
    yend = 17,
    linetype = 3,
    size = 0.5,
    color = "#1a96fc"
  ) +

 # Média
     geom_vline(
        xintercept = 7725280.,
        lty = 1,
        color = "grey50"
    ) +
    annotate(
        "text", 
        x = 7850000,
        y = 10,
        label = "7,72",
        family = font,
        color = "grey50"
    ) +
    scale_x_continuous(
        labels = scales::comma
    ) +
    labs(
        title = title_text,
        subtitle = subtitle_text,
        caption = caption_text
    ) +
    scale_y_discrete(
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
    
    annotate(
        "text",
        x = 8320000,
        y = 21,
        label = "9,15 milhões de cab.",
        fontface = "bold",
        family = font,
        size = 4.5, color = "white"
    ) +

    annotate(
        "text", x = 8700000,
        y = 20,
        label = "8,98",
        fontface = "bold",
        family = font,
        size = 4.5,
        color = "white"
    ) +

    annotate(
        "text",
        x = 7300000,
        y = 17,
        label = "7,54",
        fontface = "bold",
        family = font,
        size = 4.5,
        color = "white"
    ) +
    theme(
        legend.position = "none",
        plot.title = ggtext::element_markdown(face = "bold", family = font, color = "gray20", size = 40, hjust = 0.1),
        plot.subtitle = element_text(family = font2, size = 15, color = "gray40", hjust = 0.1),
        plot.caption = ggtext::element_markdown(family = font2, hjust = 0, margin = margin(10,0,0,0), size = 8, color = txt_col, lineheight = 1.2), # nolint
        axis.text.y = element_text(family = font2, face = "bold", color = "gray40", size = 12, hjust = 1),
       # axis.text.x = element_text(family = font2, face = "bold", color = "gray40", size = 12, hjust = 1),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        plot.margin = margin(rep(15, 4)),
        panel.background = element_rect(fill =  "white", color =  "white"),
        plot.background = element_rect(fill = "white")
        )
grafico_abt

# Salvando o gráfico
ggsave(".vscode/Images/Trimestrais/abate_cab_4tri23.png", plot = grafico_abt, width = 15, height = 9, dpi = 300)
