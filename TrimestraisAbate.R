# Biblioteca
library(tidyverse)

# Obtendo dados da pesquisa de abate
abate_br  <- sidrar::get_sidra(
    api = "/t/6829/n1/all/v/all/p/all/c12716/115236/c79/all"
    ) |>
    janitor::clean_names("snake") |>
    dplyr::glimpse()

# Manipulação e formatação dos dados
abate <- abate_br |>
    dplyr::select(
        valor,
        tipo_de_rebanho,
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
        tipo_de_rebanho == "Bovinos" & trimestre %in% c(
            "4º trimestre 2022", "3º trimestre 2023","4º trimestre 2023")
    ) |> 
    dplyr::arrange(
        desc(
            cabeca)
    )

# Configurando cores, título e subtítulo para o gráfico
colors <- c("#929d37", "#064a81")  # Corrigindo a inicialização do vetor colors
title_text <- glue::glue('Quantitativo de <span style="color:{colors[2]}">**cabeças bovinas**</span> guiadas<br>para abate no 4º trimestre de 2023') # nolint
subtitle_text <- glue::glue("")
caption_text <- glue::glue('**Plot:** **@italo.m.m**<br>**Dados:** IBGE [Diretoria de Pesquisas Agropecuárias, Coordenação de Agropecuária, Pesquisa Trimestral do Abate de Animais(2024)]<br>**Nota:** Os dados do ano de 2023 são preliminares até a divulgação dos dados do 1º trimestre de 2024. **Linha contínua** representa o valor médio do período') # nolint

# Criando o gráfico
grafico_abt <- abate |>
  dplyr::filter(
    tipo_de_rebanho == "Bovinos"
  ) |>
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
    )
  ) +
   scale_fill_manual(
      values = c(
          high = "#064a81",
          default = "grey90"
      )
   ) +
  annotate(
    "segment",
    x = 9.05e6,
    xend = 9.05e6,
    y = 0, yend = 21,
    linetype = 3,
    size = 0.5,
    color = "#064a81"
    ) +
  annotate(
    "segment",
    x = 8.85e6,
    xend = 8.85e6,
    y = 0,
    yend = 20,
    linetype = 3,
    size = 0.5,
    color = "#064a81"
  ) +
  annotate(
    "segment",
    x = 7.44e6,
    xend = 7.44e6,
    y = 0,
    yend = 17,
    linetype = 3,
    size = 0.5,
    color = "#064a81"
  ) +
     geom_vline(
        xintercept = 7607599.,
        lty = 1,
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
    theme(
        legend.position = "none",
        plot.title = ggtext::element_markdown(face = "bold", family = "Source Sans Pro", size = 48, hjust = 0, color = "gray40",), # nolint
        plot.subtitle = ggtext::element_markdown(face = "bold", family = "Fira Sans Pro", size = 15, color = "gray50", hjust = 0.1), # nolint
        plot.caption = ggtext::element_markdown(face = "italic", family = "Fira Sans Pro", size = 12, color = "gray50"), # nolint
        axis.text.y = ggtext::element_markdown(face = "italic", family = "Fira Sans Pro", size = 10, color = "gray50"),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        panel.background = element_rect(fill = "white", color = "white"),
        plot.background = element_rect(fill = "white"),
        line = element_blank(),
        axis.text.x = ggtext::element_markdown(face = "bold", family = "Fira Sans",size = 8, color = "gray50", angle = 0, hjust = 1, vjust = 1)
    ) +
    annotate(
        "text",
        x = 8320000,
        y = 21,
        label = "9,05 milhões de cabeças",
        size = 4.5, color = "white"
    ) +
    annotate(
        "text", x = 8700000,
        y = 20,
        label = "8,39",
        size = 4.5,
        color = "white"
    ) +
    annotate(
        "text",
        x = 7300000,
        y = 17,
        label = "7,54",
        size = 4.5,
        color = "white"
    )
grafico_abt

# Salvando o gráfico
ggsave("Images/abate_preliminar_4tri23.png", plot = grafico_abt, dpi = 300)