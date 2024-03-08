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
            carcaca)
    )

# Configurando cores, título e subtítulo para o gráfico
colors <- c("#929d37", "#064a81")
title_text <- glue::glue('O maior volume de <span style = "color:{colors[1]}">**toneladas de carcaça**</span><br> bovina produzidas na história')
subtitle_text <- glue::glue("")
caption_text <- glue::glue('**Plot:** @italo.m.m<br>**Dados preliminares:** IBGE [Diretoria de Pesquisas Agropecuárias, Coordenação de Agropecuária, Pesquisa Trimestral do Abate de Animais(2023)]')

# Criando o gráfico
grafico_carcaca <- abate |>
    dplyr::filter(
        tipo_de_rebanho == "Bovinos" 
    ) |>
    #dplyr::summarise(media = mean(carcaca), mediana = median(carcaca)) |>
    ggplot2::ggplot(
        aes(
            x = trimestre,
            y = carcaca
        )
    ) +
    ggchicklet::geom_chicklet(
        aes(
            fill = ifelse(
                trimestre %in% c(
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
     geom_hline(
        yintercept = 2064447993.,
        lty = 3,
        color = "gray30"
    ) +
    scale_y_continuous(
        labels = scales::comma
    ) +
    labs(
        title = title_text,
        subtitle = subtitle_text,
        caption = caption_text
    ) +
    theme(
        plot.title = ggtext::element_markdown(face = "bold", family = "Source Sans Pro", size = 48, hjust = 0, color = "gray40",), # nolint
        plot.subtitle = ggtext::element_markdown(face = "bold", family = "Fira Sans Pro", size = 15, color = "gray50", hjust = 0.1), # nolint
        plot.caption = ggtext::element_markdown(face = "italic", family = "Fira Sans Pro", size = 12, color = "gray50"), # nolint
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        panel.background = element_rect(fill = "white", color = "white"),
        plot.background = element_rect(fill = "white"),
        line = element_blank()#,
        ) +
    annotate("text", label = "2,41", x = 21, y = 2500000000, size = 5, family = "Fira Sans Pro", colour = "#929d37") +
    annotate("text", label = "2,38", x = 20, y = 2400000000, size = 4, colour = "gray45") +
    annotate("text", label = "2,03", x = 17, y = 2100000000, size = 4, colour = "gray45") +
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
    annotate( # média:1 986 839 568 ; mediana:1 984 748 758
        "text",
        label = "Média do período",
        x = 2,
        y = 2100000000.,
        size = 4,
        colour = "gray45"
    )
grafico_carcaca

# Salvando o gráfico
ggsave(
    ".vscode/Images/carcaca_preliminar_4tri23.png",
    plot = grafico_carcaca,
    dpi = 1200,
    #height = 9,
    #width =  15
    )
