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
colors <- c("#929d37", "#064a81", "mediumpurple1")
title_text <- glue::glue('<span style = "color:{colors[3]}">**Peso da carcaça bovina**</span><br>entre os trimestre de 2021 a 2023')
subtitle_text <- glue::glue("")
caption_text <- glue::glue('**Dados preliminares:** IBGE [Diretoria de Pesquisas Agropecuárias, Coordenação de Agropecuária, Pesquisa Trimestral do Abate de Animais(2023)] | **Plot:** Ítalo Monteiro')

# Criando o gráfico
grafico_peso_car <- abate |>
    dplyr::filter(
        tipo_de_rebanho == "Bovinos" &
            trimestre >= "1º trimestre 2021"
    ) |>
    #dplyr::summarise(media = mean(peso_car), mediana = median(peso_car)) |>
    ggplot2::ggplot(
        aes(
            x = trimestre,
                y = peso_car
        )
    ) +
    ggchicklet::geom_chicklet(
        aes(
            fill = ifelse(
                trimestre %in% c(
                    "3º trimestre 2022",
                        "2º trimestre 2023",
                            "3º trimestre 2023"),
                                "high", "default"
            )
        ),
        radius = grid :: unit(3, "mm")) +
    scale_fill_manual(
        values = c(
            high = "mediumpurple1",
                default = "grey70"
        )
    ) +
     geom_hline(
        yintercept = 264,
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
        #plot.margin = margin(rep(15, 4)),
        panel.background = element_rect(fill = "white", color = "white"),
        plot.background = element_rect(fill = "white"),
        line = element_blank()#,
        #rect = element_blank()
        ) +
    annotate("text", label = "266 kg/cab", x = 11, y = 270, size = 5, family = "Fira Sans Pro", colour = "mediumpurple1") +
    annotate("text", label = "260", x = 10, y = 268, size = 4, colour = "gray45") +
    annotate("text", label = "272", x = 7, y = 275, size = 4, colour = "gray45") +
    scale_x_discrete(
        limits = c(
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
            "3º trimestre 2023"
            )
        ) +
    guides(
        fill = FALSE
    ) +
    annotate( # média:7541099 ; mediana:7382158
        "text",
            label = "Média do período",
                x = 1,
                    y = 270,
                        size = 4,
                            colour = "gray45"
    )
grafico_peso_car


# Salvando o gráfico
ggsave(
    "Images/abate_peso_car.png",
        plot = grafico_peso_car,
            dpi = 500,
                width = 13.8,
                    height = 9
    )