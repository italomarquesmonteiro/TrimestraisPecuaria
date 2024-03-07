#    title: "Abate bovino no  3º trimestre 2023 "
#    description: "Quantitativo por categoria atualizado em  09 de novembro de 2023 pelo IBGE"
#    author: 
#        - name: "Ítalo Marques-Monteiro"
#        - email: "italo.marques.monteiro@outlook.com"
#        - github: https://github.com/italomarquesmonteiro
#        - kaggle: https://www.kaggle.com/talomarquesmonteiro
#        - linkedin: linkedin.com/in/ítalo-monteiro-0b2516144
#        - lattes: http://lattes.cnpq.br/7174985368517137
#        - orcid_id: https://orcid.org/0000-0003-4367-1034
#        - affiliation: Smart Data  

#    date: "2023-09-11"



library(tidyverse)

abate_br  <- sidrar::get_sidra(api = "/t/6829/n1/all/v/all/p/all/c12716/115236/c79/all") |>
    janitor::clean_names("snake") |>
    dplyr::glimpse()

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
    tidyr::pivot_wider(names_from = variavel, values_from = valor) |>
    dplyr::rename(
        cabeca = `Animais abatidos`, carcaca = `Peso total das carcaças`) |>
    dplyr::mutate(peso_car = carcaca / cabeca) |>
    dplyr::glimpse()




abate |>
    dplyr::filter(tipo_de_rebanho == "Bovinos" & trimestre %in% c(
        "4º trimestre 2022", "3º trimestre 2023","4º trimestre 2023")) |> # nolint
    #dplyr::mutate()
    dplyr::arrange(desc(cabeca))







colors <- c("#929d37", "#064a81")  # Corrigindo a inicialização do vetor colors
title_text <- glue::glue('Quantitativo de <span style="color:{colors[2]}">**cabeças bovinas**</span> guiadas<br>para abate no 4º trimestre de 2023') # nolint
subtitle_text <- glue::glue("")
caption_text <- glue::glue('**Plot:** **@italo.m.m**<br>**Dados:** IBGE [Diretoria de Pesquisas Agropecuárias, Coordenação de Agropecuária, Pesquisa Trimestral do Abate de Animais(2024)]<br>**Nota:** Os dados do ano de 2023 são preliminares até a divulgação dos dados do 1º trimestre de 2024. **Linha contínua** representa o valor médio do período') # nolint


grafico_abt <- abate |>
  dplyr::filter(
    tipo_de_rebanho == "Bovinos"
  ) |>
  #dplyr::summarise(media = mean(cabeca), mediana = median(cabeca)) |>
  ggplot2::ggplot(
    aes(
      x = cabeca, y = trimestre
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
    y = 0,
    yend = 21,
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
        xintercept = 7607599., # 7541099,
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
        axis.text.x = ggtext::element_markdown(
          face = "bold", family = "Fira Sans",size = 8, color = "gray50", angle = 0, hjust = 1, vjust = 1) # nolint
        ) +
   annotate(
    "text",
    x = 8320000,
    y = 21,
    label = "9,05 milhões de cabeças",
    size = 4.5,
    color = "white"
  ) +
  annotate(
    "text",
    x = 8700000,
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



ggsave(
    "Images/abate_preliminar_4tri23.png",
        plot = grafico_abt,
            dpi = 300,
                #width = 15.5,
                 #height = 9
    )






colors <- c("#929d37", "#064a81")
title_text <- glue::glue('O maior volume de <span style = "color:{colors[1]}">**toneladas de carcaça**</span><br> bovina produzidas na história')
subtitle_text <- glue::glue("")
caption_text <- glue::glue('**Dados preliminares:** IBGE [Diretoria de Pesquisas Agropecuárias, Coordenação de Agropecuária, Pesquisa Trimestral do Abate de Animais(2023)] | **Plot:** Ítalo Monteiro')

grafico_carcaca <- abate |>
    dplyr::filter(
        tipo_de_rebanho == "Bovinos" &
            trimestre >= "1º trimestre 2021"
    ) |>
    #dplyr::summarise(media = mean(peso_car), mediana = median(peso_car)) |>
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
                    "3º trimestre 2022",
                        "2º trimestre 2023",
                            "3º trimestre 2023"),
                                "high", "default"
            )
        ),
        radius = grid :: unit(3, "mm")) +
    scale_fill_manual(
        values = c(
            high = "#929d37",
                default = "grey70"
        )
    ) +
     geom_hline(
        yintercept = 1986839568,
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
    annotate("text", label = "2,36 milhões", x = 11, y = 2388203437, size = 5, family = "Fira Sans Pro", colour = "#929d37") +
    annotate("text", label = "2,17", x = 10, y = 2173810104, size = 4, colour = "gray45") +
    annotate("text", label = "2,16", x = 7, y = 2153264313, size = 4, colour = "gray45") +
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
    annotate( # média:1 986 839 568 ; mediana:1 984 748 758
        "text",
            label = "Média do período",
                x = 1,
                    y = 2020000000,
                        size = 4,
                            colour = "gray45"
    )
grafico_carcaca



ggsave(
    "Images/carcaca_preliminar_3tri23.png",
        plot = grafico_carcaca,
            dpi = 500,
                width = 13.8,
                    height = 9
    )



colors <- c("#929d37", "#064a81", "mediumpurple1")
title_text <- glue::glue('<span style = "color:{colors[3]}">**Peso da carcaça bovina**</span><br>entre os trimestre de 2021 a 2023')
subtitle_text <- glue::glue("")
caption_text <- glue::glue('**Dados preliminares:** IBGE [Diretoria de Pesquisas Agropecuárias, Coordenação de Agropecuária, Pesquisa Trimestral do Abate de Animais(2023)] | **Plot:** Ítalo Monteiro')
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



ggsave(
    "Images/abate_peso_car.png",
        plot = grafico_peso_car,
            dpi = 500,
                width = 13.8,
                    height = 9
    )

