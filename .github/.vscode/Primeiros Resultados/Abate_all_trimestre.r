#

library(tidyverse)

todos <- sidrar::get_sidra(api = "/t/1092/n1/all/v/284,285/p/all/c12716/115236/c18/992/c12529/118225") |>
    janitor::clean_names() |>
    tibble::as_tibble() |>
    dplyr::glimpse()


# Todos segundos trimestres da série
todos <- todos |>
  dplyr::select(valor, variavel, trimestre_codigo, trimestre) |>
  dplyr::relocate(trimestre, trimestre_codigo, variavel, valor) |>
  dplyr::filter(stringr::str_detect(trimestre, "2º trimestre"))


# Segundo trimestre de 2024
base_2t24 <- sidrar::get_sidra(api = "/t/6829/n1/all/v/all/p/last%201/c12716/115236/c79/41107") |>
    janitor::clean_names() |>
    tibble::as_tibble() |>
    dplyr::glimpse()


base_2t24 <- base_2t24 |>
    dplyr::select(valor, variavel, trimestre_codigo, trimestre) |>
    dplyr::relocate(trimestre, trimestre_codigo, variavel, valor)


todos_segundo_tri <- todos |>
    dplyr::full_join(base_2t24) |>
    dplyr::mutate(
        ano = str_extract(trimestre, "\\d{4}"),
        ano = as.numeric(ano)) |>
    dplyr::filter(variavel == "Animais abatidos")



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

title_text <- glue::glue('**Abate bovino:**') # nolint
subtitle_text <- glue::glue("**Os primeiros resultados indicam a maior quantidade de bovinos enviados<br>para abate já registrada em um 2º trimestre ao longo da série histórica.**")
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

    
media_periodo <- mean(todos_segundo_tri$valor, na.rm = TRUE)
   
plot_all2t_abate <- todos_segundo_tri |> #print(n = 28)
    #dplyr::mutate(ano_data = ymd(paste0(ano, "-01-01"))) |>
    ggplot(aes(ano_data, valor/1000000)) +
    ggchicklet::geom_chicklet(
        fill = "#fd9850",
        radius = grid::unit(3, "mm"),
        colour = "grey40",
        size = 2
    ) +
    geom_hline(yintercept = media_periodo/1000000, lty = 3, color = "grey20", size = 0.8) +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    labs(
        title = title_text,
        subtitle = subtitle_text,
        caption = caption_text,
        y = "Milhões de cab."
    ) +
    theme(
        plot.title = ggtext::element_markdown(face = "bold", family = font3, size = 35, hjust = 0, color = "grey30"),
        plot.subtitle = ggtext::element_markdown(face = "italic", family = font, size = 25, hjust = 0, color = "#fd9850"),
        plot.caption = ggtext::element_markdown(family = font, hjust = 0, margin = margin(10,0,0,0), size = 12, color = txt_col, lineheight = 1.2),
        panel.background = element_rect(fill = "white", color = "grey90"),
        plot.background = element_rect(fill = "white"),
        panel.grid = element_line(color = "grey90"),
        legend.background = element_blank(),
        legend.key = element_rect(fill = "grey95", color = "grey95"),
        legend.text = element_text(color = "grey40", size = 12),
        legend.title = element_text(face = "bold", color = "grey40"),
        axis.title.x = element_blank(),
        axis.title.y = ggtext::element_markdown(face = "bold", family = font, size = 15, color = "gray40"),
        axis.text.y = ggtext::element_markdown(face = "bold", family = font2, size = 10, color = "gray40"),
        axis.text.x = ggtext::element_markdown(face = "bold", family = font2, size = 13, color = "gray40", angle = 90, hjust = 1, vjust = 0.5),
        axis.ticks.length.x = unit(-0.95, "cm"),
        axis.ticks.x = element_line(color = "grey40", size = 1)
    )

ggsave(
    ".github\\.vscode\\Images\\Temporary\\abate_preliminar_all_2tri.png",
    plot = plot_all2t_abate,
    dpi = 300)