source("rdocs/source/packages.R")

# ---------------------------------------------------------------------------- #

#        ______   _____  ________      ________ 
#      |  ____| / ____| |__   __| /\  |__   __|
#     | |__    | (___     | |   /  \    | |   
#    |  __|    \___ \    | |  / /\ \   | |   
#   | |____   ____) |   | |  /____ \  | |   
#  |______   |_____/   |_| /_/    \_\|_|   
#  
#         Consultoria estatística 
#

# ---------------------------------------------------------------------------- #
# ############################## README ###################################### #
# Consultor, favor utilizar este arquivo .R para realizar TODAS as análises
# alocadas a você neste projeto pelo gerente responsável, salvo instrução 
# explícita do gerente para mudança.
#
# Escreva seu código da forma mais clara e legível possível, eliminando códigos
# de teste depreciados, ou ao menos deixando como comentário. Dê preferência
# as funções dos pacotes contidos no Tidyverse para realizar suas análises.

---
title: "entrega_2"
format: html
editor: visual
---

---
# Nome do arquivo PDF gerado na pasta resultado
output-file: "Entrega 2"
---

{r setup}
#| include: false
source("rdocs/entrega_2.R")

{r}
#carregando os pacotes
library(tinytex)
library(readxl)
library(tidyverse)
library(ggplot2)

#importando a padronização da estat
theme_estat <- function(...) {
  theme <- ggplot2::theme_bw() +
    ggplot2::theme(
      axis.title.y = ggplot2::element_text(colour = "black", size = 12),
      axis.title.x = ggplot2::element_text(colour = "black", size = 12),
      axis.text = ggplot2::element_text(colour = "black", size = 9.5),
      panel.border = ggplot2::element_blank(),
      axis.line = ggplot2::element_line(colour = "black"),
      legend.position = "top",
      ...
    )
  
  return(
    list(
      theme,
      ggplot2::scale_fill_manual(values = estat_colors),
      ggplot2::scale_colour_manual(values = estat_colors)
    )
  )
}

estat_colors <- c(
"#A11D21", "#003366", "#CC9900",
"#663333", "#FF6600", "#CC9966",
"#999966", "#006606", "#008091",
"#041835", "#666666" )

{r}
# Caminho do arquivo Excel
caminho_old_town_road <- "C:/Users/fredn/OneDrive/Documents/relatorio_old_town_road.xlsx"

# Ver quais abas (sheets) existem no arquivo
excel_sheets(caminho_old_town_road)
infos_clientes <- read_excel(caminho_old_town_road, sheet = "infos_clientes")


  

{r}

clientes<- infos_clientes %>%
  mutate( Height_cm = Height_dm*10,
          Weight_kg = Weight_lbs/ 2.2046)
print(clientes)

#Aqui a regressão linear
modelo <- lm(Weight_kg ~ Height_cm, data = clientes )
summary(modelo)
cor(clientes$Height_cm,clientes$Weight_kg, method = "pearson")

#Gráfico de disperção
ggplot(clientes) +
  aes(x = Weight_kg, y = Height_cm) +
  geom_point(colour = "#A11D21", size = 0.97) +
  labs(
    x = "Peso (kg)",
    y = "Altura (cm)",
    title = "Gráfico de Dispersão: Peso vs Altura"
  ) +
  theme_estat()

ggsave("disp_uni.pdf", width = 158, height = 93, units = "mm")
















# ---------------------------------------------------------------------------- #
