library(tidyverse)
library(tmap)
library(sf)
library(cagedExplorer)
source('custom_map_settings.R')

mapa_ufs <- readRDS('data/polygons-ufs-brasil.rds')
atend_agua <- readxl::read_excel('data/atendimento-agua-agregado-ufs.xlsx')
atend_esgoto <- readxl::read_excel('data/atendimento-esgoto-agregado-ufs.xlsx')

df_ufs <- mapa_ufs %>% 
  inner_join(atend_agua, by = c('NM_ESTADO' = 'uf_nome')) %>% 
  inner_join(atend_esgoto,
             by = c('NM_ESTADO' = 'uf_nome',
                    'uf_sigla', 'regiao'))

# Atendimento Total de Água ---------------------------------------------------
mapa_atend_total_agua <- df_ufs %>% 
  mutate(`População atendida` = in055) %>% 
  tm_shape() +
  tm_style(
    "beaver",
    legend.format = 
      list(
        fun = function(x) str_c(formatC(x, big.mark = '.', decimal.mark = ','),
                                '%'),
        text.separator = " a "
        )
  ) +
  tm_fill(
    'População atendida',
    style = 'fixed',
    breaks = c(0, 60, 75, 85, 90, 95, 100),
    palette = c("#FEE0D2", "#F7FBFF", "#C6DBEF",
                "#6BAED6", "#2171B5", "#08306B"),
    alpha = 1,
    id = "NM_ESTADO"
  ) +
  tm_layout(main.title = 'Índice de Atendimento Total de Água - 2018') +
  custom_map_settings ; mapa_atend_total_agua

# tmap_save(mapa_atend_total_agua,  height = 5, width = 5,
#           filename = 'plots/mapa-atendimento-total-agua.png')

# Atendimento Urbano de Água --------------------------------------------------
mapa_atend_urb_agua <- df_ufs %>% 
  mutate(`Pop. urbana atendida` = in023) %>% 
  tm_shape() +
  tm_style(
    "beaver",
    legend.format = 
      list(
        fun = function(x) str_c(formatC(x, big.mark = '.', decimal.mark = ','),
                                '%'),
        text.separator = " a "
        )
  ) +
  tm_fill(
    'Pop. urbana atendida',
    style = 'fixed',
    breaks = c(0, 60, 75, 85, 90, 95, 100),
    palette = c("#FEE0D2", "#F7FBFF", "#C6DBEF",
                "#6BAED6", "#2171B5", "#08306B"),
    alpha = 1,
    id = "NM_ESTADO"
  ) +
  tm_layout(main.title = 'Índice de Atendimento Urbano de Água - 2018') +
  custom_map_settings ; mapa_atend_urb_agua

# tmap_save(mapa_atend_urb_agua,  height = 5, width = 5,
#           filename = 'plots/mapa-atendimento-urb-agua.png')

# Atendimento Total de Esgoto -------------------------------------------------
mapa_atend_total_esgoto <- df_ufs %>% 
  mutate(`População atendida` = in056) %>% 
  tm_shape() +
  tm_style(
    "beaver",
    legend.format = 
      list(
        fun = function(x) str_c(formatC(x, big.mark = '.', decimal.mark = ','),
                                '%'),
        text.separator = " a "
      )
  ) +
  tm_fill(
    'População atendida',
    style = 'fixed',
    breaks = c(0, 20, 40, 60, 80, 90, 100),
    palette = c("#EF3B2C", "#FC9272", "#FEE0D2",
                "#9ECAE1", "#2171B5", "#08306B"),
    alpha = 1,
    id = "NM_ESTADO"
  ) +
  tm_layout(main.title = 'Índice de Atendimento Total de Esgoto - 2018') +
  custom_map_settings ; mapa_atend_total_esgoto

# tmap_save(mapa_atend_total_esgoto,  height = 5, width = 5,
#           filename = 'plots/mapa-atendimento-total-esgoto.png')

# Atendimento Urbano de Esgoto ------------------------------------------------
mapa_atend_urb_esgoto <- df_ufs %>% 
  mutate(`Pop. urbana atendida` = in024) %>% 
  tm_shape() +
  tm_style(
    "beaver",
    legend.format = 
      list(
        fun = function(x) str_c(formatC(x, big.mark = '.', decimal.mark = ','),
                                '%'),
        text.separator = " a "
      )
  ) +
  tm_fill(
    'Pop. urbana atendida',
    style = 'fixed',
    breaks = c(0, 20, 40, 60, 80, 90, 100),
    palette = c("#EF3B2C", "#FC9272", "#FEE0D2",
                "#9ECAE1", "#2171B5", "#08306B"),
    alpha = 1,
    id = "NM_ESTADO"
  ) +
  tm_layout(main.title = 'Índice de Atendimento Urbano de Esgoto - 2018') +
  custom_map_settings ; mapa_atend_urb_esgoto

# tmap_save(mapa_atend_urb_esgoto,  height = 5, width = 5,
#           filename = 'plots/mapa-atendimento-urb-esgoto.png')

# Tratamento de esgoto --------------------------------------------------------
mapa_tratamento_esgoto <- df_ufs %>% 
  mutate(`Índice de trat. de esgoto` = in016) %>% 
  tm_shape() +
  tm_style(
    "beaver",
    legend.format = 
      list(
        fun = function(x) formatC(x, big.mark = '.', decimal.mark = ','),
        text.separator = " a "
      )
  ) +
  tm_fill(
    'Índice de trat. de esgoto',
    style = 'fixed',
    # n = 6,
    breaks = c(40, 60, 85, 90, 95, 100),
    palette = c("#FCBBA1", "#F7FBFF",
                "#C6DBEF", "#6BAED6", "#2171B5"),
    alpha = 1,
    id = "NM_ESTADO"
  ) +
  tm_layout(main.title = 'Índice de Tratamento de Esgoto - 2018') +
  custom_map_settings ; mapa_tratamento_esgoto

# tmap_save(mapa_tratamento_esgoto,  height = 5, width = 5,
#           filename = 'plots/mapa-tratamento-esgoto.png')