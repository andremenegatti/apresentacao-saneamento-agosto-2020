library(tidyverse)
library(sf)
library(tmap)
source('custom_map_settings.R')

# Reading geospatial data -----------------------------------------------------
codigos_uf <- c(11:17, 21:29, 31:33, 35, 41:43, 50:53)

mapa_mun <- str_c('data/polygons-municipios-uf-', codigos_uf, '.rds') %>% 
  map(.f = readRDS) %>% 
  reduce(.f = rbind) %>% 
  select(codmun = CD_GEOCMU, coduf = UF, geometry) %>% 
  mutate(codmun = str_extract(codmun, '.{6}') %>% as.numeric())

mapa_ufs <- readRDS('data/polygons-ufs-brasil.rds')

# Loading data from SNIS ------------------------------------------------------
df_snis <- 
  readxl::read_excel('data/desagregado-todos-prestadores-2018.xlsx') %>% 
  select(
    codmun = `Código do Município`,
    municipio = `Município`,
    estado = Estado,
    tipo_servico = `Tipo de serviço`,
    sigla_prestador = `Sigla do Prestador`,
    abrangencia = `Abrangência`,
    nat_jur = `Natureza jurídica`,
    pop_atendida_agua = 
      `AG001 - População total atendida com abastecimento de água`,
    pop_atendida_esgoto = 
      `ES001 - População total atendida com esgotamento sanitário`,
    ligacoes_ativas_agua = 
      `AG002 - Quantidade de ligações ativas de água`,
    ligacoes_ativas_esgoto = 
      `ES002 - Quantidade de ligações ativas de esgotos`
  )

# Cleaning step ---------------------------------------------------------------
# Creating separate dataframes for water and sewage
# If there is more than one provider, we only consider the most important one
# (i.e., the provider that serves more people)
df_agua <- df_snis %>% 
  filter(tipo_servico %in% c('Água', 'Água e Esgoto')) %>% 
  group_by(codmun) %>% 
  arrange(desc(pop_atendida_agua)) %>% 
  slice(1) %>% 
  ungroup()

df_esgoto <- df_snis %>% 
  filter(tipo_servico %in% c('Esgotos', 'Água e Esgoto')) %>% 
  group_by(codmun) %>% 
  arrange(desc(pop_atendida_esgoto)) %>% 
  slice(1) %>% 
  ungroup()

# Data wrangling: custom provider categories ---------------------------------- 
df_agua <- mapa_mun %>% 
  left_join(df_agua, by = 'codmun') %>% 
  mutate_at(.vars = vars(municipio, estado, tipo_servico,
                      sigla_prestador, abrangencia, nat_jur),
            .funs = function(x) ifelse(is.na(x), 'Sem dados', x)) %>% 
  mutate(tipo_prestador = case_when(
    abrangencia == 'Regional' ~ 'Regional',
    nat_jur == 'Empresa privada' ~ 'Empresa privada',
    nat_jur != 'Sem dados' ~ 'Público local',
    nat_jur == 'Sem dados' ~ 'Sem dados'
    ))

df_esgoto <- mapa_mun %>% 
  left_join(df_esgoto, by = 'codmun') %>% 
  mutate_at(.vars = vars(municipio, estado, tipo_servico,
                         sigla_prestador, abrangencia, nat_jur),
            .funs = function(x) ifelse(is.na(x), 'Sem dados', x)) %>% 
  mutate(tipo_prestador = case_when(
    abrangencia == 'Regional' ~ 'Regional',
    nat_jur == 'Empresa privada' ~ 'Empresa privada',
    nat_jur != 'Sem dados' ~ 'Público local',
    nat_jur == 'Sem dados' ~ 'Sem dados'
  ))

# Plotting: water providers  --------------------------------------------------
mapa_prestadores_agua <- df_agua %>% 
  mutate(`Tipo de prestador` = tipo_prestador) %>% 
  tm_shape() +
  tm_style("beaver") +
  tm_fill(
    'Tipo de prestador',
    alpha = 1,
    palette = c('#386cb0', '#e31a1c', '#fed976', '#bdbdbd'),
    id = "codmun"
  ) +
  tm_layout(main.title = 'Tipos de prestadores - Água - 2018') +
  tm_layout(main.title.size = 1.2, fontfamily = 'serif', scale = 1.1,
            main.title.fontface = 'bold', bg.color = "white",
            inner.margins = c(.1, .1, .1, .1)) +
  tm_compass(north = 0, type = "8star",size = 2,
             position = c("right", "top")) +
  tm_legend(legend.position = c(0.01,0.08)) +
  tm_shape(mapa_ufs) +
  tm_borders(col = "black", lwd = 0.2)

# tmap_save(mapa_prestadores_agua, height = 5, width = 5,
#           filename = 'plots/mapa-prestadores-agua.png')

# Plotting: sewage providers  -------------------------------------------------
mapa_prestadores_esgoto <- df_esgoto %>% 
  mutate(`Tipo de prestador` = tipo_prestador) %>% 
  tm_shape() +
  tm_style("beaver") +
  tm_fill(
    'Tipo de prestador',
    alpha = 1,
    palette = c('#386cb0', '#e31a1c', '#fed976', '#bdbdbd'),
    id = "codmun"
  ) +
  tm_layout(main.title = 'Tipos de prestadores - Esgoto - 2018') +
  tm_layout(main.title.size = 1.2, fontfamily = 'serif', scale = 1.1,
            main.title.fontface = 'bold', bg.color = "white",
            inner.margins = c(.1, .1, .1, .1)) +
  tm_compass(north = 0, type = "8star",size = 2,
             position = c("right", "top")) +
  tm_legend(legend.position = c(0.01,0.08)) +
  tm_shape(mapa_ufs) +
  tm_borders(col = "black", lwd = 0.2)

# tmap_save(mapa_prestadores_esgoto, height = 5, width = 5,
#           filename = 'plots/mapa-prestadores-esgoto.png')
