library(tidyverse)
source('custom_theme.R')
theme_set(custom_theme())

atend_agua <- readxl::read_excel('data/atendimento-agua-agregado-ufs.xlsx')
atend_esgoto <- readxl::read_excel('data/atendimento-esgoto-agregado-ufs.xlsx')
perdas_agua <- readxl::read_excel('data/indice-perdas-agua-agregado-ufs.xlsx')
tarifas <- readxl::read_excel('data/tarifas-agregado-ufs.xlsx')

join_keys <- c('uf_nome', 'uf_sigla', 'regiao')

df_ufs <- atend_agua %>% 
  inner_join(atend_esgoto, by = join_keys) %>% 
  inner_join(perdas_agua, by = join_keys) %>% 
  inner_join(tarifas, by = join_keys)

df_br <- df_ufs %>% 
  filter(uf_sigla == 'BRASIL')

df_ufs <- df_ufs %>% 
  filter(uf_sigla != 'BRASIL')

# Índice de perdas ------------------------------------------------------------
df_ufs %>% 
  mutate(uf_sigla = fct_reorder(uf_sigla, in013)) %>% 
  ggplot() +
  geom_col(aes(x = in013, y = uf_sigla, fill = regiao), alpha = .8) +
  geom_vline(xintercept = df_br$in013, linetype = 'dotted',
             alpha = .4, col = 'gray20') +
  geom_label(x = df_br$in013, y = 5,
             label = str_replace(str_c('Brasil:\n', df_br$in013), '\\.', ','),
             col = 'gray15', family = 'serif', size = 3) +
  geom_text(aes(x = in013, y = uf_sigla,
                label = str_replace(in013, '\\.', ',')),
            nudge_x = 2.5, family = 'serif', size = 2.5, col = 'gray15') +
  theme(panel.grid = element_blank(), legend.position = c(.85, .25)) +
  scale_fill_manual(
    values = c("#BEAED4", "#FDC086", "#7FC97F", "#386CB0", "#BF5B17"),
    name = 'Região'
  ) +
  labs(y = NULL, x = 'Índice de perdas de faturamento (água)',
       title = 'Índice de perdas de faturamento no fornecimento de água',
       subtitle = 'Comparação entre Unidades Federativas')

# ggsave(filename = 'plots/barplot-perdas.png', width = 6, height = 5)

# Tarifa média água ---------------------------------------------------------
barplot_tarifa_agua <- df_ufs %>% 
  mutate(uf_sigla = fct_reorder(uf_sigla, in005)) %>% 
  ggplot() +
  geom_col(aes(x = in005, y = uf_sigla, fill = regiao), alpha = .8) +
  geom_vline(xintercept = df_br$in005, linetype = 'dotted',
             alpha = .4, col = 'gray20') +
  geom_label(x = df_br$in005, y = 5,
             label = str_replace(str_c('Brasil:\n', df_br$in005), '\\.', ','),
             col = 'gray15', family = 'serif', size = 3) +
  geom_text(aes(x = in005, y = uf_sigla,
                label = str_replace(in005, '\\.', ',')),
            nudge_x = .2, family = 'serif', size = 2.5, col = 'gray15') +
  theme(panel.grid = element_blank(), legend.position = c(.85, .25)) +
  scale_fill_manual(
    values = c("#BEAED4", "#FDC086", "#7FC97F", "#386CB0", "#BF5B17"),
    name = 'Região'
  ) +
  labs(
    y = NULL, x = bquote(bold('Tarifa média (R$/'*m^3 *')' )),
    title = 'Tarifa média do serviço de fornecimento de água',
    subtitle = 'Comparação entre Unidades Federativas'
    ) ; barplot_tarifa_agua

# ggsave(filename = 'plots/barplot-tarifa-agua.png', width = 6, height = 5,
#        plot = barplot_tarifa_agua)

# Tarifa média esgoto ---------------------------------------------------------
barplot_tarifa_esgoto <- df_ufs %>% 
  mutate(uf_sigla = fct_reorder(uf_sigla, in006)) %>% 
  ggplot() +
  geom_col(aes(x = in006, y = uf_sigla, fill = regiao), alpha = .8) +
  geom_vline(xintercept = df_br$in006, linetype = 'dotted',
             alpha = .4, col = 'gray20') +
  geom_label(x = df_br$in006, y = 5,
             label = str_replace(str_c('Brasil:\n', df_br$in006), '\\.', ','),
             col = 'gray15', family = 'serif', size = 3) +
  geom_text(aes(x = in006, y = uf_sigla,
                label = str_replace(in006, '\\.', ',')),
            nudge_x = .15, family = 'serif', size = 2.5, col = 'gray15') +
  theme(panel.grid = element_blank(), legend.position = c(.85, .25)) +
  scale_fill_manual(
    values = c("#BEAED4", "#FDC086", "#7FC97F", "#386CB0", "#BF5B17"),
    name = 'Região'
  ) +
  labs(
    y = NULL, x = bquote(bold('Tarifa média (R$/'*m^3 *')' )),
    title = 'Tarifa média do serviço de esgotamento sanitário',
    subtitle = 'Comparação entre Unidades Federativas'
    ) ; barplot_tarifa_esgoto

# ggsave(filename = 'plots/barplot-tarifa-esgoto.png', width = 6, height = 5,
#        plot = barplot_tarifa_esgoto)

# Desempenho financeiro - Prestadores regionais -------------------------------
desemp_fin <- readxl::read_excel(
  'data/desempenho-financeiro-agregado-prestadores-regionais.xlsx')

barplot_desemp_fin <- desemp_fin %>% 
  unite('sigla_prestador', c(sigla_prestador, uf_sigla), sep = ' / ') %>% 
  mutate(sigla_prestador = fct_reorder(.f = sigla_prestador, .x = in012)) %>% 
  filter(str_detect(natureza_juridica, 'Sociedade')) %>% 
  ggplot() +
  geom_col(
    aes(x = in012, y = sigla_prestador, fill = regiao)
  ) +
  geom_text(aes(x = in012, y = sigla_prestador,
                label = str_replace(in012, '\\.', ',')),
            nudge_x = 5, family = 'serif', size = 2.5, col = 'gray15') +
  theme(panel.grid = element_blank(), legend.position = c(.875, .225)) +
  scale_fill_manual(
    values = c("#BEAED4", "#FDC086", "#7FC97F", "#386CB0", "#BF5B17"),
    name = 'Região'
  ) +
  labs(
    y = NULL, x = 'Indicador de desempenho financeiro',
    title = 'Indicador de desempenho financeiro',
    subtitle =
      'Comparação entre prestadores regionais (apenas sociedades de economia mista)'
    ) ; barplot_desemp_fin

# ggsave(filename = 'plots/barplot-desempenho-financeiro-prestadores-regionais.png',
#        width = 6, height = 5,
#        plot = barplot_desemp_fin)

# Investimento per capita -----------------------------------------------------
pop_ufs <- readxl::read_excel('data/estima-pop-ufs.xlsx')
df_2018 <- readxl::read_excel('data/agregado-2018.xlsx')

df_inv <- df_2018 %>% 
  filter(str_detect(`Código do Município`, 'TOTAL'),
         !str_detect(`Estado`, '---')) %>% 
  mutate(
    inv_total =
      `FN033 - Investimentos totais realizados pelo prestador de serviços` +
      `FN048 - Investimentos totais realizados pelo(s) município(s)` +
      `FN058 - Investimentos totais realizados pelo estado`
  ) %>% 
  select(sigla_uf = Estado, regiao = `Região`, inv_total) %>% 
  mutate(sigla_uf = str_remove(sigla_uf, ' TOTAL da AMOSTRA')) %>% 
  inner_join(pop_ufs, by = 'sigla_uf') %>% 
  mutate(inv_per_capita = inv_total / pop) ; df_inv

inv_br <- df_inv %>% 
  summarise(inv_total = sum(inv_total),
            pop = sum(pop),
            inv_per_capita = inv_total / pop) ; inv_br

barplot_inv_per_capita <- df_inv %>%
  mutate(sigla_uf = fct_reorder(sigla_uf, inv_per_capita)) %>% 
  ggplot() +
  geom_col(aes(x = inv_per_capita, y = sigla_uf, fill = regiao)) +
  geom_vline(xintercept = inv_br$inv_per_capita, linetype = 'dotted',
             alpha = .4, col = 'gray20') +
  geom_label(x = inv_br$inv_per_capita, y = 5,
             label = str_replace(str_c('Brasil:\n',
                                       round(inv_br$inv_per_capita, 2)),
                                 '\\.', ','),
             col = 'gray15', family = 'serif', size = 3) +
  geom_text(aes(x = inv_per_capita, y = sigla_uf,
                label = str_replace(round(inv_per_capita, 2), '\\.', ',')),
            nudge_x = 4, family = 'serif', size = 2.5, col = 'gray15') +
  theme(panel.grid = element_blank(), legend.position = c(.85, .25)) +
  scale_fill_manual(
    values = c("#BEAED4", "#FDC086", "#7FC97F", "#386CB0", "#BF5B17"),
    name = 'Região'
  ) +
  labs(
    y = NULL, x = 'Investimento per capita (reais)',
    title = 'Investimento per capita nas redes de água e esgotamento sanitário',
    subtitle = 'Comparação entre Unidades Federativas'
    ) ; barplot_inv_per_capita

# ggsave(filename = 'plots/barplot-investimento-per-capita.png',
#        width = 6, height = 5, plot = barplot_inv_per_capita)