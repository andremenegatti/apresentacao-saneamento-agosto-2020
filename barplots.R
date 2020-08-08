library(tidyverse)
library(cagedExplorer)
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

df_ufs %>% 
  mutate(uf_sigla = fct_reorder(uf_sigla, in013)) %>% 
  ggplot() +
  geom_col(aes(x = in013, y = uf_sigla, fill = regiao), alpha = .8) +
  geom_vline(xintercept = df_br$in013, linetype = 'dashed', alpha = .8, col = 'gray30') +
  geom_label(x = df_br$in013, y = 5, label = 'Brasil',
             col = 'gray30', family = 'serif', size = 3.5) +
  theme(panel.grid = element_blank(), legend.position = c(.85, .25)) +
  scale_fill_manual(
    values = c("#BEAED4", "#FDC086", "#7FC97F", "#386CB0", "#BF5B17"),
    name = 'Região'
  ) +
  labs(y = NULL, x = 'Índice de perdas de faturamento (água)',
       title = 'Índice de perdas de faturamento no fornecimento de água',
       subtitle = 'Comparação entre Unidades Federativas')

ggsave(filename = 'plots/barplot-perdas.png', width = 6, height = 5)

RColorBrewer::display.brewer.all(n = 8)
RColorBrewer::display.brewer.pal(8, 'Accent')
RColorBrewer::brewer.pal(8, 'Accent')

df_ufs %>% 
  mutate(uf_sigla = fct_reorder(uf_sigla, in005)) %>% 
  ggplot() +
  geom_col(aes(x = in005, y = uf_sigla, fill = regiao), alpha = .8) +
  geom_vline(xintercept = df_br$in005, linetype = 'dashed', alpha = .8, col = 'gray30') +
  geom_label(x = df_br$in005, y = 5, label = 'Brasil',
             col = 'gray30', family = 'serif', size = 3.5) +
  theme(panel.grid = element_blank(), legend.position = c(.85, .25)) +
  scale_fill_manual(
    values = c("#BEAED4", "#FDC086", "#7FC97F", "#386CB0", "#BF5B17"),
    name = 'Região'
  ) +
  labs(y = NULL, x = 'Tarifa média de água (R$/m3)',
       title = 'Tarifa média do serviço de fornecimento de água',
       subtitle = 'Comparação entre Unidades Federativas')
