library(tidyverse)
library(cagedExplorer)
theme_set(custom_theme())

df_prestadores <- readxl::read_excel('data/perdas-e-desempenho-prestadores-2018.xlsx') %>% 
  select(contains('Natureza'), contains('IN')) %>% 
  filter(!`Natureza Jurídica` %in% c('---', 'Organização Social')) 

names(df_prestadores) <- c('nat_jur', 'in012', 'in013')

df_prestadores %>% 
  mutate(nat_jur = case_when(
    str_detect(nat_jur, 'Sociedade') ~ 'Soc. Econ. Mista',
    str_detect(nat_jur, 'Administração pública') ~ 'Adm. pública',
    str_detect(nat_jur, 'Empresa privada') ~ 'Emp. privada',
    str_detect(nat_jur, 'Empresa pública') ~ 'Emp. pública',
    TRUE ~ 'Autarquia'
  )) %>%
  ggplot(aes(y = nat_jur, x = in012)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(alpha = .15, shape = 1, col = 'steelblue') +
  theme(panel.grid = element_blank()) +
  labs(
    x = 'Indicador de desempenho financeiro',
    y = NULL,
    title = 'Indicador de Desempenho Financeiro - 2018',
    subtitle = 'Comparando distribuições, por tipo de prestador',
    caption = 'Fonte: Elaboração própria a partir de dados do SNIS (variável IN013)'
  )

ggsave(filename = 'plots/boxplot-desempenho-financeiro-prestadores-2018.png',
       width = 5, height = 4.5)

df_prestadores %>% 
  mutate(nat_jur = case_when(
    str_detect(nat_jur, 'Sociedade') ~ 'Soc. Econ. Mista',
    str_detect(nat_jur, 'Administração pública') ~ 'Adm. pública',
    str_detect(nat_jur, 'Empresa privada') ~ 'Emp. privada',
    str_detect(nat_jur, 'Empresa pública') ~ 'Emp. pública',
    TRUE ~ 'Autarquia'
  )) %>%
  ggplot(aes(y = nat_jur, x = in013)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(alpha = .2, shape = 1, col = 'steelblue') +
  theme(panel.grid = element_blank()) +
  labs(
    x = 'Índice de perdas',
    y = NULL,
    title = 'Índice de Perdas - 2018',
    subtitle = 'Comparando distribuições, por tipo de prestador',
    caption = 'Fonte: Elaboração própria a partir de dados do SNIS (variável IN013)'
  )


ggsave(filename = 'plots/boxplot-perdas-prestadores-2018.png',
       width = 5, height = 4.5)

