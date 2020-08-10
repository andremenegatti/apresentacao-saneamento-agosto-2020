library(tidyverse)
library(cagedExplorer)
theme_set(custom_theme())

ipca <- readxl::read_excel('data/serie-ipca-numero-indice-dezembro.xlsx')

df_series <- tibble()
for (ano in 2009:2018) {
  filename <- str_c('data/agregado-', ano, '.xlsx')
  
  df_t <- readxl::read_excel(filename) %>% 
    select(starts_with('FN'), starts_with('IN')) %>% 
    tail(1)
  
  var_dict <- names(df_t)
  names(df_t) <- str_match(names(df_t), '.{5}')[, 1]
  
  df_t$ano <- ano

  df_series <- bind_rows(df_series, df_t)
  
}

df_series <- df_series %>% 
  left_join(ipca, by = 'ano') %>%
  mutate(numero_indice_norm =  5100.61 / numero_indice) %>% 
  mutate(inv_proprios = (FN030 + FN045 + FN055) * numero_indice_norm,
         inv_onerosos = (FN031 + FN046 + FN056) * numero_indice_norm,
         inv_nao_onerosos = (FN032 + FN047 + FN057) * numero_indice_norm,
         inv_totais = (FN033 + FN048 + FN058) * numero_indice_norm,
         inv_agua = (FN023 + FN042 + FN052) * numero_indice_norm,
         inv_esgoto = (FN024 + FN043 + FN053) * numero_indice_norm)

# Investimento total ----------------------------------------------------------
df_series %>% 
  select(ano, inv_totais) %>% 
  mutate(inv_totais = inv_totais / 1e+9) %>% 
  ggplot(aes(x = ano, y = inv_totais)) +
  geom_line(size = 1, col = 'steelblue', alpha = .8) +
  geom_label(aes(label = inv_totais %>% round(2) %>% str_replace('\\.', ',')),
             size = 2, family = 'serif', col = 'gray30') +
  theme(panel.grid = element_blank(), legend.position = 'bottom',
        legend.title = element_blank()) +
  scale_color_brewer(palette = 'Dark2') +
  labs(
    x = 'Ano', y = 'Montante (bilhões de reais)',
    title = 'Evolução do investimento em serviços de água e esgotamento',
    subtitle = 'Total anual, de 2009 a 2018',
    caption = 'Fonte: Elaboração própria a partir de dados do SNIS.\n\nNotas: 
    (i) valores em reais de 2018, corrigidos pelo IPCA;
    (ii) total de investimentos realizados pelo estado, município e/ou prestador.'
  )

ggsave(filename = 'plots/lineplot-investimentos-totais.png',
       width = 5.5, height = 5)

# Investimentos por tipo de serviço -------------------------------------------
df_series %>% 
  select(ano, starts_with('inv_')) %>% 
  pivot_longer(cols = -ano, names_to = 'inv', values_to = 'valor', ) %>% 
  mutate(valor = valor / 1e+9) %>% 
  filter(inv %in% c('inv_agua', 'inv_esgoto')) %>% 
  mutate(inv = case_when(
    inv == 'inv_agua' ~ 'Água',
    TRUE ~ 'Esgoto'
  )) %>% 
  ggplot(aes(x = ano, y = valor)) +
  geom_line(aes( col = inv), size = 1, alpha = .8) +
  geom_label(aes(label = valor %>% round(2) %>% str_replace('\\.', ',')),
             size = 2, family = 'serif', col = 'gray30') +
  theme(panel.grid = element_blank(), legend.position = c(.1, .15),
        legend.title = element_blank()) +
  scale_color_manual(values = c('#3690c0', '#74c476')) +
  labs(
    x = 'Ano', y = 'Montante (bilhões de reais)',
    title = 'Investimentos em serviços de água e esgotamento',
    subtitle = 'Montante anual por tipo de serviço, de 2009 a 2018',
    caption = 'Fonte: Elaboração própria a partir de dados do SNIS.\n\nNotas: 
    (i) valores em reais de 2018, corrigidos pelo IPCA;
    (ii) total de investimentos realizados pelo estado, município e/ou prestador.'
  )

ggsave(filename = 'plots/lineplot-investimentos-tipo-servico.png',
       width = 5.5, height = 5)

# Investimentos por modalidade ------------------------------------------------
df_series %>% 
  select(ano, starts_with('inv_')) %>% 
  pivot_longer(cols = -ano, names_to = 'inv', values_to = 'valor', ) %>% 
  mutate(valor = valor / 1e+9) %>% 
  filter(inv %in% c('inv_proprios', 'inv_onerosos', 'inv_nao_onerosos')) %>% 
  mutate(inv = case_when(
    inv == 'inv_proprios' ~ 'Recursos próprios',
    inv == 'inv_onerosos' ~ 'Onerosos',
    TRUE ~ 'Não onerosos'
  )) %>% 
  mutate(inv = fct_relevel(inv, 'Recursos próprios',
                           'Onerosos', 'Não onerosos')) %>% 
  ggplot(aes(x = ano, y = valor)) +
  geom_line(aes( col = inv), size = 1, alpha = .8) +
  geom_label(aes(label = valor %>% round(2) %>% str_replace('\\.', ',')),
             size = 2, family = 'serif', col = 'gray30') +
  theme(panel.grid = element_blank(), legend.position = 'bottom',
        legend.title = element_blank()) +
  scale_color_brewer(palette = 'Dark2') +
  labs(
    x = 'Ano', y = 'Montante (bilhões de reais)',
    title = 'Investimentos em serviços de água e esgotamento',
    subtitle = 'Montante anual por modalidade, de 2009 a 2018',
    caption = 'Fonte: Elaboração própria a partir de dados do SNIS.\n\nNotas: 
    (i) valores em reais de 2018, corrigidos pelo IPCA;
    (ii) total de investimentos realizados pelo estado, município e/ou prestador.'
  )

ggsave(filename = 'plots/lineplot-investimentos-modalidade.png',
       width = 5.5, height = 5)

# Indicadores de qualidade ----------------------------------------------------
df_series %>% 
  select(ano, starts_with('IN0')) %>% 
  pivot_longer(cols = -ano, names_to = 'indicador',
               values_to = 'valor', ) %>% 
  mutate(indicador = case_when(
    indicador == 'IN016' ~ 'Tratamento de esgoto',
    indicador == 'IN055' ~ 'Atendimento de água',
    indicador == 'IN056' ~ 'Atendimento de esgoto'
  )) %>% 
  mutate(indicador = fct_relevel(indicador,
                                 'Atendimento de água',
                                 'Tratamento de esgoto',
                                 'Atendimento de esgoto')) %>% 
  ggplot(aes(x = ano, y = valor)) +
  geom_line(aes(col = indicador), size = 1, alpha = .8) +
  geom_label(aes(label = valor %>% round(2) %>% str_replace('\\.', ',')),
             size = 2, family = 'serif', col = 'gray30') +
  theme(panel.grid = element_blank(),
        legend.position = 'bottom',
        legend.title = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_color_brewer(palette = 'Dark2') +
  labs(
    x = 'Ano', y = NULL,
    title = 'Indicadores de qualidade do saneamento básico',
    subtitle = 'Valores agregados para todo o Brasil, de 2009 a 2018',
    caption = 
      'Fonte: Elaboração própria a partir de dados do SNIS (indicadores IN016, IN055 e IN056).'
  )

ggsave(filename = 'plots/lineplot-indicadores-qualidade-brasil.png',
       width = 5.5, height = 5)
