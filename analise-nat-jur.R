library(tidyverse)

files <- list.files('data')
files <- files[str_detect(files, 'perdas-e-desempenho-.*xlsx')]

df_nat_jur <- tibble()

for (i in seq_along(files)) {
  
  df_partial <- readxl::read_excel(str_c('data/', files[i]))
  
  df_agregado <- df_partial %>% tail(1) %>% 
    select(in012 = `IN012 - Indicador de desempenho financeiro`,
           in013 = `IN013 - Índice de perdas faturamento`)
  
  df_clean <- df_partial %>% 
    filter(Prestador != '---')
  
  df_agregado$n = nrow(df_clean)
  df_agregado$natureza_juridica = df_clean$`Natureza Jurídica`[1]
  
  df_nat_jur <- bind_rows(df_nat_jur, df_agregado)
  
}
