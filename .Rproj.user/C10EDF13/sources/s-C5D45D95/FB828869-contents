library(janitor)
library(futurevisions)
library(highcharter)
library(EpiEstim)
library(tidyverse)
library(magrittr)
library(furrr)
library(future)

plan(multiprocess) # activar procesamiento paralelo

source("code/limpieza_bases.R")
source("code/funciones.R")

#----------------------------------------------
# aplanamiento de curvas
#----------------------------------------------
# vector de países a remover
quitar <- c("Antigua and Barbuda", "Bahamas", "Barbados", "Belize", "Guyana", "Suriname", 
            "Trinidad and Tobago", "Dominica", "Grenada", "Saint Kitts and Nevis", "Saint Lucia",
            "Saint Vincent and the Grenadines", "Jamaica")


codigos <- read_csv("input/codigos_covid_paises.csv") %>% 
  remove_empty() %>% 
  filter(
    region_es == "América Latina y el Caribe",
    !country_region %in% quitar
  ) %>% 
  dplyr::select(
    pais = pais_region,
    pais_region = country_region,
    pais_nombre_corto,
    continente
  ) 

# preparación de base: confirmados
df_mundo %>%
  filter(
    base == "confirmados",
    pais_region %in% codigos$pais_region
  ) %>% 
  left_join(., codigos) %>% 
  group_by(pais_nombre_corto, semana) %>% 
  mutate(n = n()) %>% 
  filter(n >= 6) %>% 
  ungroup() %>% 
  group_split(pais_nombre_corto) %>% 
  map(., ~mutate(
    ., casos = sum(incidencia),
    ult_semana = pull(., incidencia) %>% last)) %>% 
  bind_rows() %>% 
  group_by(pais_nombre_corto, semana) %>% 
  mutate(
    ult_semana = sum(incidencia)
  ) %>% 
  ungroup() %>% 
  group_split(pais_nombre_corto) %>% 
  map(., ~mutate(., 
                 ult_semana = pull(., ult_semana) %>% last
  )) %>% 
  bind_rows() %>% 
  ungroup() %>% 
  mutate(
    titulo = paste0(pais_nombre_corto, ", ", casos, " casos em", "\n", total_semanas,  
                    " semanas, desde o paciente zero", "\n", "Casos na última semana:", ult_semana)
  ) -> temp


temp %>% 
  dplyr::select(pais_region, casos) %>% 
  unique() %>% 
  arrange(casos) %>% 
  mutate(num = 1:nrow(.)) -> temp_1

# grafico incidencia confirmados
temp %>% 
  group_by(titulo, pais_region, semana) %>% 
  summarise(incidencia = sum(incidencia)) %>% 
  left_join(., temp_1) %>% 
  ggplot(aes(semana, incidencia, fill = continente)) + 
  geom_col(color = NA, alpha = 0.9, fill = "#CB1724") + 
  facet_wrap(vars(fct_reorder(titulo, num, .desc = T)), scales = "free", ncol = 4) +
  hrbrthemes::theme_ipsum_rc(grid = F, base_family = "Open Sans", strip_text_size = 18) +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.background = element_rect(fill = "#F8F8F8", colour = NA),
    strip.background = element_rect(fill = "#F8F8F8", colour = NA),
  )  -> curva_confirmados 


# preparación de base: fallecidos
df_mundo %>%
  filter(
    base == "fallecidos",
    pais_region %in% codigos$pais_region
  ) %>% 
  left_join(., codigos) %>% 
  group_by(pais_region, semana) %>% 
  mutate(n = n()) %>% 
  filter(n >= 6) %>% 
  ungroup() %>% 
  group_split(pais_region) %>% 
  map(., ~mutate(
    ., casos = sum(incidencia),
    ult_semana = pull(., incidencia) %>% last)) %>% 
  bind_rows() %>% 
  group_by(pais_region, semana) %>% 
  mutate(
    ult_semana = sum(incidencia)
  ) %>% 
  ungroup() %>% 
  group_split(pais_region) %>% 
  map(., ~mutate(., 
                 ult_semana = pull(., ult_semana) %>% last
  )) %>% 
  bind_rows() %>% 
  ungroup() %>% 
  mutate(
    titulo = paste0(pais_nombre_corto, ", ", casos, " óbitos", "\n", total_semanas,  
                    " semanas desde o primeiro óbito", "\n", "Óbitos, última semana:", ult_semana)
  ) -> temp

# curva fallecidos
temp %>% 
  group_by(titulo, pais_region, semana) %>% 
  summarise(incidencia = sum(incidencia)) %>% 
  left_join(., temp_1) %>% 
  ggplot(aes(semana, incidencia, fill = continente)) + 
  geom_col(color = NA, alpha = 0.9, fill = "#09283C") + 
  facet_wrap(vars(fct_reorder(titulo, num, .desc = T)), scales = "free", ncol = 4) +
  hrbrthemes::theme_ipsum_rc(grid = F, base_family = "Open Sans", strip_text_size = 18) +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.background = element_rect(fill = "#F8F8F8", colour = NA),
    strip.background = element_rect(fill = "#F8F8F8", colour = NA),
  )  -> curva_fallecidos 

