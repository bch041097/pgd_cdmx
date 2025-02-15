library(tidyverse)
library(janitor)
library(srvyr)
library(survey)
if(! require('pacman')) install.packages('pacman')
pacman::p_load(foreign, survey, openxlsx)

dir.create('microdatos', showWarnings = FALSE)

url_1 <- 'https://www.inegi.org.mx/contenidos/programas/envipe/'
url_2 <- '/microdatos/bd_envipe_'
fin <- '_csv.zip'

years <- c(2022:2024)

data_list <- list()


for (year in years) {
  url <- paste0(url_1, year, url_2, year, fin)
  destfile <- paste0('microdatos/', year, fin)
  folder <- paste0('microdatos/', year)
  
  dir.create(folder, showWarnings = FALSE)
  
  tryCatch(
    {
      options(timeout = 600)  # Aumenta el tiempo de espera a 10 minutos
      
      download.file(url, destfile, mode = 'wb')
    },
    error = function(e) {
      message('Error al descargar el archivo: ', e)
    }
  )
  
  unzip_result <- tryCatch(
    {
      unzip(destfile, exdir = folder)
      TRUE
    },
    error = function(e) {
      message('Error al descomprimir el archivo: ', e)
      FALSE
    }
  )
  
  if (unzip_result) {
    archivos <- list.files(folder, full.names = TRUE)
    print(paste('Archivos en', folder, ':', toString(archivos)))
    
    csv_file <- list.files(folder, pattern = 'TPer_Vic1.csv', full.names = TRUE)
    
    if (length(csv_file) == 1) {
      print(paste('Leyendo archivo:', csv_file))
      data <- read_csv(csv_file, show_col_types = FALSE) %>%
        janitor::clean_names() 
      
      assign(paste0('percepcion', year), data, envir = .GlobalEnv)
      data_list[[as.character(year)]] <- data
    } else
      message('No se encontró el csv en ', folder)
    
  }
}

str(percepcion2022)

per22 <- percepcion2022 %>%
  mutate(cve_ent = as.numeric(cve_ent), fac_ele = as.numeric(fac_ele),
  sexo = as.numeric(sexo), upm = as.numeric(upm), est_dis = as.numeric(est_dis),
  anio = '2022') %>% 
  select(anio, cve_ent, sexo, ap4_4_a, ap4_4_12, edad, fac_ele, upm, est_dis)

  
per23 <- percepcion2023 %>% 
  mutate(cve_ent = as.numeric(cve_ent), fac_ele = as.numeric(fac_ele),
         sexo = as.numeric(sexo), upm = as.numeric(upm), est_dis = as.numeric(est_dis),
         anio = '2023') %>% 
  select(anio, cve_ent, sexo, ap4_4_a, ap4_4_12, edad, fac_ele, upm, est_dis)

per24 <- percepcion2024 %>% 
  mutate(cve_ent = as.numeric(cve_ent), fac_ele = as.numeric(fac_ele),
         sexo = as.numeric(sexo), upm = as.numeric(upm), est_dis = as.numeric(est_dis),
         anio = '2024') %>% 
  select(anio, cve_ent, sexo, ap4_4_a, ap4_4_12, edad, fac_ele, upm, est_dis)


#ENVIPE 21

temporal <- tempfile(fileext = '.zip')

download.file('https://www.inegi.org.mx/contenidos/programas/envipe/2021/microdatos/bd_envipe_2021_csv.zip', temporal, mode = 'wb')

file_2 <- unzip(temporal, list = TRUE)

csv_2 <- unzip(temporal, files = 'bd_envipe_2021_csv/TPer_Vic1.csv', exdir = tempdir())

percepcion2021 <- read.csv(csv_2) %>% 
  janitor::clean_names()

per21 <- percepcion2021 %>%
  mutate(cve_ent = as.numeric(cve_ent), fac_ele = as.numeric(fac_ele), upm = as.numeric(upm), est_dis = as.numeric(est_dis),
         sexo = as.numeric(sexo), ap4_4_a = as.numeric(ap4_4_a), ap4_4_12 = as.numeric(ap4_4_12), anio = '2021') %>%
  select(anio, cve_ent, sexo, ap4_4_a, ap4_4_12, edad, fac_ele, upm, est_dis)

  
#ENVIPE 20 y 19

anios <- c(2019:2020)

lista_data<- list()

for (anio in anios) {
  if (anio == 2020) {
    url <- paste0('https://www.inegi.org.mx/contenidos/programas/envipe/', anio, '/microdatos/bd_envipe_', anio, '_dbf.zip')
  } else {
    url <- paste0('https://www.inegi.org.mx/contenidos/programas/envipe/', anio, '/Microdatos/bd_envipe', anio, '_dbf.zip')
  }
  
  destfile_2 <- paste0('microdatos/', anio, '_dbf.zip')
  folder_2 <- paste0('microdatos/', anio)
  
  dir.create(folder_2, showWarnings = FALSE, recursive = TRUE)
  
  tryCatch({
    options(timeout = 600)
    download.file(url, destfile_2, mode = 'wb')
  }, error = function(e) {
    message('Error al descargar el dbf')
  }
  )
  
  unzip_result <- tryCatch({
    unzip(destfile_2, exdir = folder_2)
    TRUE
  }, error = function(e) {
    message('Error al descomprimir el archivo: ', e)
    FALSE
  }
  )
  
  if (unzip_result) {
    archivos <- list.files(folder_2, pattern = "\\.dbf$", full.names = TRUE, recursive = TRUE)
    print(paste('Archivos en', folder_2, ':', toString(archivos)))
    
    dbf_file <- archivos[grepl("TPer_Vic1\\.dbf$", archivos)]
    
    if (length(dbf_file) == 0 && length(archivos) > 0) {
      dbf_file <- archivos[1]
    }
    
    if (length(dbf_file) == 1) {
      print(paste('Leyendo archivo:', dbf_file))
      data <- read.dbf(dbf_file) %>%
        janitor::clean_names()
      
      assign(paste0('percepcion', anio), data, envir = .GlobalEnv)
      lista_data[[as.character(anio)]] <- data
    } else {
      message('No se encontró un archivo dbf válido en ', folder_2)
    }
  }
}

head(percepcion2020)


per20 <- percepcion2020 %>%
  mutate(cve_ent = as.numeric(cve_ent), fac_ele = as.numeric(fac_ele), upm = as.numeric(upm),
         est_dis = as.numeric(est_dis), ap4_4_12 = as.numeric(ap4_4_12), edad = as.numeric(edad),
         anio = '2020') %>% 
  select(anio, cve_ent, ap4_4_12, edad, fac_ele, upm, est_dis)

str(per20)


per19 <- percepcion2019 %>%
  mutate(cve_ent = as.numeric(cve_ent), fac_ele = as.numeric(fac_ele), upm = as.numeric(upm),
         est_dis = as.numeric(est_dis), ap4_4_12 = as.numeric(ap4_4_12), edad = as.numeric(edad),
         anio = '2019') %>% 
  select(anio, cve_ent, ap4_4_12, edad, fac_ele, upm, est_dis)


str(per19)


#Población 18+ que se sienten seguras en parques y centros recreativos 

cdmx_parques <- bind_rows(per24, per23, per22, per21, per20, per19) %>% 
  filter(cve_ent == 09) 

segu_parques <- cdmx_mujeres %>% 
  filter(edad >= 18) %>% 
  group_by(anio) %>% 
  summarise(
    total_pob = sum (fac_ele, na.rm = TRUE),
    total_segu = sum(fac_ele * (ap4_4_12 == 2), na.rm = TRUE),
    porcen= (total_segu / total_pob * 100) )

print(segu_parques)


#Intento 1. Con diseño muestral
segu_parques <- cdmx_parques %>%
  filter(edad >= 18) %>%
  group_by(anio) %>%
  summarise(
    total_seg = survey_total(ap4_4_12 * fac_ele, na.rm = TRUE), 
    total_pob = survey_total(fac_ele, na.rm = TRUE)
  ) %>%
  mutate(propor_seg = total_seg / total_pob) %>%  
  select(anio, propor_seg) %>%
  as.data.frame()

print(segu_parques)

# write_xlsx(segu_parques, 'personas_seguras_parques.xlsx')



#Porcentaje de mujeres que se sienten seguras al caminar solas por la noche

cdmx_mujeres <- bind_rows(per24, per23, per22, per21) %>% 
  filter(cve_ent == 9)

tail(cdmx_mujeres)

inseg_mujeres <- cdmx_mujeres %>%
  filter(edad >= 18, sexo == 2) %>% 
  group_by(anio) %>%
  summarise(
    total_pob = sum(fac_ele, na.rm = TRUE),
    total_segu = sum(fac_ele * (ap4_4_a %in% c(1,  2)), na.rm = TRUE), 
    porcent = (total_segu / total_pob) * 100
  )

print(inseg_mujeres)

# write_xlsx(inseg_mujeres, 'mujeres_seguras.xlsx')


