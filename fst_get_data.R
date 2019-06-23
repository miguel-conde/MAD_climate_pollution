library(tidyverse)
library(lubridate)


# POLLUTION DATA ----------------------------------------------------------



csv_c_2019 <- "https://datos.madrid.es/egob/catalogo/201410-10306606-calidad-aire-diario.csv"
csv_c_2018 <- "https://datos.madrid.es/egob/catalogo/201410-7775096-calidad-aire-diario.csv"
csv_c_2017 <- "https://datos.madrid.es/egob/catalogo/201410-7775098-calidad-aire-diario.csv"
csv_c_2016 <- "https://datos.madrid.es/egob/catalogo/201410-10306574-calidad-aire-diario.csv"



prepare_c_data <- function(raw_c_data) {
  
  raw_c_data <- raw_c_data %>%
    janitor::clean_names() %>%
    as.tibble
  
  aux1 <- raw_c_data  %>%
    select(-provincia, -municipio, -estacion, -punto_muestreo, -starts_with("v")) %>%
    mutate_at(vars(starts_with("d")), .funs = list(as.numeric)) %>%
    gather(dia, valor_mag, starts_with("d"))
  
  aux2 <- raw_c_data %>%
    select(-provincia, -municipio, -estacion, -punto_muestreo, -starts_with("d")) %>%
    gather(valid, valor_valid, starts_with("v"))
  
  rm(raw_c_data)
  gc()
  
  all_c_data <- inner_join(aux1, aux2) %>%
    filter(valor_valid == "V") %>%
    select(-valid, -valor_valid) %>%
    mutate(dia = dia %>% str_replace("d", "") %>% as.integer) %>%
    group_by(ano, mes, dia, magnitud) %>%
    summarise(mn_valor_mag = mean(valor_mag, na.rm = TRUE))
  
  rm(aux1)
  rm(aux2)
  gc()
  
  all_c_data <- all_c_data %>%
    unite("fecha", ano, mes, dia, sep = "-") %>%
    mutate(fecha = ymd(fecha) )%>%
    filter(!is.na(fecha))
  
  all_c_data <- all_c_data %>%
    spread(magnitud, mn_valor_mag) %>%
    rename_at(vars(-fecha), .funs = list(function(x) paste0("m_", x)))
  
  return(all_c_data)
}

all_c_data <- prepare_c_data(read.csv2(csv_c_2016)) %>% 
  bind_rows(prepare_c_data(read.csv2(csv_c_2017))) %>% 
  bind_rows(prepare_c_data(read.csv2(csv_c_2018))) %>% 
  bind_rows(prepare_c_data(read.csv2(csv_c_2019))) %>% 
  arrange(fecha)

names(all_c_data) <- c("fecha",
                       "SO2", "CO", "NO", "NO2", "PM2_5", "PM10", "NOx", "O3",
                       "TOL", "BEN", "EBE", "TCH", "CH4", "NMHC")

boxplot(all_c_data %>% select(-fecha))

all_c_data %>% filter(NO == max(NO))

plot(all_c_data %>% select(fecha, NO), type = "l")



# CLIMATE DATA ------------------------------------------------------------

library(jsonlite)
library(httr)

# https://opendata.aemet.es/opendata/api/valores/climatologicos/diarios/datos/fechaini/2016-01-01T00:00:00UTC/fechafin/2019-06-21T23:59:59UTC/estacion/3195/?api_key=eyJhbGciOiJIUzI1NiJ9.eyJzdWIiOiJtaWd1ZWxjbzIwMDBAZ21haWwuY29tIiwianRpIjoiMzg4MTIwNjctY2FkMC00MGFjLWE0NjEtNDE2YjZiMmRmMmIxIiwiaXNzIjoiQUVNRVQiLCJpYXQiOjE1NjEwOTQ0NTMsInVzZXJJZCI6IjM4ODEyMDY3LWNhZDAtNDBhYy1hNDYxLTQxNmI2YjJkZjJiMSIsInJvbGUiOiIifQ.-hckRfUYCSGVwJr3z7GN-PBW_oBrckf4Rfg1ps_8sG8

API_key <- readRDS(file.path("data", "AEMET_API_key.rds"))

base_url <- "https://opendata.aemet.es/opendata/api/valores/climatologicos/diarios/datos"
endpoint <- 
  "/fechaini/2016-01-01T00:00:00UTC/fechafin/2019-06-21T23:59:59UTC/estacion/3195/"

callx <- paste0(base_url, endpoint, "?api_key=", API_key)
callx

json_clim_data <- GET(callx, add_headers('cache-control' = "no-cache"))
json_clim_data

txt_clim_data <- content(json_clim_data, "text")
txt_clim_data

lst_clim_data <- fromJSON(txt_clim_data, flatten = TRUE)

temp_file <- tempfile()
download.file(lst_clim_data$datos, temp_file)

raw_clim_data <- fromJSON(temp_file) %>% 
  as.tibble

unlink(temp_file)
