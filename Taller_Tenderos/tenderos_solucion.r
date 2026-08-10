# =========================================================================
# TALLER COMPLETO DE MANEJO DE DATOS - TENDEROS Y PENETRACIÓN DE INTERNET
# =========================================================================

# 0. Cargar librerías necesarias
library(tidyverse)
library(haven)

# 1. Cargar la base de microdatos de tenderos
tenderos_raw <- read_dta("C:/Users/prestamour/Downloads/TenderosFU03_Publica.dta")

# Definir diccionario de nombres para las 11 actividades económicas
etiquetas_actividades <- c(
  "actG1"  = "Tienda",
  "actG2"  = "Comida preparada",
  "actG3"  = "Peluqueria y belleza",
  "actG4"  = "Ropa",
  "actG5"  = "Otras variedades",
  "actG6"  = "Papelería y comunicaciones",
  "actG7"  = "Vida nocturna",
  "actG8"  = "Productos bajo inventario",
  "actG9"  = "Salud",
  "actG10" = "Servicios",
  "actG11" = "Ferretería y afines"
)

# -------------------------------------------------------------------------
# TAREA 1: Penetración de Internet por Ciudad / Municipio
# -------------------------------------------------------------------------
tarea1_muni <- tenderos_raw %>%
  group_by(Munic_Dept) %>%
  summarise(
    internet_promedio = mean(uso_internet == 1, na.rm = TRUE),
    total_tiendas = n(),
    .groups = "drop"
  ) %>%
  mutate(internet_pct = scales::percent(internet_promedio, accuracy = 0.1))

print("=== TAREA 1: PENETRACIÓN POR MUNICIPIO ===")
print(tarea1_muni)


# -------------------------------------------------------------------------
# TAREA 2: Penetración de Internet por Sector Comercial
# -------------------------------------------------------------------------
tarea2_sector <- tenderos_raw %>%
  pivot_longer(
    cols = starts_with("actG"),
    names_to = "actG",
    values_to = "realiza_actividad"
  ) %>%
  filter(realiza_actividad == 1) %>%
  group_by(actG) %>%
  summarise(
    internet_promedio = mean(uso_internet == 1, na.rm = TRUE),
    total_tiendas = n(),
    .groups = "drop"
  ) %>%
  mutate(
    Actividad = recode(actG, !!!etiquetas_actividades),
    internet_pct = scales::percent(internet_promedio, accuracy = 0.1)
  ) %>%
  arrange(desc(internet_promedio))

print("=== TAREA 2: PENETRACIÓN POR SECTOR COMERCIAL ===")
print(tarea2_sector %>% select(actG, Actividad, internet_pct, total_tiendas))


# -------------------------------------------------------------------------
# TAREA 3: Penetración por Municipio x Sector Comercial
# -------------------------------------------------------------------------
tarea3_muni_act <- tenderos_raw %>%
  pivot_longer(
    cols = starts_with("actG"),
    names_to = "actG",
    values_to = "realiza_actividad"
  ) %>%
  filter(realiza_actividad == 1) %>%
  group_by(Munic_Dept, actG) %>%
  summarise(
    internet_promedio = mean(uso_internet == 1, na.rm = TRUE),
    total_tiendas = n(),
    .groups = "drop"
  ) %>%
  mutate(
    Actividad = recode(actG, !!!etiquetas_actividades),
    internet_pct = scales::percent(internet_promedio, accuracy = 0.1)
  )

print("=== TAREA 3: PENETRACIÓN POR MUNICIPIO X ACTIVIDAD ===")
print(tarea3_muni_act)


# -------------------------------------------------------------------------
# TAREA 4: Unión con Población DANE y Nombre de Municipio
# -------------------------------------------------------------------------

# 1. Asignar nombres posicionales a TerriData
terridata_base <- TerriData_Dim2
colnames(terridata_base)[1:10] <- c(
  "Cod_Depto", "Departamento", "Cod_Muni", "Municipio", 
  "Dimension", "Indicador", "Variable", "Valor", 
  "V9", "Anio"
)

# 2. Extraer datos de población y nombre de municipio
poblacion_limpia <- terridata_base %>%
  mutate(
    DIVIPOLA = str_pad(as.character(Cod_Muni), width = 5, side = "left", pad = "0"),
    Poblacion_num = as.numeric(gsub(",", ".", gsub("\\.", "", as.character(Valor))))
  ) %>%
  filter(!is.na(Poblacion_num) & Poblacion_num > 0) %>%
  group_by(DIVIPOLA) %>%
  slice_tail(n = 1) %>%
  ungroup() %>%
  select(DIVIPOLA, Municipio, Poblacion = Poblacion_num)

# 3. Unir con Tarea 1, trayendo Nombre de Ciudad y calculando la densidad
tarea4_merged <- tarea1_muni %>%
  mutate(
    DIVIPOLA_clean = str_pad(as.character(Munic_Dept), width = 5, side = "left", pad = "0")
  ) %>%
  left_join(poblacion_limpia, by = c("DIVIPOLA_clean" = "DIVIPOLA")) %>%
  mutate(
    tiendas_por_10k_hab = round((total_tiendas / Poblacion) * 10000, 2)
  ) %>%
  select(Munic_Dept, Municipio, total_tiendas, internet_pct, Poblacion, tiendas_por_10k_hab)

print("=== TAREA 4: BASE UNIDA CON NOMBRES DE CIUDAD Y DENSIDAD ===")
print(tarea4_merged)
