# -----------------------------------------------------------------------------#
# Taller de GitHub y limpieza básica de datos con dplyr
# Archivo: limpieza_base_datos.R
# -----------------------------------------------------------------------------

library(tidyverse)

ruta_entrada <- "datos/base_sucia_encuesta.txt"
ruta_salida  <- "resultados/base_limpia.csv"


# 0. Inspección inicial --------------------------------------------------------

lineas_iniciales <- readLines(
  ruta_entrada,
  n = 5,
  warn = FALSE
)

print(lineas_iniciales)


# 1. Importar la base ----------------------------------------------------------

base <- read_delim(
  file = ruta_entrada,
  delim = ";",
  col_types = cols(.default = col_character()),
  locale = locale(encoding = "UTF-8"),
  na = c("N/D", "-", ""),
  trim_ws = FALSE,
  show_col_types = FALSE
)

glimpse(base)
print(base)


# 2. Corregir los nombres ------------------------------------------------------

base <- base |> 
  mutate(
    nombre = str_squish(nombre),
    nombre = case_when(
      str_detect(nombre, "(?i)ana")   ~ "Ana María López",
      str_detect(nombre, "(?i)jos")   ~ "José Muñoz",
      str_detect(nombre, "(?i)luc")   ~ "Lucía Pérez",
      str_detect(nombre, "(?i)andr")  ~ "Andrés Niño",
      str_detect(nombre, "(?i)mar")   ~ "María José Gómez",
      str_detect(nombre, "(?i)cam")   ~ "Camilo Rojas",
      str_detect(nombre, "(?i)sof")   ~ "Sofía León",
      TRUE ~ nombre
    )
  )


# 3. Corregir las ciudades -----------------------------------------------------

base <- base |> 
  mutate(
    ciudad = str_squish(ciudad),
    ciudad = case_when(
      str_detect(ciudad, "(?i)bog") ~ "Bogotá",
      str_detect(ciudad, "(?i)med") ~ "Medellín",
      str_detect(ciudad, "(?i)cal") ~ "Cali",
      str_detect(ciudad, "(?i)bar") ~ "Barranquilla",
      str_detect(ciudad, "(?i)car") ~ "Cartagena",
      str_detect(ciudad, "(?i)per") ~ "Pereira",
      TRUE ~ ciudad
    )
  )


# 4. Corregir las fechas -------------------------------------------------------

base <- base |> 
  mutate(
    fecha_encuesta = str_squish(fecha_encuesta),
    fecha_encuesta = case_when(
      str_detect(fecha_encuesta, "03/08/2026")    ~ "2026-08-03",
      str_detect(fecha_encuesta, "2026-08-04")    ~ "2026-08-04",
      str_detect(fecha_encuesta, "5 agosto 2026") ~ "2026-08-05",
      str_detect(fecha_encuesta, "06-08-26")      ~ "2026-08-06",
      str_detect(fecha_encuesta, "2026/08/07")    ~ "2026-08-07",
      str_detect(fecha_encuesta, "08.08.2026")    ~ "2026-08-08",
      str_detect(fecha_encuesta, "08/13/2026")    ~ "2026-08-13",
      TRUE ~ fecha_encuesta
    ),
    fecha_encuesta = as.Date(fecha_encuesta, format = "%Y-%m-%d")
  )


# 5. Corregir el ingreso mensual ----------------------------------------------

base <- base |> 
  mutate(
    ingreso_mensual = str_squish(ingreso_mensual),
    ingreso_mensual = case_when(
      str_detect(ingreso_mensual, "1.250.000,50") ~ "1250000.50",
      str_detect(ingreso_mensual, "950000.75")    ~ "950000.75",
      str_detect(ingreso_mensual, "1,100,000.00") ~ "1100000.00",
      str_detect(ingreso_mensual, "875.500,00")   ~ "875500.00",
      str_detect(ingreso_mensual, "1 050 000,25") ~ "1050000.25",
      str_detect(ingreso_mensual, "725000")       ~ "725000.00",
      TRUE ~ NA_character_
    ),
    ingreso_mensual = as.numeric(ingreso_mensual)
  )


# 6. Corregir la nota promedio -------------------------------------------------

base <- base |> 
  mutate(
    nota_promedio = str_squish(nota_promedio),
    nota_promedio = case_when(
      str_detect(nota_promedio, "4,2") ~ "4.2",
      str_detect(nota_promedio, "3.8") ~ "3.8",
      str_detect(nota_promedio, "4,0") ~ "4.0",
      str_detect(nota_promedio, "3,5") ~ "3.5",
      str_detect(nota_promedio, "4.5") ~ "4.5",
      str_detect(nota_promedio, "4,1") ~ "4.1",
      TRUE ~ NA_character_
    ),
    nota_promedio = as.numeric(nota_promedio)
  )


# 7. Corregir la variable trabaja ---------------------------------------------

base <- base |> 
  mutate(
    trabaja = str_squish(trabaja),
    trabaja = case_when(
      str_detect(trabaja, "(?i)^s") ~ "Sí",
      str_detect(trabaja, "(?i)^n") ~ "No",
      TRUE ~ trabaja
    )
  )


# 8. Convertir el identificador ------------------------------------------------

base <- base |> 
  mutate(
    id = as.integer(id)
  )


# 9. Revisar el resultado ------------------------------------------------------

print(base)
glimpse(base)
summary(base)


# 10. Comprobaciones automáticas ----------------------------------------------

stopifnot(nrow(base) == 7)
stopifnot(length(unique(base$id)) == 7)
stopifnot(inherits(base$fecha_encuesta, "Date"))
stopifnot(is.numeric(base$ingreso_mensual))
stopifnot(is.numeric(base$nota_promedio))
stopifnot(sum(is.na(base$ingreso_mensual)) == 1)
stopifnot(sum(is.na(base$nota_promedio)) == 1)
stopifnot(all(na.omit(base$trabaja) %in% c("Sí", "No")))


# 11. Exportar la base ---------------------------------------------------------

dir.create(dirname(ruta_salida), showWarnings = FALSE)

write_excel_csv(
  base,
  ruta_salida,
  na = ""
)

print(paste("La base limpia fue guardada en", ruta_salida))