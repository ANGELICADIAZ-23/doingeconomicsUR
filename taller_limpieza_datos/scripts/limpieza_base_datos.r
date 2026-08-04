# -----------------------------------------------------------------------------#
# Taller de GitHub y limpieza básica de datos con dplyr
# Archivo: limpieza_base_datos.R
# -----------------------------------------------------------------------------
#
# Objetivo:
# Observar una base con errores y corregir cada problema de manera explícita
# usando mutate() y recode().
#
# No es necesario construir funciones automáticas.
# -----------------------------------------------------------------------------#

# Ejecute esta línea una sola vez si no tiene instalado tidyverse:
# install.packages("tidyverse")
library(tidyverse)

ruta_entrada <- "datos/base_sucia_encuesta.txt"
ruta_salida  <- "resultados/base_limpia.csv"


# 0. Inspección inicial --------------------------------------------------------

# Lea unas pocas líneas sin modificar el archivo.
# ¿Se ven correctamente las tildes, la ñ y los signos especiales?

lineas_iniciales <- readLines(
  ruta_entrada,
  n = 5,
  warn = FALSE
)

print(lineas_iniciales)

# TODO 1:
# Identifique:
# a) el delimitador: ';'
# b) la codificación del archivo: "UTF-8"
# c) las cadenas que representan valores perdidos: "N/D", "-", ""


# 1. Importar la base ----------------------------------------------------------


# TODO 2:
# Complete la importación.

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

unique(base$nombre)

base <- base |> 
  mutate(
    nombre = str_squish(nombre),
    nombre = recode(
      nombre,
      "JOSE MUÑOZ"        = "José Muñoz",
      " Ana María López " = "Ana María López",
      "Lucía Pérez"       = "Lucía Pérez",
      "Andrés Niño"       = "Andrés Niño",
      "María José Gómez"  = "María José Gómez",
      "Camilo Rojas"      = "Camilo Rojas",
      "Sofía León"        = "Sofía León"
    )
  )


# 3. Corregir las ciudades -----------------------------------------------------

unique(base$ciudad)

base <- base |> 
  mutate(
    ciudad = str_squish(ciudad),
    ciudad = case_when(
      ciudad %in% c("Bogotá ", " bogotá", "Bogotá", "bogotá") ~ "Bogotá",
      ciudad %in% c("medellín", "Medellín") ~ "Medellín",
      ciudad %in% c("CALI", "Cali") ~ "Cali",
      TRUE ~ ciudad
    )
  )


# 4. Corregir las fechas -------------------------------------------------------

unique(base$fecha_encuesta)

base <- base |> 
  mutate(
    fecha_encuesta = str_squish(fecha_encuesta),
    fecha_encuesta = recode(
      fecha_encuesta,
      "03/08/2026"    = "2026-08-03",
      "2026-08-04"    = "2026-08-04",
      "5 agosto 2026" = "2026-08-05",
      "06-08-26"      = "2026-08-06",
      "2026/08/07"    = "2026-08-07",
      "08.08.2026"    = "2026-08-08",
      "08/13/2026"    = "2026-08-13"
    ),
    fecha_encuesta = as.Date(fecha_encuesta, format = "%Y-%m-%d")
  )


# 5. Corregir el ingreso mensual ----------------------------------------------

unique(base$ingreso_mensual)

base <- base |> 
  mutate(
    ingreso_mensual = str_squish(ingreso_mensual),
    ingreso_mensual = recode(
      ingreso_mensual,
      "1.250.000,50" = "1250000.50",
      "950000.75"    = "950000.75",
      "1,100,000.00" = "1100000.00",
      "875.500,00"   = "875500.00",
      "1 050 000,25" = "1050000.25",
      "725000"       = "725000.00"
    ),
    ingreso_mensual = as.numeric(ingreso_mensual)
  )


# 6. Corregir la nota promedio -------------------------------------------------

unique(base$nota_promedio)

base <- base |> 
  mutate(
    nota_promedio = str_squish(nota_promedio),
    nota_promedio = recode(
      nota_promedio,
      "4,2" = "4.2",
      "3.8" = "3.8",
      "4,0" = "4.0",
      "3,5" = "3.5",
      "4.5" = "4.5",
      "4,1" = "4.1"
    ),
    nota_promedio = as.numeric(nota_promedio)
  )


# 7. Corregir la variable trabaja ---------------------------------------------

unique(base$trabaja)

base <- base |> 
  mutate(
    trabaja = str_squish(trabaja),
    trabaja = case_when(
      str_to_lower(trabaja) %in% c("sí", "si", "s...") ~ "Sí",
      str_to_lower(trabaja) %in% c("no") ~ "No",
      TRUE ~ trabaja
    )
  )


# 8. Convertir el identificador ------------------------------------------------

base <- base |> 
  mutate(id = as.integer(id))


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
