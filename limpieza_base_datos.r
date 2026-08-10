# -----------------------------------------------------------------------------#
# Taller de GitHub y limpieza básica de datos con dplyr
# Archivo: limpieza_base_datos.R
# -----------------------------------------------------------------------------

library(tidyverse)

ruta_entrada <- "../datos/base_sucia_encuesta.txt"
ruta_salida  <- "../resultados/base_limpia.csv"


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
  locale = locale(encoding = "UTF-8"),
  na = c("", "N/D", "-"),
  col_types = cols(.default = col_character()),
  trim_ws = FALSE,
  show_col_types = FALSE
)

glimpse(base)
print(base)


# 2. Corregir los nombres ------------------------------------------------------

unique(base$nombre)

base <- base %>%
  mutate(
    nombre = recode(
      nombre,
      " Ana María López " = "Ana María López",
      "JOSE MUÑOZ" = "Jose Muñoz",
      "Lucía Pérez" = "Lucía Pérez",
      "Andrés Niño" = "Andrés Niño",
      "María José Gómez" = "María José Gómez",
      "Camilo Rojas" = "Camilo Rojas",
      "Sofía León" = "Sofía León"
    )
  )


# 3. Corregir las ciudades -----------------------------------------------------

unique(base$ciudad)

base <- base %>%
  mutate(
    ciudad = recode(
      ciudad,
      "Bogotá " = "Bogotá",
      "medellín" = "Medellín",
      "CALI" = "Cali",
      "Barranquilla" = "Barranquilla",
      " bogotá" = "Bogotá",
      "Cartagena" = "Cartagena",
      "Pereira" = "Pereira"
    )
  )


# 4. Corregir las fechas -------------------------------------------------------

unique(base$fecha_encuesta)

base <- base %>%
  mutate(
    fecha_encuesta = recode(
      fecha_encuesta,
      "03/08/2026" = "2026-08-03",
      "2026-08-04" = "2026-08-04",
      "5 agosto 2026" = "2026-08-05",
      "06-08-26" = "2026-08-06",
      "2026/08/07" = "2026-08-07",
      "08.08.2026" = "2026-08-08",
      "08/13/2026" = "2026-08-13"
    )
  )

base <- base %>%
  mutate(
    fecha_encuesta = as.Date(
      fecha_encuesta,
      format = "%Y-%m-%d"
    )
  )


# 5. Corregir el ingreso mensual ----------------------------------------------

unique(base$ingreso_mensual)

base <- base %>%
  mutate(
    ingreso_mensual = recode(
      ingreso_mensual,
      "1.250.000,50" = "1250000.50",
      "950000.75" = "950000.75",
      "1,100,000.00" = "1100000.00",
      "N/D" = NA_character_,
      "875.500,00" = "875500.00",
      "1 050 000,25" = "1050000.25",
      "725000" = "725000"
    )
  )

base <- base %>%
  mutate(
    ingreso_mensual = as.numeric(ingreso_mensual)
  )


# 6. Corregir la nota promedio -------------------------------------------------

unique(base$nota_promedio)

base <- base %>%
  mutate(
    nota_promedio = recode(
      nota_promedio,
      "4,2" = "4.2",
      "3.8" = "3.8",
      "4,0" = "4.0",
      "3,5" = "3.5",
      "4.5" = "4.5",
      "-" = NA_character_,
      "4,1" = "4.1"
    )
  )

base <- base %>%
  mutate(
    nota_promedio = as.numeric(nota_promedio)
  )


# 7. Corregir la variable trabaja ---------------------------------------------

unique(base$trabaja)

base <- base %>%
  mutate(
    trabaja = recode(
      trabaja,
      "Sí" = "Sí",
      "si " = "Sí",
      "No" = "No",
      "NO" = "No",
      "sí" = "Sí",
      "no" = "No"
    )
  )


# 8. Convertir el identificador ------------------------------------------------

base <- base %>%
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

dir.create("../resultados", showWarnings = FALSE)

write_csv(
  base,
  ruta_salida,
  na = ""
)

print("La base limpia fue guardada en resultados/base_limpia.csv")
