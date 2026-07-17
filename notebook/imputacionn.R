# ============================================================
# PASO 1. AUDITORÍA DE DATOS ORIGINALES
# ============================================================

library(readxl)
library(tidyverse)

ruta_datos <- file.path(
  "data",
  "raw",
  "emisiones_co2_sector_energetico_rd_2000_2022.xlsx"
)

# Leer los guiones directamente como NA
df_raw <- read_excel(
  ruta_datos,
  na = "-"
)

# Eliminar posibles espacios en los nombres de columnas
names(df_raw) <- stringr::str_squish(names(df_raw))

# Convertir todas las variables a formato numérico
df_num <- df_raw |>
  mutate(
    across(
      everything(),
      as.numeric
    )
  )

# Variables que no entrarán activamente en la imputación
var_suplementarias <- c("Año", "Total")

# Variables correspondientes a los combustibles
var_activas <- setdiff(
  names(df_num),
  var_suplementarias
)

# ------------------------------------------------------------
# 1. Identificar las posiciones originalmente faltantes
# ------------------------------------------------------------

tabla_faltantes_originales <- df_num |>
  select(Año, all_of(var_activas)) |>
  pivot_longer(
    cols = -Año,
    names_to = "Variable",
    values_to = "Valor_original"
  ) |>
  filter(is.na(Valor_original)) |>
  arrange(Variable, Año)

tabla_faltantes_originales

cat(
  "Número total de valores faltantes:",
  nrow(tabla_faltantes_originales),
  "\n"
)

# ------------------------------------------------------------
# 2. Identificar valores negativos presentes en la fuente
# ------------------------------------------------------------

tabla_negativos_originales <- df_num |>
  select(Año, all_of(var_activas)) |>
  pivot_longer(
    cols = -Año,
    names_to = "Variable",
    values_to = "Valor_original"
  ) |>
  filter(
    !is.na(Valor_original),
    Valor_original < 0
  ) |>
  arrange(Variable, Año)

tabla_negativos_originales

cat(
  "Número de valores negativos originales:",
  nrow(tabla_negativos_originales),
  "\n"
)

# ------------------------------------------------------------
# 3. Verificar cómo se relaciona Total con los combustibles
# ------------------------------------------------------------

matriz_combustibles <- df_num |>
  select(all_of(var_activas))

comprobacion_total <- df_num |>
  transmute(
    Año,
    Total_reportado = Total,
    Suma_valores_disponibles = rowSums(
      matriz_combustibles,
      na.rm = TRUE
    ),
    Diferencia = Total_reportado -
      Suma_valores_disponibles
  )

comprobacion_total

cat(
  "Máxima diferencia absoluta entre Total y la suma:",
  max(abs(comprobacion_total$Diferencia)),
  "\n"
)

# ------------------------------------------------------------
# 4. Guardar las tablas de auditoría
# ------------------------------------------------------------

dir.create(
  file.path("results", "tables"),
  recursive = TRUE,
  showWarnings = FALSE
)

write_csv(
  tabla_faltantes_originales,
  file.path(
    "results",
    "tables",
    "auditoria_faltantes_originales.csv"
  )
)

write_csv(
  tabla_negativos_originales,
  file.path(
    "results",
    "tables",
    "auditoria_negativos_originales.csv"
  )
)

write_csv(
  comprobacion_total,
  file.path(
    "results",
    "tables",
    "auditoria_comprobacion_total.csv"
  )
)




# ============================================================
# PASO 2. IMPUTACIÓN PCA EXCLUYENDO KEROSENE
# Escenario de sensibilidad, no modificación de los datos raw
# ============================================================

library(missMDA)
library(tidyverse)

# Variables suplementarias
var_suplementarias <- c("Año", "Total")

# Kerosene se excluye solamente de este escenario de imputación
var_excluida_sensibilidad <- "Kerosene"

# Variables activas para esta prueba
var_activas_sin_kerosene <- setdiff(
  names(df_num),
  c(var_suplementarias, var_excluida_sensibilidad)
)

# Matriz utilizada en la nueva imputación
X_sin_kerosene <- df_num |>
  select(all_of(var_activas_sin_kerosene)) |>
  as.data.frame()

# Verificar dimensiones y faltantes
cat(
  "Dimensiones de la matriz:",
  nrow(X_sin_kerosene),
  "x",
  ncol(X_sin_kerosene),
  "\n"
)

cat(
  "Valores faltantes antes de imputar:",
  sum(is.na(X_sin_kerosene)),
  "\n"
)

# Registrar las posiciones originalmente faltantes
posiciones_na <- which(
  is.na(X_sin_kerosene),
  arr.ind = TRUE
)

# ------------------------------------------------------------
# Estimar el número de componentes
# ------------------------------------------------------------

set.seed(123)

nb_sin_kerosene <- estim_ncpPCA(
  X_sin_kerosene,
  ncp.min = 0,
  ncp.max = 5,
  method = "Regularized",
  scale = TRUE,
  method.cv = "loo",
  verbose = TRUE
)

cat(
  "Número de componentes seleccionado:",
  nb_sin_kerosene$ncp,
  "\n"
)

print(nb_sin_kerosene$criterion)

# ------------------------------------------------------------
# Realizar la imputación
# ------------------------------------------------------------

imp_sin_kerosene <- imputePCA(
  X_sin_kerosene,
  ncp = nb_sin_kerosene$ncp,
  method = "Regularized",
  scale = TRUE,
  threshold = 1e-06,
  maxiter = 1000
)

X_imp_sin_kerosene <- as.data.frame(
  imp_sin_kerosene$completeObs
)

# Reconstruir la base, conservando Año, Total y Kerosene original
data_imputada_sin_kerosene <- bind_cols(
  df_num |>
    select(Año, Total, Kerosene),
  X_imp_sin_kerosene
)



tabla_imputados_sin_kerosene <- tibble(
  Año = df_num$Año[posiciones_na[, "row"]],
  Variable = colnames(X_sin_kerosene)[posiciones_na[, "col"]],
  Valor_imputado = X_imp_sin_kerosene[
    cbind(
      posiciones_na[, "row"],
      posiciones_na[, "col"]
    )
  ]
) |>
  mutate(
    Negativo = Valor_imputado < 0
  ) |>
  arrange(Variable, Año)

tabla_imputados_sin_kerosene


resumen_imputacion_sin_kerosene <- tabla_imputados_sin_kerosene |>
  summarise(
    Numero_imputados = n(),
    Numero_negativos = sum(Negativo),
    Valor_minimo = min(Valor_imputado),
    Valor_maximo = max(Valor_imputado)
  )

resumen_imputacion_sin_kerosene





imputados_gas_natural <- tabla_imputados_sin_kerosene |>
  filter(Variable == "Gas natural")

imputados_gas_natural



ruta_tablas <- file.path(
  "results",
  "tables"
)

dir.create(
  ruta_tablas,
  recursive = TRUE,
  showWarnings = FALSE
)

write_csv(
  tabla_imputados_sin_kerosene,
  file.path(
    ruta_tablas,
    "auditoria_imputacion_PCA_sin_kerosene.csv"
  )
)

write_csv(
  resumen_imputacion_sin_kerosene,
  file.path(
    ruta_tablas,
    "resumen_imputacion_PCA_sin_kerosene.csv"
  )
)



nb_sin_kerosene$ncp
resumen_imputacion_sin_kerosene
imputados_gas_natural



install.packages("missForest")


library(missForest)
library(tidyverse)

# ============================================================
# PASO 3. IMPUTACIÓN MEDIANTE MISSFOREST
# Misma matriz utilizada en el escenario PCA sin Kerosene
# ============================================================

set.seed(123)

imp_missforest <- missForest(
  xmis = X_sin_kerosene,
  maxiter = 20,
  ntree = 500,
  mtry = max(
    1,
    floor(sqrt(ncol(X_sin_kerosene)))
  ),
  variablewise = TRUE,
  verbose = TRUE,
  parallelize = "no"
)

# Base completa imputada
X_imp_missforest <- as.data.frame(
  imp_missforest$ximp
)

imp_missforest$OOBerror




# Posiciones originalmente faltantes
posiciones_na_mf <- which(
  is.na(X_sin_kerosene),
  arr.ind = TRUE
)

tabla_imputados_missforest <- tibble(
  Año = df_num$Año[
    posiciones_na_mf[, "row"]
  ],
  Variable = colnames(X_sin_kerosene)[
    posiciones_na_mf[, "col"]
  ],
  Valor_imputado = X_imp_missforest[
    cbind(
      posiciones_na_mf[, "row"],
      posiciones_na_mf[, "col"]
    )
  ]
) |>
  mutate(
    Negativo = Valor_imputado < 0
  ) |>
  arrange(Variable, Año)

tabla_imputados_missforest




resumen_imputacion_missforest <- tabla_imputados_missforest |>
  summarise(
    Numero_imputados = n(),
    Numero_negativos = sum(Negativo),
    Valor_minimo = min(Valor_imputado),
    Valor_maximo = max(Valor_imputado)
  )

resumen_imputacion_missforest


imputados_gas_natural_missforest <-
  tabla_imputados_missforest |>
  filter(Variable == "Gas natural")

imputados_gas_natural_missforest



comparacion_imputaciones <- tabla_imputados_sin_kerosene |>
  select(
    Año,
    Variable,
    Valor_PCA = Valor_imputado
  ) |>
  left_join(
    tabla_imputados_missforest |>
      select(
        Año,
        Variable,
        Valor_missForest = Valor_imputado
      ),
    by = c("Año", "Variable")
  ) |>
  mutate(
    Diferencia = Valor_missForest - Valor_PCA,
    Diferencia_relativa_pct = case_when(
      Valor_PCA != 0 ~
        100 * Diferencia / abs(Valor_PCA),
      TRUE ~ NA_real_
    )
  ) |>
  arrange(Variable, Año)

comparacion_imputaciones


comparacion_gas_natural <- comparacion_imputaciones |>
  filter(Variable == "Gas natural")

comparacion_gas_natural



imp_missforest$OOBerror
resumen_imputacion_missforest
comparacion_gas_natural



# ============================================================
# PASO 4. MISSFOREST USANDO AÑO COMO PREDICTOR AUXILIAR
# ============================================================

library(missForest)
library(tidyverse)

# La matriz anterior excluía Kerosene
# X_sin_kerosene contiene únicamente los combustibles activos

X_auxiliar_anio <- bind_cols(
  Año = df_num$Año,
  X_sin_kerosene
) |>
  as.data.frame()

str(X_auxiliar_anio)

cat(
  "Número de valores faltantes:",
  sum(is.na(X_auxiliar_anio)),
  "\n"
)



set.seed(123)

imp_missforest_anio <- missForest(
  xmis = X_auxiliar_anio,
  maxiter = 20,
  ntree = 500,
  mtry = max(
    1,
    floor(sqrt(ncol(X_auxiliar_anio)))
  ),
  variablewise = TRUE,
  verbose = TRUE,
  parallelize = "no"
)


datos_auxiliares_imputados <- as.data.frame(
  imp_missforest_anio$ximp
)

X_imp_missforest_anio <- datos_auxiliares_imputados |>
  select(-Año)





posiciones_na_anio <- which(
  is.na(X_sin_kerosene),
  arr.ind = TRUE
)

tabla_imputados_missforest_anio <- tibble(
  Año = df_num$Año[
    posiciones_na_anio[, "row"]
  ],
  Variable = colnames(X_sin_kerosene)[
    posiciones_na_anio[, "col"]
  ],
  Valor_imputado = X_imp_missforest_anio[
    cbind(
      posiciones_na_anio[, "row"],
      posiciones_na_anio[, "col"]
    )
  ]
) |>
  mutate(
    Negativo = Valor_imputado < 0
  ) |>
  arrange(Variable, Año)

tabla_imputados_missforest_anio


resumen_imputacion_missforest_anio <-
  tabla_imputados_missforest_anio |>
  summarise(
    Numero_imputados = n(),
    Numero_negativos = sum(Negativo),
    Valor_minimo = min(Valor_imputado),
    Valor_maximo = max(Valor_imputado)
  )

resumen_imputacion_missforest_anio





# Verificar que los objetos previos existan
objetos_requeridos <- c(
  "df_num",
  "X_sin_kerosene",
  "imp_missforest_anio"
)

objetos_faltantes <- objetos_requeridos[
  !vapply(objetos_requeridos, exists, logical(1))
]

if (length(objetos_faltantes) > 0) {
  stop(
    "Faltan estos objetos: ",
    paste(objetos_faltantes, collapse = ", "),
    ". Debes ejecutar primero la imputación missForest con Año."
  )
}

# Extraer la matriz completa generada por missForest
datos_auxiliares_imputados <- as.data.frame(
  imp_missforest_anio$ximp
)

# Retirar Año, porque solo fue predictor auxiliar
X_imp_missforest_anio <- datos_auxiliares_imputados |>
  dplyr::select(-Año)

# Identificar las posiciones originalmente faltantes
posiciones_na_anio <- which(
  is.na(X_sin_kerosene),
  arr.ind = TRUE
)

# Construir la tabla de todos los valores imputados
tabla_imputados_missforest_anio <- tibble::tibble(
  Año = df_num$Año[posiciones_na_anio[, "row"]],
  Variable = colnames(X_sin_kerosene)[
    posiciones_na_anio[, "col"]
  ],
  Valor_imputado = X_imp_missforest_anio[
    cbind(
      posiciones_na_anio[, "row"],
      posiciones_na_anio[, "col"]
    )
  ]
) |>
  dplyr::mutate(
    Negativo = Valor_imputado < 0
  ) |>
  dplyr::arrange(Variable, Año)

# Crear específicamente el objeto de gas natural
gas_natural_missforest_anio <-
  tabla_imputados_missforest_anio |>
  dplyr::filter(Variable == "Gas natural")

# Mostrar resultado
gas_natural_missforest_anio




comparacion_gas_natural_completa <-
  comparacion_gas_natural |>
  select(
    Año,
    Variable,
    Valor_PCA,
    Valor_missForest
  ) |>
  left_join(
    gas_natural_missforest_anio |>
      select(
        Año,
        Valor_missForest_Año = Valor_imputado
      ),
    by = "Año"
  ) |>
  mutate(
    Valor_observado_2003 = df_num$`Gas natural`[
      df_num$Año == 2003
    ]
  )

comparacion_gas_natural_completa




valor_imputado_2002 <- gas_natural_missforest_anio |>
  filter(Año == 2002) |>
  pull(Valor_imputado)

valor_observado_2003 <- df_num |>
  filter(Año == 2003) |>
  pull(`Gas natural`)

salto_2002_2003 <- valor_observado_2003 -
  valor_imputado_2002

cociente_2002_2003 <- valor_imputado_2002 /
  valor_observado_2003

cat(
  "Valor imputado para 2002:",
  valor_imputado_2002,
  "\n"
)

cat(
  "Valor observado para 2003:",
  valor_observado_2003,
  "\n"
)

cat(
  "Cambio absoluto 2002–2003:",
  salto_2002_2003,
  "\n"
)

cat(
  "Cociente imputado 2002 / observado 2003:",
  cociente_2002_2003,
  "\n"
)




errores_oob_missforest_anio <- tibble(
  Variable = names(X_auxiliar_anio),
  MSE_OOB = as.numeric(
    imp_missforest_anio$OOBerror
  ),
  Tiene_faltantes = map_lgl(
    X_auxiliar_anio,
    ~ any(is.na(.x))
  ),
  Desviacion_observada = map_dbl(
    X_auxiliar_anio,
    ~ sd(.x, na.rm = TRUE)
  )
) |>
  mutate(
    RMSE_OOB = sqrt(MSE_OOB),
    NRMSE_aproximado = case_when(
      Desviacion_observada > 0 ~
        RMSE_OOB / Desviacion_observada,
      TRUE ~ NA_real_
    )
  )

errores_oob_missforest_anio |>
  filter(Tiene_faltantes)





write_csv(
  tabla_imputados_missforest_anio,
  file.path(
    "results",
    "tables",
    "auditoria_imputacion_missforest_con_anio.csv"
  )
)

write_csv(
  comparacion_gas_natural_completa,
  file.path(
    "results",
    "tables",
    "comparacion_imputacion_gas_natural.csv"
  )
)

write_csv(
  errores_oob_missforest_anio,
  file.path(
    "results",
    "tables",
    "errores_oob_missforest_con_anio.csv"
  )
)



resumen_imputacion_missforest_anio
gas_natural_missforest_anio
errores_oob_missforest_anio |>
  filter(Tiene_faltantes)






library(tidyverse)

datos_gas_natural <- df_num |>
  transmute(
    Año,
    Gas_natural = `Gas natural`
  ) |>
  filter(!is.na(Gas_natural))

datos_gas_natural







predecir_gas_natural <- function(datos_entrenamiento,
                                 anios_prediccion,
                                 metodo) {

  nuevos_datos <- tibble(
    Año = anios_prediccion
  )

  if (metodo == "Lineal_original") {

    modelo <- lm(
      Gas_natural ~ Año,
      data = datos_entrenamiento
    )

    prediccion <- predict(
      modelo,
      newdata = nuevos_datos
    )
  }

  if (metodo == "Lineal_log") {

    modelo <- lm(
      log1p(Gas_natural) ~ Año,
      data = datos_entrenamiento
    )

    prediccion <- expm1(
      predict(
        modelo,
        newdata = nuevos_datos
      )
    )
  }

  if (metodo == "Gamma_log") {

    modelo <- glm(
      Gas_natural ~ Año,
      data = datos_entrenamiento,
      family = Gamma(link = "log")
    )

    prediccion <- predict(
      modelo,
      newdata = nuevos_datos,
      type = "response"
    )
  }

  as.numeric(prediccion)
}





metodos_temporales <- c(
  "Lineal_original",
  "Lineal_log",
  "Gamma_log"
)

inicios_validacion <- 2003:2011

validacion_temporal <- purrr::map_dfr(
  inicios_validacion,
  function(inicio) {

    anios_prueba <- inicio:(inicio + 2)

    datos_prueba <- datos_gas_natural |>
      filter(Año %in% anios_prueba)

    datos_entrenamiento <- datos_gas_natural |>
      filter(Año > inicio + 2)

    purrr::map_dfr(
      metodos_temporales,
      function(metodo_actual) {

        predicciones <- predecir_gas_natural(
          datos_entrenamiento = datos_entrenamiento,
          anios_prediccion = datos_prueba$Año,
          metodo = metodo_actual
        )

        tibble(
          Bloque = paste0(
            inicio,
            "–",
            inicio + 2
          ),
          Método = metodo_actual,
          Año = datos_prueba$Año,
          Observado = datos_prueba$Gas_natural,
          Predicho = predicciones,
          Error = Predicho - Observado,
          Error_absoluto = abs(Error),
          Error_cuadrado = Error^2,
          Prediccion_negativa = Predicho < 0
        )
      }
    )
  }
)

validacion_temporal



metricas_validacion_temporal <- validacion_temporal |>
  group_by(Método) |>
  summarise(
    MAE = mean(Error_absoluto),
    RMSE = sqrt(mean(Error_cuadrado)),
    Sesgo = mean(Error),
    Numero_predicciones_negativas =
      sum(Prediccion_negativa),
    .groups = "drop"
  ) |>
  arrange(MAE)

metricas_validacion_temporal



predicciones_2000_2002 <- purrr::map_dfr(
  metodos_temporales,
  function(metodo_actual) {

    valores_predichos <- predecir_gas_natural(
      datos_entrenamiento = datos_gas_natural,
      anios_prediccion = 2000:2002,
      metodo = metodo_actual
    )

    tibble(
      Método = metodo_actual,
      Año = 2000:2002,
      Valor_imputado = valores_predichos,
      Negativo = Valor_imputado < 0
    )
  }
)

predicciones_2000_2002


datos_gas_grafico <- bind_rows(
  datos_gas_natural |>
    mutate(
      Tipo = "Observado",
      Método = "Datos originales"
    ),
  predicciones_2000_2002 |>
    filter(Método %in% c("Lineal_log", "Gamma_log")) |>
    transmute(
      Año,
      Gas_natural = Valor_imputado,
      Tipo = "Imputado",
      Método
    )
)

ggplot(
  datos_gas_grafico,
  aes(
    x = Año,
    y = Gas_natural,
    shape = Tipo,
    linetype = Método
  )
) +
  geom_line() +
  geom_point(size = 2.5) +
  labs(
    title = "Comparación de métodos temporales para gas natural",
    x = "Año",
    y = "Emisiones asociadas al gas natural",
    shape = "Origen",
    linetype = "Método"
  ) +
  theme_classic(base_size = 13)





metricas_validacion_temporal
predicciones_2000_2002






# ============================================================
# PASO 6.1. IMPUTACIÓN TEMPORAL DE GAS NATURAL
# ============================================================

# Extraer las predicciones del modelo log-lineal seleccionado
imputacion_gas_log <- predicciones_2000_2002 |>
  filter(Método == "Lineal_log") |>
  select(
    Año,
    Valor_imputado
  )

imputacion_gas_log


df_hibrido_base <- df_num

indices_gas <- match(
  imputacion_gas_log$Año,
  df_hibrido_base$Año
)


df_hibrido_base$`Gas natural`[indices_gas] <-
  imputacion_gas_log$Valor_imputado

#-
df_hibrido_base |>
  select(Año, `Gas natural`) |>
  filter(Año <= 2004)

sum(is.na(df_hibrido_base$`Gas natural`))
#-
variables_suplementarias <- c(
  "Año",
  "Total"
)

variables_excluidas_modelo <- "Kerosene"

variables_modelo <- setdiff(
  names(df_hibrido_base),
  c(
    variables_suplementarias,
    variables_excluidas_modelo
  )
)

variables_modelo



X_hibrido_sin_anio <- df_hibrido_base |>
  select(all_of(variables_modelo)) |>
  as.data.frame()

tabla_faltantes_restantes <- df_hibrido_base |>
  select(Año, all_of(variables_modelo)) |>
  pivot_longer(
    cols = -Año,
    names_to = "Variable",
    values_to = "Valor"
  ) |>
  filter(is.na(Valor)) |>
  count(
    Variable,
    name = "Numero_faltantes"
  )

tabla_faltantes_restantes

sum(is.na(X_hibrido_sin_anio))


#-
X_hibrido_auxiliar <- bind_cols(
  Año = df_hibrido_base$Año,
  X_hibrido_sin_anio
) |>
  as.data.frame()


library(missForest)

set.seed(123)

imp_hibrida_missforest <- missForest(
  xmis = X_hibrido_auxiliar,
  maxiter = 20,
  ntree = 500,
  mtry = max(
    1,
    floor(sqrt(ncol(X_hibrido_auxiliar)))
  ),
  variablewise = TRUE,
  verbose = TRUE,
  parallelize = "no"
)


datos_hibridos_completos <- as.data.frame(
  imp_hibrida_missforest$ximp
)

X_hibrido_imputado <- datos_hibridos_completos |>
  select(-Año)



#-
posiciones_na_hibridas <- which(
  is.na(X_hibrido_sin_anio),
  arr.ind = TRUE
)

tabla_imputados_hibridos_mf <- tibble(
  Año = df_hibrido_base$Año[
    posiciones_na_hibridas[, "row"]
  ],
  Variable = colnames(X_hibrido_sin_anio)[
    posiciones_na_hibridas[, "col"]
  ],
  Valor_imputado = X_hibrido_imputado[
    cbind(
      posiciones_na_hibridas[, "row"],
      posiciones_na_hibridas[, "col"]
    )
  ]
) |>
  mutate(
    Método = "missForest con Año",
    Negativo = Valor_imputado < 0
  ) |>
  arrange(Variable, Año)

tabla_imputados_hibridos_mf


tabla_imputados_gas <- imputacion_gas_log |>
  transmute(
    Año,
    Variable = "Gas natural",
    Valor_imputado,
    Método = "Regresión lineal sobre log1p",
    Negativo = Valor_imputado < 0
  )

tabla_imputaciones_finales <- bind_rows(
  tabla_imputados_gas,
  tabla_imputados_hibridos_mf
) |>
  arrange(Variable, Año)

tabla_imputaciones_finales


nrow(tabla_imputaciones_finales)


resumen_imputacion_hibrida <- tabla_imputaciones_finales |>
  summarise(
    Numero_imputados = n(),
    Numero_negativos = sum(Negativo),
    Valor_minimo = min(Valor_imputado),
    Valor_maximo = max(Valor_imputado)
  )

resumen_imputacion_hibrida

#-
colSums(is.na(X_hibrido_imputado))
#-
data_num_imputed_hibrida <- bind_cols(
  df_num |>
    select(
      Año,
      Total,
      Kerosene
    ),
  X_hibrido_imputado
)




orden_columnas <- names(df_num)

data_num_imputed_hibrida <- data_num_imputed_hibrida |>
  select(any_of(orden_columnas))

#-
dim(data_num_imputed_hibrida)

colSums(
  is.na(
    data_num_imputed_hibrida |>
      select(-Kerosene)
  )
)
#-
errores_oob_hibridos <- tibble(
  Variable = names(X_hibrido_auxiliar),
  MSE_OOB = as.numeric(
    imp_hibrida_missforest$OOBerror
  ),
  Tiene_faltantes = map_lgl(
    X_hibrido_auxiliar,
    ~ any(is.na(.x))
  ),
  Desviacion_observada = map_dbl(
    X_hibrido_auxiliar,
    ~ sd(.x, na.rm = TRUE)
  )
) |>
  mutate(
    RMSE_OOB = sqrt(MSE_OOB),
    NRMSE_aproximado = case_when(
      Desviacion_observada > 0 ~
        RMSE_OOB / Desviacion_observada,
      TRUE ~ NA_real_
    )
  )

errores_oob_hibridos |>
  filter(Tiene_faltantes)

#-
ruta_datos_procesados <- file.path(
  "data",
  "processed"
)

ruta_tablas <- file.path(
  "results",
  "tables"
)

dir.create(
  ruta_datos_procesados,
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  ruta_tablas,
  recursive = TRUE,
  showWarnings = FALSE
)

readr::write_csv(
  data_num_imputed_hibrida,
  file.path(
    ruta_datos_procesados,
    "data_num_imputed_hybrid.csv"
  ),
  na = ""
)

readr::write_csv(
  tabla_imputaciones_finales,
  file.path(
    ruta_tablas,
    "auditoria_imputacion_hibrida.csv"
  )
)

readr::write_csv(
  errores_oob_hibridos,
  file.path(
    ruta_tablas,
    "errores_oob_imputacion_hibrida.csv"
  )
)

#-
resumen_imputacion_hibrida

tabla_imputaciones_finales

errores_oob_hibridos |>
  filter(Tiene_faltantes)



#-
library(tidyverse)
library(missForest)

variables_terminales <- c(
  "Bagazo de caña",
  "Coque",
  "Leña",
  "Otros primarias"
)

# Cada valor representa el último año del bloque de prueba
finales_bloque <- 2011:2018

#-
evaluar_bloque_terminal <- function(variable_objetivo,
                                    fin_bloque,
                                    datos,
                                    variables_predictoras) {

  inicio_bloque <- fin_bloque - 3
  anios_prueba <- inicio_bloque:fin_bloque

  # Se corta la base en el último año del bloque.
  # Así no se permite que el método utilice información futura.
  datos_corte <- datos |>
    filter(Año <= fin_bloque)

  observados <- datos_corte |>
    filter(Año %in% anios_prueba) |>
    select(
      Año,
      Observado = all_of(variable_objetivo)
    )

  # Si el bloque contiene algún NA original, no puede validarse.
  if (
    nrow(observados) != 4 ||
    any(is.na(observados$Observado))
  ) {
    return(tibble())
  }

  datos_entrenamiento <- datos_corte |>
    filter(Año < inicio_bloque) |>
    select(
      Año,
      Valor = all_of(variable_objetivo)
    ) |>
    filter(!is.na(Valor))

  if (nrow(datos_entrenamiento) < 6) {
    return(tibble())
  }

  nuevos_datos <- tibble(
    Año = anios_prueba
  )

  # ----------------------------------------------------------
  # Método 1. Regresión lineal en escala original
  # ----------------------------------------------------------

  modelo_lineal <- lm(
    Valor ~ Año,
    data = datos_entrenamiento
  )

  pred_lineal <- as.numeric(
    predict(
      modelo_lineal,
      newdata = nuevos_datos
    )
  )

  # ----------------------------------------------------------
  # Método 2. Regresión lineal en escala logarítmica
  # ----------------------------------------------------------

  modelo_log <- lm(
    log1p(Valor) ~ Año,
    data = datos_entrenamiento
  )

  pred_log <- expm1(
    predict(
      modelo_log,
      newdata = nuevos_datos
    )
  ) |>
    as.numeric()

  # ----------------------------------------------------------
  # Método 3. Promedio de las últimas tres observaciones
  # ----------------------------------------------------------

  media_ultimos_3 <- mean(
    tail(datos_entrenamiento$Valor, 3)
  )

  pred_media3 <- rep(
    media_ultimos_3,
    length(anios_prueba)
  )

  # ----------------------------------------------------------
  # Método 4. missForest con Año como predictor auxiliar
  # ----------------------------------------------------------

  matriz_mf <- datos_corte |>
    select(
      Año,
      all_of(variables_predictoras)
    ) |>
    as.data.frame()

  filas_prueba <- matriz_mf$Año %in% anios_prueba

  # Ocultar únicamente la variable objetivo en el bloque.
  matriz_mf[
    filas_prueba,
    variable_objetivo
  ] <- NA_real_

  set.seed(
    123 +
      fin_bloque +
      match(
        variable_objetivo,
        variables_terminales
      )
  )

  imputacion_mf <- missForest(
    xmis = matriz_mf,
    maxiter = 15,
    ntree = 300,
    mtry = max(
      1,
      floor(sqrt(ncol(matriz_mf)))
    ),
    variablewise = FALSE,
    verbose = FALSE,
    parallelize = "no"
  )

  pred_missforest <- imputacion_mf$ximp[
    filas_prueba,
    variable_objetivo
  ] |>
    as.numeric()

  # ----------------------------------------------------------
  # Organizar predicciones
  # ----------------------------------------------------------

  predicciones <- bind_rows(
    tibble(
      Método = "Lineal_original",
      Predicho = pred_lineal
    ),
    tibble(
      Método = "Lineal_log",
      Predicho = pred_log
    ),
    tibble(
      Método = "Media_ultimos_3",
      Predicho = pred_media3
    ),
    tibble(
      Método = "missForest_con_Año",
      Predicho = pred_missforest
    )
  ) |>
    group_by(Método) |>
    mutate(
      Año = anios_prueba
    ) |>
    ungroup() |>
    left_join(
      observados,
      by = "Año"
    ) |>
    mutate(
      Variable = variable_objetivo,
      Bloque = paste0(
        inicio_bloque,
        "–",
        fin_bloque
      ),
      Error = Predicho - Observado,
      Error_absoluto = abs(Error),
      Error_cuadrado = Error^2,
      Prediccion_negativa = Predicho < 0
    ) |>
    select(
      Variable,
      Bloque,
      Método,
      Año,
      Observado,
      Predicho,
      Error,
      Error_absoluto,
      Error_cuadrado,
      Prediccion_negativa
    )

  predicciones
}

#-
diseño_validacion <- tidyr::crossing(
  Variable = variables_terminales,
  Fin_bloque = finales_bloque
)

validacion_bloques_terminales <- purrr::pmap_dfr(
  diseño_validacion,
  function(Variable, Fin_bloque) {

    evaluar_bloque_terminal(
      variable_objetivo = Variable,
      fin_bloque = Fin_bloque,
      datos = df_hibrido_base,
      variables_predictoras = variables_modelo
    )
  }
)

validacion_bloques_terminales
#-

metricas_bloques_terminales <- validacion_bloques_terminales |>
  group_by(
    Variable,
    Método
  ) |>
  summarise(
    Numero_predicciones = n(),
    MAE = mean(
      Error_absoluto,
      na.rm = TRUE
    ),
    RMSE = sqrt(
      mean(
        Error_cuadrado,
        na.rm = TRUE
      )
    ),
    Sesgo = mean(
      Error,
      na.rm = TRUE
    ),
    Negativos = sum(
      Prediccion_negativa,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) |>
  arrange(
    Variable,
    MAE
  )

metricas_bloques_terminales


#-
ranking_metodos_terminales <- metricas_bloques_terminales |>
  filter(Negativos == 0) |>
  group_by(Variable) |>
  arrange(
    MAE,
    RMSE,
    .by_group = TRUE
  ) |>
  mutate(
    Posicion = row_number()
  ) |>
  ungroup()

ranking_metodos_terminales


#-

mejor_metodo_por_variable <- ranking_metodos_terminales |>
  filter(Posicion == 1)

mejor_metodo_por_variable

#-

metricas_por_bloque <- validacion_bloques_terminales |>
  group_by(
    Variable,
    Método,
    Bloque
  ) |>
  summarise(
    MAE = mean(Error_absoluto),
    RMSE = sqrt(mean(Error_cuadrado)),
    Sesgo = mean(Error),
    .groups = "drop"
  ) |>
  arrange(
    Variable,
    Método,
    Bloque
  )

metricas_por_bloque


#-

write_csv(
  validacion_bloques_terminales,
  file.path(
    "results",
    "tables",
    "validacion_bloques_terminales.csv"
  )
)

write_csv(
  metricas_bloques_terminales,
  file.path(
    "results",
    "tables",
    "metricas_validacion_bloques_terminales.csv"
  )
)

write_csv(
  ranking_metodos_terminales,
  file.path(
    "results",
    "tables",
    "ranking_metodos_imputacion_terminal.csv"
  )
)

#-
mejor_metodo_por_variable


metricas_bloques_terminales |>
  arrange(Variable, MAE)



#-
library(tidyverse)
library(missForest)

df_imputado_final <- df_num

anios_terminales <- 2019:2022


#-
datos_gas_observados <- df_num |>
  transmute(
    Año,
    Valor = `Gas natural`
  ) |>
  filter(!is.na(Valor))

modelo_gas_log <- lm(
  log1p(Valor) ~ Año,
  data = datos_gas_observados
)

pred_gas <- tibble(
  Año = 2000:2002
) |>
  mutate(
    Valor_imputado = expm1(
      predict(
        modelo_gas_log,
        newdata = pick(Año)
      )
    )
  )

pred_gas

#-
indices_gas <- match(
  pred_gas$Año,
  df_imputado_final$Año
)

df_imputado_final$`Gas natural`[indices_gas] <-
  pred_gas$Valor_imputado

#-

media_bagazo_ultimos3 <- df_num |>
  filter(
    Año < 2019,
    !is.na(`Bagazo de caña`)
  ) |>
  slice_max(
    order_by = Año,
    n = 3,
    with_ties = FALSE
  ) |>
  summarise(
    Media = mean(`Bagazo de caña`)
  ) |>
  pull(Media)

media_bagazo_ultimos3

df_imputado_final <- df_imputado_final |>
  mutate(
    `Bagazo de caña` = if_else(
      Año %in% anios_terminales &
        is.na(`Bagazo de caña`),
      media_bagazo_ultimos3,
      `Bagazo de caña`
    )
  )

#-
media_coque_ultimos3 <- df_num |>
  filter(
    Año < 2019,
    !is.na(Coque)
  ) |>
  slice_max(
    order_by = Año,
    n = 3,
    with_ties = FALSE
  ) |>
  summarise(
    Media = mean(Coque)
  ) |>
  pull(Media)

media_coque_ultimos3

df_imputado_final <- df_imputado_final |>
  mutate(
    Coque = if_else(
      Año %in% anios_terminales &
        is.na(Coque),
      media_coque_ultimos3,
      Coque
    )
  )

#-


datos_otros_observados <- df_num |>
  transmute(
    Año,
    Valor = `Otros primarias`
  ) |>
  filter(!is.na(Valor))

modelo_otros_lineal <- lm(
  Valor ~ Año,
  data = datos_otros_observados
)

pred_otros <- tibble(
  Año = anios_terminales
) |>
  mutate(
    Valor_imputado = as.numeric(
      predict(
        modelo_otros_lineal,
        newdata = pick(Año)
      )
    )
  )

pred_otros

#-
if (any(pred_otros$Valor_imputado < 0)) {
  stop(
    "La regresión produjo valores negativos para Otros primarias."
  )
}
#-
indices_otros <- match(
  pred_otros$Año,
  df_imputado_final$Año
)

df_imputado_final$`Otros primarias`[indices_otros] <-
  pred_otros$Valor_imputado


#-
variables_suplementarias <- c(
  "Año",
  "Total"
)

variables_excluidas <- "Kerosene"

variables_modelo_final <- setdiff(
  names(df_imputado_final),
  c(
    variables_suplementarias,
    variables_excluidas
  )
)

faltantes_antes_leña <- df_imputado_final |>
  select(
    Año,
    all_of(variables_modelo_final)
  ) |>
  pivot_longer(
    cols = -Año,
    names_to = "Variable",
    values_to = "Valor"
  ) |>
  filter(is.na(Valor))

faltantes_antes_leña


#-
if (
  nrow(faltantes_antes_leña) != 4 ||
  any(faltantes_antes_leña$Variable != "Leña")
) {
  stop(
    "Existen faltantes inesperados antes de imputar Leña."
  )
}


#-
X_final_auxiliar <- df_imputado_final |>
  select(
    Año,
    all_of(variables_modelo_final)
  ) |>
  as.data.frame()

set.seed(123)

imp_leña_final <- missForest(
  xmis = X_final_auxiliar,
  maxiter = 20,
  ntree = 500,
  mtry = max(
    1,
    floor(sqrt(ncol(X_final_auxiliar)))
  ),
  variablewise = TRUE,
  verbose = TRUE,
  parallelize = "no"
)

#-
X_final_completo <- as.data.frame(
  imp_leña_final$ximp
)

pred_leña <- tibble(
  Año = X_final_completo$Año,
  Valor_imputado = X_final_completo$Leña
) |>
  filter(Año %in% anios_terminales)

pred_leña

#-

indices_leña <- match(
  pred_leña$Año,
  df_imputado_final$Año
)

df_imputado_final$Leña[indices_leña] <-
  pred_leña$Valor_imputado

#-
tabla_imputacion_gas_final <- pred_gas |>
  transmute(
    Año,
    Variable = "Gas natural",
    Valor_imputado,
    Método = "Regresión lineal sobre log1p",
    Negativo = Valor_imputado < 0
  )

tabla_imputacion_bagazo_final <- tibble(
  Año = anios_terminales,
  Variable = "Bagazo de caña",
  Valor_imputado = media_bagazo_ultimos3,
  Método = "Media de las últimas 3 observaciones",
  Negativo = Valor_imputado < 0
)

tabla_imputacion_coque_final <- tibble(
  Año = anios_terminales,
  Variable = "Coque",
  Valor_imputado = media_coque_ultimos3,
  Método = "Media de las últimas 3 observaciones",
  Negativo = Valor_imputado < 0
)

tabla_imputacion_otros_final <- pred_otros |>
  transmute(
    Año,
    Variable = "Otros primarias",
    Valor_imputado,
    Método = "Regresión lineal",
    Negativo = Valor_imputado < 0
  )

tabla_imputacion_leña_final <- pred_leña |>
  transmute(
    Año,
    Variable = "Leña",
    Valor_imputado,
    Método = "missForest con Año",
    Negativo = Valor_imputado < 0
  )

tabla_imputaciones_definitivas <- bind_rows(
  tabla_imputacion_bagazo_final,
  tabla_imputacion_coque_final,
  tabla_imputacion_gas_final,
  tabla_imputacion_leña_final,
  tabla_imputacion_otros_final
) |>
  arrange(Variable, Año)

tabla_imputaciones_definitivas

#-
stopifnot(
  nrow(tabla_imputaciones_definitivas) == 19
)

stopifnot(
  sum(tabla_imputaciones_definitivas$Negativo) == 0
)


#-
datos_analisis_final <- df_imputado_final |>
  select(
    Año,
    Total,
    all_of(variables_modelo_final)
  )


#-
resumen_base_final <- tibble(
  Numero_años = nrow(datos_analisis_final),
  Numero_combustibles = length(variables_modelo_final),
  Numero_faltantes = sum(is.na(datos_analisis_final)),
  Numero_negativos_imputados =
    sum(tabla_imputaciones_definitivas$Negativo)
)

resumen_base_final

#-
auditoria_rangos_finales <- datos_analisis_final |>
  select(all_of(variables_modelo_final)) |>
  summarise(
    across(
      everything(),
      list(
        Minimo = min,
        Maximo = max
      )
    )
  )

auditoria_rangos_finales


#-
ruta_datos_procesados <- file.path(
  "data",
  "processed"
)

ruta_tablas <- file.path(
  "results",
  "tables"
)

dir.create(
  ruta_datos_procesados,
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  ruta_tablas,
  recursive = TRUE,
  showWarnings = FALSE
)

write_csv(
  datos_analisis_final,
  file.path(
    ruta_datos_procesados,
    "data_analysis_final.csv"
  )
)

write_csv(
  tabla_imputaciones_definitivas,
  file.path(
    ruta_tablas,
    "auditoria_imputaciones_definitivas.csv"
  )
)

write_csv(
  resumen_base_final,
  file.path(
    ruta_tablas,
    "resumen_base_analitica_final.csv"
  )
)

write_csv(
  metricas_bloques_terminales,
  file.path(
    ruta_tablas,
    "metricas_seleccion_metodos_imputacion.csv"
  )
)




##########################################################################
#
# correlaciones→KMO y Bartlett→seleccioˊn de variables→PCA→cluˊsteres
#
###########################################################################

