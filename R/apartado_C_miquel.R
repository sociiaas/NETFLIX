################################################################################
# TRABAJO FINAL DECISIONES EMPRESARIALES — NETFLIX (REDUCCIÓN DE CHURN)
# APARTADO C: APLICACIÓN DE LA TEORÍA (MIQUEL — SUBAPARTADOS C.1, C.2, C.3 y C.4)
#
# Alcance estricto:
#   - C.1. Árboles de decisión (modelo principal, sensibilidad y predicción)
#   - C.2. Impureza de Gini (fórmula, curva teórica y selección del nodo raíz)
#   - C.3. Sobreajuste (partición 70/30, estimación vs. evaluación, poda)
#   - C.4. Matriz de confusión (aciertos y errores, Accuracy, Sensitivity y Specificity)
#   - Aplicación obligatoria de las 4 "Observacions Tomas"
#
# Metodología estricta según /apuntes:
#   - Librerías exclusivas: rpart, rpart.plot y R base
#   - Rutas relativas a la carpeta raíz del proyecto
################################################################################


################################################################################
# 1. LIMPIAR ENTORNO Y CARGAR LIBRERÍAS
################################################################################

rm(list = ls())

library(rpart)
library(rpart.plot)

# Asegurar que el directorio de trabajo es la raíz del proyecto
if (!dir.exists("datos") && dir.exists("../datos")) {
  setwd("..")
}

# Crear carpeta de salida para gráficos de alta resolución
dir.create("graficos", recursive = TRUE, showWarnings = FALSE)


################################################################################
# 2. CARGA Y DEPURACIÓN DE DATOS (OBSERVACIÓN TOMÁS #1: ELIMINAR REPETIDOS)
################################################################################

library(readr)
netflix_raw  <- read_csv("C:/Users/table/Desktop/MASTER/DECISIONS/NETFLIX/datos/netflix_churn_enhanced.csv")


cat("=== 2. DEPURACIÓN INICIAL (OBSERVACIÓN TOMÁS #1) ===\n")
cat("Registros originales en el CSV:", nrow(netflix_raw), "\n")
cat("Filas exactamente duplicadas:", sum(duplicated(netflix_raw)), "\n")
cat("IDs de cliente duplicados:", sum(duplicated(netflix_raw$customer_id)), "\n")

# Eliminación de observaciones repetidas
netflix <- netflix_raw[!duplicated(netflix_raw), ]
cat("Registros únicos tras deduplicar:", nrow(netflix), "\n")

# Corrección de erratas menores e inconsistencias de formato con R base
# 1) Edades imposibles (< 13 o > 100) se pasan a NA
netflix$age[!is.na(netflix$age) & (netflix$age < 13 | netflix$age > 100)] <- NA

# 2) Unificación de mayúsculas/espacios en gender
netflix$gender <- trimws(netflix$gender)
netflix$gender[netflix$gender == "F"]     <- "Female"
netflix$gender[netflix$gender == "male"]  <- "Male"
netflix$gender[netflix$gender == "other"] <- "Other"

# 3) Unificación de mayúsculas/minúsculas en device
netflix$device <- tolower(trimws(netflix$device))
netflix$device[netflix$device == "tv"]      <- "TV"
netflix$device[netflix$device == "desktop"] <- "Desktop"
netflix$device[netflix$device == "laptop"]  <- "Laptop"
netflix$device[netflix$device == "mobile"]  <- "Mobile"
netflix$device[netflix$device == "tablet"]  <- "Tablet"

# 4) Cadenas vacías en payment_method pasan a NA
netflix$payment_method[trimws(netflix$payment_method) == ""] <- NA

# Conversión a factor de variables categóricas (como en apuntes)
vars_categoricas <- c(
  "gender", "country", "region", "subscription_type",
  "promo_used", "acquisition_channel", "payment_method",
  "auto_renew", "device", "favorite_genre"
)

for (col in vars_categoricas) {
  netflix[[col]] <- as.factor(netflix[[col]])
}

# Variable objetivo categórica con etiquetas claras
netflix$churned_f <- factor(
  netflix$churned,
  levels = c(0, 1),
  labels = c("Continua", "Cancela")
)

cat("\nEstructura del dataset depurado:\n")
str(netflix)

cat("\nDistribución global de cancelación (n = 12,000):\n")
print(table(netflix$churned_f))
print(round(prop.table(table(netflix$churned_f)), 4))


################################################################################
# 3. PARTICIÓN EN GRUPO DE ESTIMACIÓN (70%) Y EVALUACIÓN (30%)
################################################################################

# Misma metodología reproducible que 2. respuesta_estimacion_evaluacion.R.txt
set.seed(123)

train_index <- sample(
  1:nrow(netflix),
  size = 0.7 * nrow(netflix)
)

train_all <- netflix[train_index, ]
test_all  <- netflix[-train_index, ]

cat("\n=== 3. PARTICIÓN 70/30 ===\n")
cat("Grupo de estimación (train):", nrow(train_all), "observaciones\n")
cat("Grupo de evaluación (test) :", nrow(test_all), "observaciones\n")


################################################################################
# 4. OBSERVACIÓN TOMÁS #2: PRIMER NODO — ¿CAUSA O CONSECUENCIA?
################################################################################

cat("\n=== 4. AUDITORÍA CAUSA VS. CONSECUENCIA (OBSERVACIÓN TOMÁS #2) ===\n")
cat("Resumen de tenure_months según estado de suscripción:\n")
print(tapply(netflix$tenure_months, netflix$churned_f, summary))

# Verificación contra fechas reales del registro
fecha_alta <- as.Date(netflix$signup_date)
fecha_baja <- as.Date(ifelse(netflix$churn_date == "", NA, netflix$churn_date))
fecha_corte <- max(c(fecha_alta, fecha_baja), na.rm = TRUE)

idx_canc <- which(netflix$churned == 1)
idx_cont <- which(netflix$churned == 0)

diff_canc <- as.numeric(fecha_baja[idx_canc] - fecha_alta[idx_canc]) / 30.44 - netflix$tenure_months[idx_canc]
diff_cont <- as.numeric(fecha_corte - fecha_alta[idx_cont]) / 30.44 - netflix$tenure_months[idx_cont]

cat("\nDiferencia (meses reales - tenure_months) en cancelados:\n")
print(round(summary(diff_canc), 3))
cat("Diferencia (meses hasta fecha corte - tenure_months) en activos:\n")
print(round(summary(diff_cont), 3))

cat(sprintf(
  "Correlación con churned -> tenure_months: %.3f | lifetime_revenue: %.3f\n",
  cor(netflix$churned, netflix$tenure_months),
  cor(netflix$churned, netflix$lifetime_revenue)
))

# Modelo inicial CON fuga (incluyendo tenure_months y lifetime_revenue)
cols_fuga_id <- c("customer_id", "signup_date", "churn_date", "churn_reason", "churned")
train_con_fuga <- train_all[, !(names(train_all) %in% cols_fuga_id)]

arbol_con_fuga <- rpart(
  churned_f ~ .,
  data = train_con_fuga,
  method = "class"
)

cat("\nÁrbol inicial CON tenure_months y lifetime_revenue (demostración de fuga):\n")
print(arbol_con_fuga)

# Exportar gráfico 01: Modelo inicial con fuga
png(
  "graficos/01_arbol_con_fuga_tenure.png",
  width = 2000, height = 1200, res = 210
)
rpart.plot(
  arbol_con_fuga,
  type = 2,
  extra = 104,
  fallen.leaves = TRUE,
  box.palette = list("Blues", "Reds"),
  shadow.col = "gray88",
  roundint = FALSE,
  tweak = 1.05,
  main = "Modelo inicial (con fuga): tenure_months domina el nodo raiz"
)
dev.off()


################################################################################
# 5. OBSERVACIÓN TOMÁS #3: INVESTIGACIÓN DE "last_login_days"
################################################################################

cat("\n=== 5. INVESTIGACIÓN DE last_login_days (OBSERVACIÓN TOMÁS #3) ===\n")
cat("Resumen de last_login_days por estado:\n")
print(tapply(netflix$last_login_days, netflix$churned_f, summary))

# Tasa de cancelación por tramos de 10 días
tramos_login <- cut(
  netflix$last_login_days,
  breaks = c(-1, 10, 20, 30, 40, 50, 60),
  labels = c("0-10d", "11-20d", "21-30d", "31-40d", "41-50d", "51-60d")
)
tabla_login <- table(tramos_login, netflix$churned_f)
tasa_login <- round(100 * prop.table(tabla_login, 1)[, "Cancela"], 1)
print(cbind(tabla_login, Tasa_Cancela_Pct = tasa_login))

# Mediana de last_login_days por año de baja (para descartar acumulación post-baja)
anio_baja <- substr(as.character(netflix$churn_date[idx_canc]), 1, 4)
mediana_por_anio <- tapply(netflix$last_login_days[idx_canc], anio_baja, median)
cat("\nMediana de last_login_days en cancelados según año de baja:\n")
print(mediana_por_anio)

# Exportar gráfico 02: Dos paneles de diagnóstico sobre last_login_days
png(
  "graficos/02_investigacion_last_login.png",
  width = 2200, height = 1100, res = 220
)
par(mfrow = c(1, 2), mar = c(5, 4.5, 4, 1.5))

# Panel A: Tasa de cancelación por tramo de inactividad
colores_tramos <- c("#3B5B7C", "#3B5B7C", "#3B5B7C", "#E50914", "#E50914", "#E50914")
bp1 <- barplot(
  tasa_login,
  col = colores_tramos,
  border = NA,
  ylim = c(0, 75),
  ylab = "Tasa de cancelacion (%)",
  xlab = "Dias desde el ultimo acceso (last_login_days)",
  main = "Salto de riesgo a partir de 31 dias sin acceso",
  cex.names = 0.95,
  cex.axis = 0.95,
  cex.lab = 1.0,
  cex.main = 1.05
)
abline(h = 100 * mean(netflix$churned), lty = 2, col = "#64748B", lwd = 1.5)
text(x = bp1, y = tasa_login + 3, labels = paste0(tasa_login, "%"), cex = 0.9, font = 2, col = "#1E293B")
legend(
  "topleft",
  legend = c("<= 30 dias (bajo/medio)", ">= 31 dias (alerta roja)", "Media global (40.0%)"),
  fill = c("#3B5B7C", "#E50914", NA),
  border = c(NA, NA, NA),
  lty = c(NA, NA, 2),
  col = c(NA, NA, "#64748B"),
  lwd = c(NA, NA, 1.5),
  bty = "n",
  cex = 0.85
)

# Panel B: Mediana por año de baja (ventana constante 0-60 días)
bp2 <- barplot(
  mediana_por_anio,
  col = "#475569",
  border = NA,
  ylim = c(0, 68),
  ylab = "Mediana de last_login_days (dias)",
  xlab = "Año en que se produjo la baja",
  main = "Estabilidad por año: ventana maxima de 60 dias",
  cex.names = 0.95,
  cex.axis = 0.95,
  cex.lab = 1.0,
  cex.main = 1.05
)
abline(h = 60, lty = 2, col = "#E50914", lwd = 1.5)
text(x = bp2, y = mediana_por_anio + 2.8, labels = paste0(mediana_por_anio, " d"), cex = 0.9, font = 2, col = "#1E293B")
text(x = mean(bp2), y = 63, labels = "Tope maximo observado = 60 dias (no acumula post-baja)", cex = 0.82, col = "#E50914", font = 2)

dev.off()


################################################################################
# 6. C.1. ÁRBOL DE DECISIÓN PRINCIPAL (SIN FUGA) Y ANÁLISIS DE SENSIBILIDAD
################################################################################

# Excluimos identificadores, fechas, motivo post-baja y variables de consecuencia
cols_excluir_modelo <- c(
  "customer_id", "signup_date", "churn_date", "churn_reason",
  "churned", "tenure_months", "lifetime_revenue"
)

train_modelo <- train_all[, !(names(train_all) %in% cols_excluir_modelo)]
test_modelo  <- test_all[, !(names(test_all) %in% cols_excluir_modelo)]

# Modelo principal podado por defecto (cp = 0.01)
arbol_principal <- rpart(
  churned_f ~ .,
  data = train_modelo,
  method = "class"
)

cat("\n=== 6. C.1. ÁRBOL DE DECISIÓN PRINCIPAL (SIN FUGA, PODADO) ===\n")
print(arbol_principal)
cat("\nImportancia de variables (Modelo Principal):\n")
print(round(arbol_principal$variable.importance, 1))

# Exportar gráfico 03: Árbol principal podado
png(
  "graficos/03_arbol_principal_podado.png",
  width = 2900, height = 1600, res = 220
)
rpart.plot(
  arbol_principal,
  type = 2,
  extra = 104,
  fallen.leaves = TRUE,
  box.palette = list("Blues", "Reds"),
  shadow.col = "gray88",
  roundint = FALSE,
  tweak = 1.02,
  split.fun = function(x, labs, digits, varlen, faclen) {
    gsub(" = ", "\n= ", labs)
  },
  nn = FALSE,
  main = "Arbol de Decision Principal (Podado, cp = 0.01) — Sin fuga de informacion"
)
dev.off()

# Análisis de sensibilidad: árbol EXCLUYENDO last_login_days
train_sin_ll <- train_modelo[, names(train_modelo) != "last_login_days"]
test_sin_ll  <- test_modelo[, names(test_modelo) != "last_login_days"]

arbol_sin_last_login <- rpart(
  churned_f ~ .,
  data = train_sin_ll,
  method = "class"
)

acc_train_sin_ll <- mean(predict(arbol_sin_last_login, train_sin_ll, type = "class") == train_sin_ll$churned_f)
acc_test_sin_ll  <- mean(predict(arbol_sin_last_login, test_sin_ll, type = "class") == test_sin_ll$churned_f)

cat("\n=== SENSIBILIDAD: ÁRBOL SIN last_login_days ===\n")
print(arbol_sin_last_login)
cat(sprintf(
  "Hojas: %d | Acierto Estimación: %.2f%% | Acierto Evaluación: %.2f%%\n",
  sum(arbol_sin_last_login$frame$var == "<leaf>"),
  100 * acc_train_sin_ll,
  100 * acc_test_sin_ll
))

# Exportar gráfico 04: Árbol de sensibilidad sin last_login_days
png(
  "graficos/04_arbol_sensibilidad_sin_last_login.png",
  width = 2800, height = 1550, res = 220
)
rpart.plot(
  arbol_sin_last_login,
  type = 2,
  extra = 104,
  fallen.leaves = TRUE,
  box.palette = list("Blues", "Reds"),
  shadow.col = "gray88",
  roundint = FALSE,
  tweak = 1.02,
  split.fun = function(x, labs, digits, varlen, faclen) {
    gsub(" = ", "\n= ", labs)
  },
  main = "Analisis de sensibilidad: Arbol sin la variable last_login_days"
)
dev.off()

# Predicción para 3 nuevos clientes (como en bloques 8-12 de apuntes)
nuevos_clientes <- data.frame(
  age = c(34, 48, 29),
  gender = factor(c("Female", "Male", "Other"), levels = levels(train_modelo$gender)),
  country = factor(c("Spain", "United States", "Mexico"), levels = levels(train_modelo$country)),
  region = factor(c("Europe", "North America", "North America"), levels = levels(train_modelo$region)),
  subscription_type = factor(c("Premium", "Basic", "Standard"), levels = levels(train_modelo$subscription_type)),
  monthly_fee = c(17.99, 8.99, 13.99),
  promo_used = factor(c("No", "Yes", "No"), levels = levels(train_modelo$promo_used)),
  discount_percent = c(0, 20, 0),
  acquisition_channel = factor(c("Organic", "Paid Ads", "Referral"), levels = levels(train_modelo$acquisition_channel)),
  payment_method = factor(c("Credit Card", "Gift Card", "PayPal"), levels = levels(train_modelo$payment_method)),
  auto_renew = factor(c("Yes", "No", "Yes"), levels = levels(train_modelo$auto_renew)),
  payment_failures = c(0, 1, 0),
  device = factor(c("TV", "Mobile", "Laptop"), levels = levels(train_modelo$device)),
  number_of_profiles = c(4, 1, 2),
  favorite_genre = factor(c("Drama", "Action", "Comedy"), levels = levels(train_modelo$favorite_genre)),
  watch_hours = c(25.5, 3.0, 9.0),
  avg_watch_time_per_day = c(0.85, 0.10, 0.30),
  avg_session_minutes = c(55, 18, 35),
  sessions_per_month = c(28, 4, 12),
  last_login_days = c(5, 42, 38),
  support_tickets = c(0, 3, 1),
  satisfaction_score = c(9, 3, 6)
)

pred_nuevos_clase <- predict(arbol_principal, nuevos_clientes, type = "class")
pred_nuevos_prob  <- predict(arbol_principal, nuevos_clientes, type = "prob")

cat("\n=== PREDICCIÓN PARA 3 NUEVOS CLIENTES (type = 'class' y 'prob') ===\n")
resumen_nuevos <- data.frame(
  Perfil = c("Cliente 1 (Fiel / Activo)", "Cliente 2 (Riesgo Critico)", "Cliente 3 (Alerta Temprana)"),
  Uso_Dia = nuevos_clientes$avg_watch_time_per_day,
  Dias_Sin_Login = nuevos_clientes$last_login_days,
  Plan = nuevos_clientes$subscription_type,
  Pago = nuevos_clientes$payment_method,
  Perfiles = nuevos_clientes$number_of_profiles,
  Prediccion = pred_nuevos_clase,
  Prob_Continua = round(pred_nuevos_prob[, "Continua"], 4),
  Prob_Cancela  = round(pred_nuevos_prob[, "Cancela"], 4)
)
print(resumen_nuevos)


################################################################################
# 7. OBSERVACIÓN TOMÁS #4: INCLUSIÓN RIGUROSA DE "churn_reason"
################################################################################

cat("\n=== 7. INTEGRACIÓN DE churn_reason (OBSERVACIÓN TOMÁS #4) ===\n")
cat("Comprobación de fuga: relación churn_reason vs churned_f:\n")
print(table(netflix$churn_reason, netflix$churned_f))

cancelados <- netflix[netflix$churned == 1, ]
cancelados$churn_reason <- as.factor(cancelados$churn_reason)

tabla_motivos <- sort(table(cancelados$churn_reason), decreasing = FALSE)
pct_motivos   <- round(100 * prop.table(tabla_motivos), 1)
cat("\nDistribución de motivos de cancelación (n = 4,803):\n")
print(cbind(Cancelaciones = rev(tabla_motivos), Porcentaje = rev(pct_motivos)))

cat("\nPerfil medio de variables clave según motivo de cancelación:\n")
vars_perfil_motivo <- c("watch_hours", "payment_failures", "support_tickets", "satisfaction_score", "monthly_fee")
perfil_motivos <- sapply(vars_perfil_motivo, function(v) tapply(cancelados[[v]], cancelados$churn_reason, mean, na.rm = TRUE))
print(round(perfil_motivos, 2))
cat("\nProporción de plan Basic/Standard/Premium por motivo:\n")
print(round(100 * prop.table(table(cancelados$churn_reason, cancelados$subscription_type), 1), 1))

# Cruce de churn_reason dentro de las hojas del árbol principal (en train)
train_con_hoja <- train_all
train_con_hoja$hoja <- arbol_principal$where
ch_train <- train_con_hoja[train_con_hoja$churned == 1, ]
cat("\nMotivos de baja (%) dentro de las principales hojas de riesgo del árbol (train):\n")
print(round(100 * prop.table(table(ch_train$hoja, ch_train$churn_reason), 1), 1))

# Árbol explicativo complementario: ¿qué características explican cada motivo?
cols_excluir_motivos <- c(
  "customer_id", "signup_date", "churn_date",
  "churned", "churned_f", "tenure_months", "lifetime_revenue"
)
cancelados_modelo <- cancelados[, !(names(cancelados) %in% cols_excluir_motivos)]

arbol_motivos <- rpart(
  churn_reason ~ .,
  data = cancelados_modelo,
  method = "class"
)

cat("\nÁrbol explicativo de motivos de cancelación (n = 4,803 bajas):\n")
print(arbol_motivos)

# Exportar gráfico 05a: Árbol explicativo de motivos
png(
  "graficos/05a_arbol_motivos_cancelacion.png",
  width = 2200, height = 1250, res = 220
)
rpart.plot(
  arbol_motivos,
  type = 2,
  extra = 104,
  fallen.leaves = TRUE,
  box.palette = "auto",
  shadow.col = "gray88",
  roundint = FALSE,
  tweak = 1.0,
  legend.x = NA,
  main = "Arbol explicativo de motivos de cancelacion (n = 4,803 bajas)"
)
dev.off()

# Exportar gráfico 05b: Distribución de los 7 motivos
png(
  "graficos/05b_distribucion_motivos_churn.png",
  width = 1900, height = 1100, res = 220
)
par(mar = c(4.5, 11.5, 3.5, 4.5))
colores_motivos <- ifelse(
  names(tabla_motivos) %in% c("Price too high", "Not enough content", "Switched to competitor", "Billing problems"),
  "#E50914",
  "#94A3B8"
)
bp_m <- barplot(
  tabla_motivos,
  horiz = TRUE,
  las = 1,
  col = colores_motivos,
  border = NA,
  xlim = c(0, 1320),
  xlab = "Numero de cancelaciones",
  main = "Motivos de cancelacion observados (n = 4,803)",
  cex.names = 0.95,
  cex.axis = 0.95,
  cex.lab = 1.0,
  cex.main = 1.1
)
text(
  x = as.numeric(tabla_motivos) + 25,
  y = bp_m,
  labels = paste0(format(as.numeric(tabla_motivos), big.mark = ","), " (", pct_motivos, "%)"),
  adj = 0,
  cex = 0.9,
  font = 2,
  col = "#1E293B"
)
dev.off()


################################################################################
# 8. C.2. IMPUREZA DE GINI (FÓRMULA, CURVA TEÓRICA Y SELECCIÓN DEL NODO RAÍZ)
################################################################################

calcular_gini <- function(y) {
  p <- prop.table(table(y))
  1 - sum(p^2)
}

cat("\n=== 8. C.2. IMPUREZA DE GINI ===\n")
prob_raiz <- prop.table(table(train_modelo$churned_f))
gini_raiz <- calcular_gini(train_modelo$churned_f)

# Primera división óptima del árbol principal: avg_watch_time_per_day < 0.195
rama_izq <- train_modelo[train_modelo$avg_watch_time_per_day >= 0.195, ]
rama_der <- train_modelo[train_modelo$avg_watch_time_per_day < 0.195, ]

gini_izq <- calcular_gini(rama_izq$churned_f)
gini_der <- calcular_gini(rama_der$churned_f)

n_train <- nrow(train_modelo)
gini_ponderado <- (nrow(rama_izq) / n_train) * gini_izq + (nrow(rama_der) / n_train) * gini_der
reduccion_gini <- gini_raiz - gini_ponderado

cat("Distribución nodo raíz (train, n = 8,400):\n")
print(table(train_modelo$churned_f))
print(round(prob_raiz, 4))
cat(sprintf("Gini nodo raíz        = %.4f\n", gini_raiz))
cat(sprintf("Rama Izq (>= 0.195)   : n=%d | p(Cancela)=%.4f | Gini=%.4f\n",
            nrow(rama_izq), mean(rama_izq$churned_f == "Cancela"), gini_izq))
cat(sprintf("Rama Der (< 0.195)    : n=%d | p(Cancela)=%.4f | Gini=%.4f\n",
            nrow(rama_der), mean(rama_der$churned_f == "Cancela"), gini_der))
cat(sprintf("Gini total ponderado  = %.4f (Reducción = %.4f)\n", gini_ponderado, reduccion_gini))

# Comparación de Gini total ponderado en el mejor corte individual por variable
evaluar_corte_gini <- function(var_nombre) {
  sub_df <- train_modelo[, c("churned_f", var_nombre)]
  stump  <- rpart(churned_f ~ ., data = sub_df, method = "class",
                  control = rpart.control(maxdepth = 1, cp = 0))
  nodos  <- stump$where
  g_tot  <- 0
  for (nd in unique(nodos)) {
    yy <- sub_df$churned_f[nodos == nd]
    g_tot <- g_tot + (length(yy) / nrow(sub_df)) * calcular_gini(yy)
  }
  g_tot
}

vars_candidatas <- c(
  "avg_watch_time_per_day", "last_login_days", "watch_hours",
  "sessions_per_month", "support_tickets", "payment_failures",
  "auto_renew", "subscription_type"
)

gini_candidatas <- sapply(vars_candidatas, evaluar_corte_gini)
tabla_gini <- data.frame(
  Variable = names(gini_candidatas),
  Gini_Total = round(as.numeric(gini_candidatas), 4),
  Reduccion = round(gini_raiz - as.numeric(gini_candidatas), 4)
)
tabla_gini <- tabla_gini[order(tabla_gini$Gini_Total), ]
cat("\nComparación de Impureza de Gini Total en el nodo raíz:\n")
print(tabla_gini, row.names = FALSE)

# Exportar gráfico 06: Curva teórica de Gini + Comparativa de variables
png(
  "graficos/06_impureza_gini.png",
  width = 2500, height = 1150, res = 220
)
par(mfrow = c(1, 2), mar = c(5, 4.5, 4, 2))

# Panel A: Curva teórica de Gini (igual que 2. respuesta_gini.R.txt)
p_v <- seq(0, 1, by = 0.01)
p_f <- 1 - p_v
gini_curva <- 1 - p_v^2 - p_f^2

plot(
  p_v, gini_curva,
  type = "l",
  lwd = 3,
  col = "#1E293B",
  ylim = c(0, 0.58),
  xlab = "Proporcion de clientes que cancelan, p(Cancela)",
  ylab = "Impureza de Gini = 1 - p(Cancela)^2 - p(Continua)^2",
  main = "Curva teorica de Gini y particion del nodo raiz",
  cex.lab = 0.95,
  cex.axis = 0.95,
  cex.main = 1.02
)
grid(col = "#E2E8F0", lty = 1)
lines(p_v, gini_curva, lwd = 3, col = "#1E293B")

p_raiz_c <- as.numeric(prob_raiz["Cancela"])
p_izq_c  <- mean(rama_izq$churned_f == "Cancela")
p_der_c  <- mean(rama_der$churned_f == "Cancela")

points(p_raiz_c, gini_raiz, pch = 19, cex = 1.8, col = "#475569")
points(p_izq_c,  gini_izq,  pch = 19, cex = 1.8, col = "#2563EB")
points(p_der_c,  gini_der,  pch = 19, cex = 1.8, col = "#E50914")

text(p_raiz_c, gini_raiz + 0.045, sprintf("Raiz (0.403, %.4f)", gini_raiz), cex = 0.82, font = 2, col = "#334155")
text(p_izq_c + 0.03, gini_izq - 0.085, sprintf("Rama >=0.195\n(0.265, %.4f)", gini_izq), cex = 0.8, font = 2, col = "#2563EB")
text(p_der_c - 0.04, gini_der - 0.085, sprintf("Rama <0.195\n(0.670, %.4f)", gini_der), cex = 0.8, font = 2, col = "#E50914")

# Panel B: Impureza de Gini Total por variable candidata (ordenado, mejor arriba)
par(mar = c(5, 11.8, 4.5, 4.5))
gini_ord <- rev(setNames(tabla_gini$Gini_Total, tabla_gini$Variable))
col_gini <- ifelse(names(gini_ord) == "avg_watch_time_per_day", "#E50914", "#475569")
bp_g <- barplot(
  gini_ord,
  horiz = TRUE,
  las = 1,
  col = col_gini,
  border = NA,
  xlim = c(0, 0.65),
  xlab = "Impureza de Gini Total ponderada (menor = mejor)",
  main = "Seleccion raiz por menor Gini Total",
  cex.names = 0.9,
  cex.axis = 0.95,
  cex.lab = 0.95,
  cex.main = 1.02
)
abline(v = gini_raiz, lty = 2, col = "#94A3B8", lwd = 1.5)
text(x = gini_raiz + 0.015, y = bp_g, labels = sprintf("%.4f", gini_ord), adj = 0, cex = 0.85, font = 2, col = "#1E293B")
text(x = gini_raiz, y = max(bp_g) + 0.85, labels = "Raiz = 0.4812", adj = 0.5, cex = 0.8, col = "#64748B", font = 2, xpd = TRUE)

dev.off()


################################################################################
# 9. C.3. SOBREAJUSTE: ESTIMACIÓN (70%) VS. EVALUACIÓN (30%)
################################################################################

cat("\n=== 9. C.3. SOBREAJUSTE: COMPARACIÓN POR COMPLEJIDAD ===\n")

# 1) Árbol muy simple (maxdepth = 2) -> alto sesgo
arbol_muy_simple <- rpart(
  churned_f ~ .,
  data = train_modelo,
  method = "class",
  control = rpart.control(maxdepth = 2)
)

# 2) Árbol poco profundo (maxdepth = 3)
arbol_poco_profundo <- rpart(
  churned_f ~ .,
  data = train_modelo,
  method = "class",
  control = rpart.control(maxdepth = 3)
)

# 3) Árbol podado automáticamente por rpart (arbol_principal, cp = 0.01)

# 4) Árbol completo sin poda (cp = 0, minsplit = 2, minbucket = 1, maxdepth = 30)
arbol_completo <- rpart(
  churned_f ~ .,
  data = train_modelo,
  method = "class",
  control = rpart.control(
    cp = 0,
    minsplit = 2,
    minbucket = 1,
    maxdepth = 30
  )
)

# Exportar gráfico 08: referencia visual del árbol poco profundo (maxdepth = 3)
png(
  "graficos/08_arbol_poco_profundo_d3.png",
  width = 2000, height = 1150, res = 220
)
rpart.plot(
  arbol_poco_profundo,
  type = 2,
  extra = 104,
  fallen.leaves = TRUE,
  box.palette = list("Blues", "Reds"),
  shadow.col = "gray88",
  roundint = FALSE,
  tweak = 1.08,
  main = "Arbol poco profundo (maxdepth = 3, 6 hojas)"
)
dev.off()

# Evaluación de tasa global de acierto (Accuracy) sin métricas de C.4
modelos_c3 <- list(
  "Muy simple (maxdepth=2)" = arbol_muy_simple,
  "Poco profundo (maxdepth=3)" = arbol_poco_profundo,
  "Podado rpart (cp=0.01)" = arbol_principal,
  "Completo sin poda (cp=0)" = arbol_completo
)

res_c3 <- data.frame(
  Modelo = names(modelos_c3),
  Hojas = integer(length(modelos_c3)),
  Acierto_Estimacion = numeric(length(modelos_c3)),
  Acierto_Evaluacion = numeric(length(modelos_c3)),
  Brecha_pp = numeric(length(modelos_c3)),
  stringsAsFactors = FALSE
)

for (i in seq_along(modelos_c3)) {
  mod <- modelos_c3[[i]]
  p_tr <- predict(mod, train_modelo, type = "class")
  p_te <- predict(mod, test_modelo, type = "class")
  acc_tr <- 100 * mean(p_tr == train_modelo$churned_f)
  acc_te <- 100 * mean(p_te == test_modelo$churned_f)
  res_c3$Hojas[i] <- sum(mod$frame$var == "<leaf>")
  res_c3$Acierto_Estimacion[i] <- round(acc_tr, 2)
  res_c3$Acierto_Evaluacion[i] <- round(acc_te, 2)
  res_c3$Brecha_pp[i] <- round(acc_tr - acc_te, 2)
}

print(res_c3, row.names = FALSE)

# Exportar gráfico 07: Curva de sobreajuste (Estimación vs Evaluación)
png(
  "graficos/07_sobreajuste_estimacion_vs_evaluacion.png",
  width = 2200, height = 1150, res = 220
)
par(mar = c(5.5, 4.8, 4.2, 2))
x_pos <- 1:4
etiquetas_x <- c(
  "Muy simple\n(2 hojas)",
  "Poco profundo\n(6 hojas)",
  "Podado rpart\n(11 hojas)",
  "Completo sin poda\n(905 hojas)"
)

plot(
  x_pos, res_c3$Acierto_Estimacion,
  type = "b",
  pch = 19,
  lwd = 3,
  cex = 1.6,
  col = "#2563EB",
  ylim = c(65, 106),
  xlim = c(0.7, 4.3),
  xaxt = "n",
  xlab = "Complejidad del arbol de decision",
  ylab = "Tasa de acierto global (%)",
  main = "Sobreajuste: Acierto en Estimacion (70%) vs. Evaluacion (30%)",
  cex.lab = 1.05,
  cex.axis = 0.95,
  cex.main = 1.15
)
grid(col = "#E2E8F0", lty = 1)
abline(v = 3, lty = 2, col = "#16A34A", lwd = 2)

lines(x_pos, res_c3$Acierto_Estimacion, type = "b", pch = 19, lwd = 3, cex = 1.6, col = "#2563EB")
lines(x_pos, res_c3$Acierto_Evaluacion, type = "b", pch = 15, lwd = 3, cex = 1.6, col = "#E50914")

axis(1, at = x_pos, labels = etiquetas_x, cex.axis = 0.92, padj = 0.5)

# Etiquetas numéricas claras sin solapamiento
text(x_pos, res_c3$Acierto_Estimacion + 2.3, labels = sprintf("%.2f%%", res_c3$Acierto_Estimacion), col = "#1D4ED8", font = 2, cex = 0.92)
text(x_pos, res_c3$Acierto_Evaluacion - 2.6, labels = sprintf("%.2f%%", res_c3$Acierto_Evaluacion), col = "#B91C1C", font = 2, cex = 0.92)

text(2.92, 68.0, "Equilibrio optimo\n(Brecha: 1.09 pp)", adj = 1, col = "#15803D", font = 2, cex = 0.88)
text(4, 88.8, sprintf("Sobreajuste\n(+%.2f pp)", res_c3$Brecha_pp[4]), col = "#DC2626", font = 2, cex = 0.88)

legend(
  "topleft",
  legend = c("Grupo de estimacion (70%, n = 8,400)", "Grupo de evaluacion (30%, n = 3,600)"),
  col = c("#2563EB", "#E50914"),
  pch = c(19, 15),
  lwd = 3,
  bty = "n",
  cex = 0.95
)

dev.off()

################################################################################
# 10. C.4. MATRIZ DE CONFUSIÓN (ACIERTOS, ERRORES Y MÉTRICAS DE CLASIFICACIÓN)
################################################################################

cat("\n=== 10. C.4. MATRIZ DE CONFUSIÓN (MODELO PRINCIPAL PODADO) ===\n")

# Predicciones de clase sobre evaluación (30%, n = 3,600) y estimación (70%, n = 8,400)
pred_eval  <- predict(arbol_principal, test_modelo, type = "class")
pred_estim <- predict(arbol_principal, train_modelo, type = "class")

# Matriz de confusión (Predicho en filas, Real en columnas; positiva = "Cancela")
matriz_eval <- table(Predicho = pred_eval, Real = test_modelo$churned_f)
matriz_estim <- table(Predicho = pred_estim, Real = train_modelo$churned_f)

if (requireNamespace("caret", quietly = TRUE)) {
  cm_eval <- caret::confusionMatrix(
    data = pred_eval,
    reference = test_modelo$churned_f,
    positive = "Cancela"
  )
  cat("\nResultado completo caret::confusionMatrix (Grupo de Evaluación, 30%):\n")
  print(cm_eval)
} else {
  cat("\nMatriz de confusión (Grupo de Evaluación, 30%, n = 3,600):\n")
  print(matriz_eval)
}

# Extracción de celdas (TN, FN, FP, TP) y cálculo de métricas en Evaluación (30%)
TN <- as.numeric(matriz_eval["Continua", "Continua"])
FN <- as.numeric(matriz_eval["Continua", "Cancela"])
FP <- as.numeric(matriz_eval["Cancela", "Continua"])
TP <- as.numeric(matriz_eval["Cancela", "Cancela"])
N_eval <- sum(matriz_eval)

acc_eval  <- (TP + TN) / N_eval
sens_eval <- TP / (TP + FN)   # Sensitivity / Recall: TP / (TP + FN)
spec_eval <- TN / (TN + FP)   # Specificity: TN / (TN + FP)
prec_eval <- TP / (TP + FP)   # Precision / Pos Pred Value
npv_eval  <- TN / (TN + FN)   # Neg Pred Value

cat("\nDesglose de aciertos y errores (Evaluación, n = 3,600):\n")
cat(sprintf("  TN (Continúan bien clasificados)              : %d (%.1f%%)\n", TN, 100 * TN / N_eval))
cat(sprintf("  TP (Cancelaciones correctamente identificadas): %d (%.1f%%)\n", TP, 100 * TP / N_eval))
cat(sprintf("  FN (Cancelaciones no detectadas)              : %d (%.1f%%)\n", FN, 100 * FN / N_eval))
cat(sprintf("  FP (Falsas alarmas de cancelación)            : %d (%.1f%%)\n", FP, 100 * FP / N_eval))

cat("\nMétricas de evaluación (Evaluación, 30%):\n")
cat(sprintf("  Accuracy (Acierto global)      : %.4f (%.2f%%)\n", acc_eval, 100 * acc_eval))
cat(sprintf("  Sensitivity (Recall)           : %.4f (%.2f%%)\n", sens_eval, 100 * sens_eval))
cat(sprintf("  Specificity                    : %.4f (%.2f%%)\n", spec_eval, 100 * spec_eval))
cat(sprintf("  Precision (Pos Pred Value)     : %.4f (%.2f%%)\n", prec_eval, 100 * prec_eval))
cat(sprintf("  Neg Pred Value                 : %.4f (%.2f%%)\n", npv_eval, 100 * npv_eval))

# Comparativa breve en Estimación (70%)
TN_tr <- as.numeric(matriz_estim["Continua", "Continua"])
FN_tr <- as.numeric(matriz_estim["Continua", "Cancela"])
FP_tr <- as.numeric(matriz_estim["Cancela", "Continua"])
TP_tr <- as.numeric(matriz_estim["Cancela", "Cancela"])
cat(sprintf(
  "\nReferencia en Estimación (70%%, n = %d): TN=%d | FN=%d | FP=%d | TP=%d | Acc=%.2f%% | Sens=%.2f%% | Spec=%.2f%%\n",
  sum(matriz_estim), TN_tr, FN_tr, FP_tr, TP_tr,
  100 * (TP_tr + TN_tr) / sum(matriz_estim),
  100 * TP_tr / (TP_tr + FN_tr),
  100 * TN_tr / (TN_tr + FP_tr)
))

# Exportar gráfico 09: Matriz de confusión y métricas de clasificación (Evaluación 30%)
png(
  "graficos/09_matriz_confusion.png",
  width = 2500, height = 1200, res = 220
)
layout(matrix(c(1, 2), nrow = 1), widths = c(1.15, 1))

# Panel A: Matriz de confusión 2x2 (Evaluación, n = 3,600)
par(mar = c(3.8, 5.2, 4.8, 2.0))
plot(
  0, 0, type = "n",
  xlim = c(0, 2), ylim = c(0, 2),
  xaxs = "i", yaxs = "i",
  xaxt = "n", yaxt = "n",
  xlab = "", ylab = "",
  bty = "n"
)

# Celdas: filas = Predicho (arriba Continua [y=1..2], abajo Cancela [y=0..1])
#         cols  = Real (izq Continua [x=0..1], der Cancela [x=1..2])
rect(0, 1, 1, 2, col = "#DBEAFE", border = "#FFFFFF", lwd = 4) # TN
rect(1, 1, 2, 2, col = "#FEF3C7", border = "#FFFFFF", lwd = 4) # FN
rect(0, 0, 1, 1, col = "#FFEDD5", border = "#FFFFFF", lwd = 4) # FP
rect(1, 0, 2, 1, col = "#DCFCE7", border = "#FFFFFF", lwd = 4) # TP

dibujar_celda <- function(cx, cy, titulo, valor, n_tot, subtitulo, col_txt) {
  text(cx, cy + 0.28, titulo, font = 2, cex = 0.92, col = col_txt)
  text(cx, cy + 0.02, format(valor, big.mark = ","), font = 2, cex = 1.75, col = "#0F172A")
  text(cx, cy - 0.18, sprintf("(%.1f%% del total)", 100 * valor / n_tot), cex = 0.85, col = "#334155")
  text(cx, cy - 0.34, subtitulo, cex = 0.78, font = 3, col = "#475569")
}

dibujar_celda(0.5, 1.5, "TN (Acierto)", TN, N_eval, "Continuan bien clasificados", "#1D4ED8")
dibujar_celda(1.5, 1.5, "FN (Error)",   FN, N_eval, "Cancelaciones no detectadas", "#B45309")
dibujar_celda(0.5, 0.5, "FP (Error)",   FP, N_eval, "Falsas alarmas de cancelacion", "#C2410C")
dibujar_celda(1.5, 0.5, "TP (Acierto)", TP, N_eval, "Cancelaciones identificadas", "#15803D")

axis(3, at = c(0.5, 1.5), labels = c("Real: Continua", "Real: Cancela"), tick = FALSE, cex.axis = 1.0, font = 2, line = -0.3)
axis(2, at = c(1.5, 0.5), labels = c("Predicho:\nContinua", "Predicho:\nCancela"), tick = FALSE, las = 1, cex.axis = 0.98, font = 2, line = -0.2)
mtext("Matriz de Confusion (Evaluacion 30%, n = 3,600)", side = 3, line = 2.6, cex = 1.12, font = 2, col = "#0F172A")
mtext("Clase positiva: Cancela | Modelo podado (11 hojas)", side = 1, line = 1.2, cex = 0.88, col = "#475569")

# Panel B: Barras horizontales de métricas de evaluación
par(mar = c(4.8, 8.8, 4.8, 3.2))
metricas_vals <- c(
  "Neg Pred Value" = 100 * npv_eval,
  "Precision (PPV)" = 100 * prec_eval,
  "Specificity"     = 100 * spec_eval,
  "Sensitivity"     = 100 * sens_eval,
  "Accuracy"        = 100 * acc_eval
)
col_met <- c("#64748B", "#475569", "#2563EB", "#E50914", "#16A34A")

bp_m <- barplot(
  metricas_vals,
  horiz = TRUE,
  las = 1,
  col = col_met,
  border = NA,
  xlim = c(0, 108),
  xlab = "Porcentaje (%)",
  main = "Metricas de Clasificacion (Evaluacion)",
  cex.names = 0.95,
  cex.axis = 0.95,
  cex.lab = 1.0,
  cex.main = 1.12
)
abline(v = c(70, 80, 90), lty = 3, col = "#CBD5E1")
text(
  x = metricas_vals + 1.8,
  y = bp_m,
  labels = sprintf("%.2f%%", metricas_vals),
  adj = 0,
  font = 2,
  cex = 0.95,
  col = "#0F172A"
)

dev.off()

cat("\n=== EJECUCIÓN FINALIZADA SIN ERRORES ===\n")
cat("Gráficos exportados en: graficos/\n")
print(list.files("graficos"))
