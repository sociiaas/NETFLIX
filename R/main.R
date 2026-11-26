rm(list = ls())
if (!dir.exists("datos") && dir.exists("../datos")) {
  setwd("..")
}
netflix <- read.csv("datos/netflix_churn_enhanced.csv")
View(netflix)
attach(netflix)


## chumed=1 -> suscripcion cancelada

library(rpart)
library(rpart.plot)
library(caret)


str(netflix)
# Eliminamos las columnas identificadoras, fechas y variables posteriores a la baja
netflix_datos <- netflix[, !(names(netflix) %in% c("customer_id", "signup_date", "churn_date", "churn_reason"))]
#Convertimos la variable objetivo 'churned' a factor con etiquetas claras
netflix_datos$churned <- factor(netflix$churned,
                                levels = c(0, 1),
                                labels = c("Continua", "Cancela"))
levels(netflix_datos$churned)
table(netflix$churned)

# 3. División en grupo de estimación (70%) y evaluación (30%)
set.seed(123)
train_index <- sample(1:nrow(netflix_datos), size = 0.7 * nrow(netflix_datos))
train <- netflix_datos[train_index, ]
test  <- netflix_datos[-train_index, ]

# Ajustamos el modelo de clasificación usando todas las variables explicativas (.)
arbol_netflix_complejo <- rpart(churned ~ ., data = train, method = "class")

# Mostrar las reglas numéricas del árbol por consola
arbol_netflix_complejo

# 4. Visualización gráfica del árbol
rpart.plot(arbol_netflix_complejo)


# Creamos un árbol más sencillo
arbol_netflix_sencillo <- rpart(
  churned ~ .,
  data = train,
  method = "class",
  control = rpart.control(maxdepth = 3)
)

# Mostrar las reglas numéricas del árbol por consola
arbol_netflix_sencillo

# 4. Visualización gráfica del árbol
rpart.plot(arbol_netflix_sencillo)



prob_raiz <- prop.table(table(train$churned))
prob_raiz

# Aplicamos la fórmula de clase: Gini = 1 - p(cancelacion)^2 - p(continuidad)^2
gini_raiz <- 1 - sum(prob_raiz^2)
gini_raiz

# 2. Impureza de Gini tras la primera división (Nodo raíz: tenure_months < 19.5)
# Rama izquierda: clientes con antigüedad menor a 19.5 meses
rama_izq <- train[train$tenure_months < 19.5, ]
prob_izq <- prop.table(table(rama_izq$churned))
gini_izq <- 1 - sum(prob_izq^2)

# Rama derecha: clientes con antigüedad mayor o igual a 19.5 meses
rama_der <- train[train$tenure_months >= 19.5, ]
prob_der <- prop.table(table(rama_der$churned))
gini_der <- 1 - sum(prob_der^2)

# 3. Impureza de Gini total ponderada tras el primer corte (como en las diapositivas)
n_total <- nrow(train)
gini_ponderado <- (nrow(rama_izq) / n_total) * gini_izq + (nrow(rama_der) / n_total) * gini_der

gini_izq
gini_der
gini_ponderado
