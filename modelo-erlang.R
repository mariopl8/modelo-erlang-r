

# Instalación de Paqueterías (si no están instaladas)
if (!require("datasets")) install.packages("datasets", dependencies = TRUE)
if (!require("ggplot2")) install.packages("ggplot2", dependencies = TRUE)
if (!require("GGally")) install.packages("GGally", dependencies = TRUE)
if (!require("fitdistrplus")) install.packages("fitdistrplus", dependencies = TRUE)

# Cargar las librerías necesarias
library(datasets)      # Para acceder al dataset 'chickwts'
library(ggplot2)       # Para crear los gráficos
library(GGally)        # Para usar ggpairs()
library(fitdistrplus)  # Para ajustar distribuciones

# Punto 1: Cargar el dataset 'chickwts'
data("chickwts")
str(chickwts)  # Explorar la estructura del dataset

# Punto 2: Análisis Exploratorio de Datos (EDA)
# Gráfico de pares para explorar relaciones entre variables
ggpairs(chickwts, columns = c("weight", "feed"), 
        title = "Análisis Exploratorio de Datos (EDA)")

# Boxplot del peso por tipo de alimentación
ggplot(chickwts, aes(x = feed, y = weight, fill = feed)) +
  geom_boxplot() +
  labs(title = "Distribución del Peso por Tipo de Alimentación", 
       x = "Tipo de Alimentación", y = "Peso (gramos)") +
  theme_minimal()

# Punto 3: Ajuste a una distribución de Erlang (Gamma con shape entero)
erlang_fit <- fitdist(chickwts$weight, "gamma", method = "mle")
summary(erlang_fit)
plot(erlang_fit)

# Gráfico Q-Q para comparar con la distribución de Erlang
qq_data <- data.frame(
  Empirical = sort(chickwts$weight),
  Theoretical = qgamma(ppoints(length(chickwts$weight)), shape = erlang_fit$estimate["shape"], rate = erlang_fit$estimate["rate"])
)

ggplot(qq_data, aes(x = Theoretical, y = Empirical)) +
  geom_point(color = "blue", size = 2) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Gráfico Q-Q para el Peso", 
       x = "Cuantiles Teóricos (Erlang)", 
       y = "Cuantiles Muestrales") +
  theme_minimal()

# Punto 4: Calcular probabilidades
# Calcular la probabilidad de que un proceso tarde más de X minutos
X <- 300  # Umbral de peso (interpretado como tiempo de espera)
probabilidad <- 1 - pgamma(X, shape = erlang_fit$estimate["shape"], rate = erlang_fit$estimate["rate"])
cat("La probabilidad de que un proceso tarde más de", X, "gramos (minutos) es:", probabilidad, "\n")

# Punto 5: Conclusión y reflexión
cat("\nConclusión:\n")
cat("El modelo de Erlang se ajusta a los datos con los siguientes parámetros:\n")
cat(" - Shape (forma):", erlang_fit$estimate["shape"], "\n")
cat(" - Rate (tasa):", erlang_fit$estimate["rate"], "\n")
cat("La probabilidad calculada sugiere que", round(probabilidad * 100, 2), "% de los procesos tardan más de", X, "gramos (minutos).\n")
