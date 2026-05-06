
# ---------------------------------------------------------------------------
# SECCION 0: Carga y filtrado de los datos
# --------------------------------------------------------------------------

# Carpeta donde vive este script; todos los archivos (CSV y PNG) se buscan
# y guardan aqui. Funciona tanto con Source como con Rscript.
script_dir <- tryCatch(
  dirname(rstudioapi::getActiveDocumentContext()$path),
  error = function(e) dirname(sys.frames()[[1]]$ofile)
)
if (is.null(script_dir) || script_dir == "") script_dir <- getwd()
setwd(script_dir)

raw <- read.csv("idealista_es_sale_homes_madrid_pages84_2025-07-15.csv")

# Municipio asignado a nuestro grupo
chosen_muni <- "Alcobendas"

# Nos quedamos solo con los anuncios de venta en Alcobendas
dat <- subset(raw, municipality == chosen_muni & operation == "sale")

# Seleccionamos las columnas que vamos a usar
selected_cols <- c("price", "propertyType", "size", "rooms", "bathrooms", "status")
dat <- dat[, selected_cols]

# Eliminamos filas con valores perdidos
dat <- na.omit(dat)

# Creamos las variables logaritmicas que usaremos despues
dat$log_price <- log(dat$price)
dat$log_size  <- log(dat$size)

# Convertimos las variables categoricas a factores
dat$propertyType <- factor(dat$propertyType)
dat$status       <- factor(dat$status)

# Vemos cuantos anuncios tenemos en total
cat("Numero total de anuncios en Alcobendas:", nrow(dat), "\n")


# ---------------------------------------------------------------------------
# SECCION 1: Descripcion de los datos y analisis preliminar
# ---------------------------------------------------------------------------

# --- 1.1 Distribucion por tipo de propiedad y estado ---

cat("\nAnuncios por tipo de propiedad:\n")
print(table(dat$propertyType))

cat("\nAnuncios por estado del inmueble:\n")
print(table(dat$status))


# --- 1.2 Estadisticos descriptivos ---

cat("\nEstadisticos descriptivos (price, size, rooms, bathrooms):\n")
print(summary(dat[, c("price", "size", "rooms", "bathrooms")]))

# La desviacion tipica no sale en summary(), la calculamos aparte
cat("\nDesviaciones tipicas:\n")
cat("  price:    ", round(sd(dat$price), 2), "\n")
cat("  size:     ", round(sd(dat$size), 2), "\n")
cat("  rooms:    ", round(sd(dat$rooms), 2), "\n")
cat("  bathrooms:", round(sd(dat$bathrooms), 2), "\n")


# --- 1.3 Graficos descriptivos ---

# Abrimos un PNG para guardar los histogramas de precio
png(file.path(script_dir, "hist_precios.png"), width = 1200, height = 550, res = 130)
par(mfrow = c(1, 2), mar = c(5, 5, 4, 2))

# Numero de breaks adaptado al tamano de la muestra (regla de Sturges)
n_breaks <- max(8, round(1 + log2(nrow(dat))))

hist(dat$price,
     main   = "Distribucion del precio",
     xlab   = "Precio (EUR)",
     ylab   = "Frecuencia",
     col    = "#4C87C4",
     border = "white",
     breaks = n_breaks,
     xaxt   = "n",
     cex.main = 1.1, cex.lab = 0.95)
# Eje X con formato de miles (sin notacion cientifica)
axis(1, at = axTicks(1),
     labels = format(axTicks(1), big.mark = ".", scientific = FALSE))

hist(dat$log_price,
     main   = "Distribucion del log-precio",
     xlab   = "log(Precio)",
     ylab   = "Frecuencia",
     col    = "#E07B39",
     border = "white",
     breaks = n_breaks,
     cex.main = 1.1, cex.lab = 0.95)

par(mfrow = c(1, 1))
dev.off()

# Diagrama de cajas: precio segun tipo de propiedad
colores_box <- c("#4C87C4", "#E07B39", "#3A9E5F", "#9B6BB5", "#D95F5F")
png(file.path(script_dir, "boxplot_tipo.png"), width = 1000, height = 580, res = 130)
par(mar = c(5, 6, 4, 2))
boxplot(price ~ propertyType,
        data     = dat,
        main     = "Precio por tipo de propiedad (Alcobendas)",
        xlab     = "Tipo de propiedad",
        ylab     = "",
        col      = colores_box[seq_len(nlevels(dat$propertyType))],
        border   = "#333333",
        whisklty = 1,
        las      = 1,
        yaxt     = "n",
        cex.main = 1.1, cex.lab = 0.95, cex.axis = 0.85)
# Eje Y con formato de miles
axis(2, at = axTicks(2),
     labels = format(axTicks(2), big.mark = ".", scientific = FALSE),
     las = 1, cex.axis = 0.8)
mtext("Precio (EUR)", side = 2, line = 4.5, cex = 0.95)
dev.off()

# Nota: el histograma de precios tiene mucha asimetria a la derecha (cola larga
# por los pisos mas caros). Al tomar logaritmos la distribucion se vuelve mucho
# mas simetrica y se parece mas a una normal, lo que es mas apropiado para la
# regresion lineal.


# -------------------------------------------------------------------------
# SECCION 2: Regresiones simples
# ---------------------------------------------------------------------------

# Aqui estimamos seis modelos univariantes para ver el efecto de cada variable
# por separado, antes de meterlas todas juntas.

# Precio en niveles sobre tamano
m1 <- lm(price ~ size, data = dat)

# Log-precio sobre log-tamano: esto nos da directamente la elasticidad
# (la interpretacion es: si el tamano sube 1%, el precio sube beta1 %)
m2 <- lm(log_price ~ log_size, data = dat)

# Efectos de habitaciones y banos sobre el log-precio
m3 <- lm(log_price ~ rooms,     data = dat)
m4 <- lm(log_price ~ bathrooms, data = dat)

# Efectos categoricos: tipo de propiedad y estado
m5 <- lm(price ~ propertyType, data = dat)
m6 <- lm(price ~ status,       data = dat)

cat("\n--- Modelo 1: price ~ size ---\n");         print(summary(m1))
cat("\n--- Modelo 2: log_price ~ log_size ---\n"); print(summary(m2))
cat("\n--- Modelo 3: log_price ~ rooms ---\n");    print(summary(m3))
cat("\n--- Modelo 4: log_price ~ bathrooms ---\n");print(summary(m4))
cat("\n--- Modelo 5: price ~ propertyType ---\n"); print(summary(m5))
cat("\n--- Modelo 6: price ~ status ---\n");       print(summary(m6))


# ---------------------------------------------------------------------------
# SECCION 3: Modelo multivariante
# ---------------------------------------------------------------------------

# Este es el modelo principal del trabajo. Incluimos todo a la vez para
# controlar por las demas variables y evitar sesgo por variable omitida.
# La variable dependiente es el log del precio.

full <- lm(log_price ~ log_size + rooms + bathrooms + propertyType + status,
           data = dat)

cat("\n--- Modelo completo (multivariante) ---\n")
print(summary(full))

# --- F-test: contraste de significacion conjunta de propertyType ---
# Estimamos el modelo restringido (sin propertyType) y comparamos con el full

reduced <- lm(log_price ~ log_size + rooms + bathrooms + status, data = dat)

cat("\n--- F-test: significacion conjunta de propertyType ---\n")
print(anova(reduced, full))

# Si el p-valor del F-test es pequeno (< 0.05), el tipo de propiedad aporta
# informacion significativa y no deberiamos quitarlo del modelo.


# ---------------------------------------------------------------------------
# SECCION 4: Forma funcional y extensiones
# ---------------------------------------------------------------------------

# Aqui probamos si la relacion entre tamano y precio es no lineal.
# Anadimos el cuadrado del log-tamano al modelo completo.
# Si beta2 es negativo y significativo, habria rendimientos decrecientes al
# tamano (cada m2 adicional vale menos cuanto mas grande es ya el piso).

nl <- lm(log_price ~ log_size + I(log_size^2) + rooms + bathrooms +
           propertyType + status,
         data = dat)

cat("\n--- Modelo con termino cuadratico en log-tamano ---\n")
print(summary(nl))

# --- Calculo del punto de giro ---
# En el modelo: log_price = b0 + b1*log_size + b2*log_size^2 + ...
# La derivada respecto a log_size es: b1 + 2*b2*log_size = 0
# Despejando: log_size* = -b1 / (2*b2)
# En metros cuadrados: size* = exp(log_size*)

b1 <- coef(nl)["log_size"]
b2 <- coef(nl)["I(log_size^2)"]

# Solo tiene sentido calcularlo si el termino cuadratico es significativo
pval_b2 <- summary(nl)$coefficients["I(log_size^2)", "Pr(>|t|)"]

if (pval_b2 < 0.05) {
  log_size_star <- -b1 / (2 * b2)
  size_star     <- exp(log_size_star)
  cat("\nEl termino cuadratico ES significativo (p =", round(pval_b2, 4), ")\n")
  cat("Punto de giro estimado: ", round(size_star, 1), "m2\n")
  cat("(A partir de ese tamano el precio por m2 adicional empieza a disminuir)\n")
} else {
  cat("\nEl termino cuadratico NO es significativo (p =", round(pval_b2, 4), ")\n")
  cat("No hay evidencia de no linealidad relevante en el tamano.\n")
}


# -------------------------------------------------------------------------------
# EXTRA: Grafico de residuos del modelo completo (util para el informe Sr. Jaime)
# ------------------------------------------------------------------------------

png(file.path(script_dir, "residuos_modelo_full.png"), width = 900, height = 450)
par(mfrow = c(1, 2))

# Residuos vs valores ajustados (detectar heterocedasticidad o no linealidad)
plot(fitted(full), residuals(full),
     xlab = "Valores ajustados",
     ylab = "Residuos",
     main = "Residuos vs. Ajustados",
     col  = "steelblue",
     pch  = 16,
     cex  = 0.7)
abline(h = 0, col = "red", lty = 2)

# QQ-plot para ver si los residuos son aproximadamente normales
qqnorm(residuals(full), main = "QQ-Plot de residuos", col = "steelblue", pch = 16, cex = 0.7)
qqline(residuals(full), col = "red", lty = 2)

par(mfrow = c(1, 1))
dev.off()

