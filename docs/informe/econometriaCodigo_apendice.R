# ---------------------------------------------------------------------------
# SECCION 0: Carga y filtrado de los datos
# ---------------------------------------------------------------------------

# Carpeta donde vive este script. Si se ejecuta desde docs/informe, el script
# sube a la raiz del proyecto, donde estan el CSV principal y los graficos.
script_path <- tryCatch(
  rstudioapi::getActiveDocumentContext()$path,
  error = function(e) {
    file_arg <- grep("^--file=", commandArgs(), value = TRUE)
    if (length(file_arg)) sub("^--file=", "", file_arg[1]) else ""
  }
)

script_dir <- if (!is.null(script_path) && nzchar(script_path)) dirname(script_path) else getwd()
project_dir <- script_dir

if (!file.exists(file.path(project_dir, "idealista_es_sale_homes_madrid_pages84_2025-07-15.csv"))) {
  project_dir <- normalizePath(file.path(script_dir, "..", ".."), winslash = "/", mustWork = FALSE)
}

setwd(project_dir)

raw <- read.csv("idealista_es_sale_homes_madrid_pages84_2025-07-15.csv")

# Municipio asignado al grupo
chosen_muni <- "Alcobendas"

# Conservar solo los anuncios de venta en Alcobendas
dat <- subset(raw, municipality == chosen_muni & operation == "sale")

# Seleccionar las columnas utilizadas en el analisis
selected_cols <- c("price", "propertyType", "size", "rooms", "bathrooms", "status")
dat <- dat[, selected_cols]

# Eliminar filas con valores perdidos
dat <- na.omit(dat)

# Crear las variables logaritmicas del modelo
dat$log_price <- log(dat$price)
dat$log_size  <- log(dat$size)

# Convertir las variables categoricas a factores
dat$propertyType <- factor(dat$propertyType)
dat$status       <- factor(dat$status)

cat("Numero total de anuncios en Alcobendas:", nrow(dat), "\n")


# ---------------------------------------------------------------------------
# SECCION 1: Descripcion de los datos y analisis preliminar
# ---------------------------------------------------------------------------

cat("\nAnuncios por tipo de propiedad:\n")
print(table(dat$propertyType))

cat("\nAnuncios por estado del inmueble:\n")
print(table(dat$status))

cat("\nEstadisticos descriptivos (price, size, rooms, bathrooms):\n")
print(summary(dat[, c("price", "size", "rooms", "bathrooms")]))

cat("\nDesviaciones tipicas:\n")
cat("  price:    ", round(sd(dat$price), 2), "\n")
cat("  size:     ", round(sd(dat$size), 2), "\n")
cat("  rooms:    ", round(sd(dat$rooms), 2), "\n")
cat("  bathrooms:", round(sd(dat$bathrooms), 2), "\n")


# ---------------------------------------------------------------------------
# SECCION 1.3: Graficos descriptivos
# ---------------------------------------------------------------------------

png(file.path(project_dir, "hist_precios.png"), width = 1200, height = 550, res = 130)
par(mfrow = c(1, 2), mar = c(5, 5, 4, 2))

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

colores_box <- c("#4C87C4", "#E07B39", "#3A9E5F", "#9B6BB5", "#D95F5F")
png(file.path(project_dir, "boxplot_tipo.png"), width = 1000, height = 580, res = 130)
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
axis(2, at = axTicks(2),
     labels = format(axTicks(2), big.mark = ".", scientific = FALSE),
     las = 1, cex.axis = 0.8)
mtext("Precio (EUR)", side = 2, line = 4.5, cex = 0.95)
dev.off()


# ---------------------------------------------------------------------------
# SECCION 2: Regresiones simples
# ---------------------------------------------------------------------------

m1 <- lm(price ~ size, data = dat)
m2 <- lm(log_price ~ log_size, data = dat)
m3 <- lm(log_price ~ rooms, data = dat)
m4 <- lm(log_price ~ bathrooms, data = dat)
m5 <- lm(price ~ propertyType, data = dat)
m6 <- lm(price ~ status, data = dat)

cat("\n--- Modelo 1: price ~ size ---\n");          print(summary(m1))
cat("\n--- Modelo 2: log_price ~ log_size ---\n"); print(summary(m2))
cat("\n--- Modelo 3: log_price ~ rooms ---\n");    print(summary(m3))
cat("\n--- Modelo 4: log_price ~ bathrooms ---\n"); print(summary(m4))
cat("\n--- Modelo 5: price ~ propertyType ---\n"); print(summary(m5))
cat("\n--- Modelo 6: price ~ status ---\n");       print(summary(m6))


# ---------------------------------------------------------------------------
# SECCION 3: Modelo multivariante
# ---------------------------------------------------------------------------

full <- lm(log_price ~ log_size + rooms + bathrooms + propertyType + status,
           data = dat)

cat("\n--- Modelo completo (multivariante) ---\n")
print(summary(full))

reduced <- lm(log_price ~ log_size + rooms + bathrooms + status, data = dat)

cat("\n--- F-test: significacion conjunta de propertyType ---\n")
print(anova(reduced, full))


# ---------------------------------------------------------------------------
# SECCION 4: Forma funcional y extensiones
# ---------------------------------------------------------------------------

nl <- lm(log_price ~ log_size + I(log_size^2) + rooms + bathrooms +
           propertyType + status,
         data = dat)

cat("\n--- Modelo con termino cuadratico en log-tamano ---\n")
print(summary(nl))

b1 <- coef(nl)["log_size"]
b2 <- coef(nl)["I(log_size^2)"]
pval_b2 <- summary(nl)$coefficients["I(log_size^2)", "Pr(>|t|)"]

if (pval_b2 < 0.05) {
  log_size_star <- -b1 / (2 * b2)
  size_star     <- exp(log_size_star)
  cat("\nEl termino cuadratico ES significativo (p =", round(pval_b2, 4), ")\n")
  cat("Punto de giro estimado:", round(size_star, 1), "m2\n")
  cat("(A partir de ese tamano el precio por m2 adicional empieza a disminuir)\n")
} else {
  cat("\nEl termino cuadratico NO es significativo (p =", round(pval_b2, 4), ")\n")
  cat("No hay evidencia de no linealidad relevante en el tamano.\n")
}


# ---------------------------------------------------------------------------
# SECCION 5: Diagnostico visual de residuos del modelo principal
# ---------------------------------------------------------------------------

png(file.path(project_dir, "residuos_modelo_full.png"), width = 900, height = 450)
par(mfrow = c(1, 2))

plot(fitted(full), residuals(full),
     xlab = "Valores ajustados",
     ylab = "Residuos",
     main = "Residuos vs. Ajustados",
     col  = "steelblue",
     pch  = 16,
     cex  = 0.7)
abline(h = 0, col = "red", lty = 2)

qqnorm(residuals(full), main = "QQ-Plot de residuos", col = "steelblue", pch = 16, cex = 0.7)
qqline(residuals(full), col = "red", lty = 2)

par(mfrow = c(1, 1))
dev.off()
