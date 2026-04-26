# 0) Leer el fichero de datos
raw <- read.csv("idealista_es_sale_homes_madrid_pages84_2025-07-15.csv")
# Sustituir "Madrid" por la cadena EXACTA del municipio asignado a vuestro grupo
chosen_muni <- "Madrid"
# Conservar solo vuestro municipio y las ventas
dat <- subset(raw, municipality == chosen_muni & operation == "sale")
# Conservar ´unicamente las columnas seleccionadas (no renombrar)
selected_cols <- c("price","propertyType","size","rooms",
                   "bathrooms","status")
dat <- dat[, selected_cols]
# Limpieza b´asica
dat <- na.omit(dat)
# Crear variables
dat$log_price <- log(dat$price)
dat$log_size <- log(dat$size)
# Crear factores
dat$propertyType <- factor(dat$propertyType)
dat$status <- factor(dat$status)


# 1) Descripcion de los datos y analisis preliminar

table(dat$propertyType); table(dat$status)

summary(dat[, c("price","size","rooms","bathrooms")])

hist(dat$price, main="Precios de los anuncios", xlab="EUR")
hist(dat$log_price, main="Logaritmo de precios", xlab="log(EUR)")
boxplot(price ~ propertyType, data=dat, main="Precio por tipo de propiedad", las=2)

# 2) regresiones simples

