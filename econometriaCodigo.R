# 0) Leer el fichero de datos
raw <- read.csv("idealista_es_sale_homes_madrid_pages84_2025-07-15.csv")
chosen_muni <- "Alcobendas"
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

# Tama~no y precio
m1 <- lm(price ~ size, data=dat) # precio sobre tama~no
m2 <- lm(log_price ~ log_size, data=dat) # elasticidad del precio respecto al tama~no
# Habitaciones y ba~nos
m3 <- lm(log_price ~ rooms, data=dat) # efecto de las habitaciones
m4 <- lm(log_price ~ bathrooms, data=dat) # efecto de los ba~nos

# Tipo de propiedad y estado
m5 <- lm(price ~ propertyType, data=dat)
m6 <- lm(price ~ status, data=dat)
summary(m1); summary(m2); summary(m3)
summary(m4); summary(m5); summary(m6)

# 3) regresion multivariante
full <- lm(log_price ~ log_size + rooms + bathrooms + propertyType + status, data=dat)
summary(full)


reduced <- lm(log_price ~ log_size + rooms + bathrooms + status, data=dat)
anova(reduced, full) # F-test para propertyType



# 4) Forma funcional y extensiones
nl <- lm(log_price ~ log_size + I(log_size^2) + rooms + bathrooms +
           propertyType + status,
         data=dat)
summary(nl)