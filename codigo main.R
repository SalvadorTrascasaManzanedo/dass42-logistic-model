# ============================================================
# 0. PAQUETE
# ============================================================
install.packages("haven")   # solo si no lo tienes instalado
install.packages("psych")   # solo si no lo tienes instalado
library(psych)

# ============================================================
# 1. IMPORTAR DATOS
# ============================================================

datos <- read.delim("data.csv", header = TRUE, sep = "\t")

# ============================================================
# 2. DEFINIR ÍTEMS DASS
# ============================================================

depresion_items <- c("Q3A","Q5A","Q10A","Q13A","Q16A","Q17A","Q21A",
                     "Q24A","Q26A","Q31A","Q34A","Q37A","Q38A","Q42A")

ansiedad_items <- c("Q2A","Q4A","Q7A","Q9A","Q15A","Q19A","Q20A",
                    "Q23A","Q25A","Q28A","Q30A","Q36A","Q40A","Q41A")

estres_items <- c("Q1A","Q6A","Q8A","Q11A","Q12A","Q14A","Q18A",
                  "Q22A","Q27A","Q29A","Q32A","Q33A","Q35A","Q39A")

items_dass <- c(depresion_items, ansiedad_items, estres_items)

# ============================================================
# 3. RECODIFICAR DASS DE 1-4 A 0-3
# ============================================================

datos[items_dass] <- datos[items_dass] - 1

# Comprobar rangos
sapply(datos[items_dass], range, na.rm = TRUE)

# ============================================================
# 4. CREAR PUNTUACIONES DASS
# ============================================================

datos$depresion <- rowSums(datos[depresion_items], na.rm = TRUE)
datos$ansiedad  <- rowSums(datos[ansiedad_items], na.rm = TRUE)
datos$estres    <- rowSums(datos[estres_items], na.rm = TRUE)



# ============================================================
# 6. RECODIFICAR CATEGÓRICAS
# ============================================================

datos$gender <- factor(datos$gender,
                       levels = c(1, 2, 3),
                       labels = c("Hombre", "Mujer", "Otro"))

datos$education <- factor(datos$education,
                          levels = c(1, 2, 3, 4),
                          labels = c("Menos_secundaria", "Secundaria",
                                     "Universidad", "Posgrado"))

datos$urban <- factor(datos$urban,
                      levels = c(1, 2, 3),
                      labels = c("Rural", "Suburbana", "Urbana"))

datos$uniquenetworklocation <- factor(datos$uniquenetworklocation,
                                      levels = c(1, 2),
                                      labels = c("Red_unica", "Red_multiple"))

# ============================================================
# 7. CALIDAD DE DATOS
# ============================================================

# Edad
summary(datos$age)
hist(datos$age, breaks = 80,
     main = "Distribución de edad",
     xlab = "Edad")

# Tiempo total del test
summary(datos$testelapse)
hist(datos$testelapse, breaks = 80,
     main = "Distribución de testelapse",
     xlab = "Tiempo total DASS")

# VCL falsos
datos$vcl_falsos <- datos$VCL6 + datos$VCL9 + datos$VCL12

table(datos$vcl_falsos)
round(prop.table(table(datos$vcl_falsos)) * 100, 2)

# Red única / múltiple
table(datos$uniquenetworklocation)
round(prop.table(table(datos$uniquenetworklocation)) * 100, 2)

# ============================================================
# 8. DEPURACIÓN
# ============================================================

p99_testelapse <- quantile(datos$testelapse, .99, na.rm = TRUE)

datos_limpios <- subset(datos,
                        age >= 13 &
                          age <= 100 &
                          vcl_falsos <= 1 &
                          testelapse >= 84 &
                          testelapse <= p99_testelapse)

dim(datos)
dim(datos_limpios)

# Casos eliminados
nrow(datos) - nrow(datos_limpios)


# ============================================================
# 10. DESCRIPTIVOS CONTINUOS
# ============================================================

vars_cont <- datos_limpios[, c("depresion", "ansiedad", "estres", "age")]

psych::describe(vars_cont)


# Histogramas
par(mfrow = c(2, 2))

hist(datos_limpios$depresion, breaks = 40,
     main = "Distribución de depresión",
     xlab = "Depresión",
     col = "orange")

hist(datos_limpios$ansiedad, breaks = 40,
     main = "Distribución de ansiedad",
     xlab = "Ansiedad",
     col = "orange")

hist(datos_limpios$estres, breaks = 40,
     main = "Distribución de estrés",
     xlab = "Estrés",
     col = "orange")

hist(datos_limpios$age, breaks = 40,
     main = "Distribución de edad",
     xlab = "DASS total",
     col = "orange")

par(mfrow = c(1, 1))


# ============================================================
# DELIMITACIÓN DE LA MUESTRA POR EDAD
# Adultos jóvenes: 18–30 años
# ============================================================

base_modelo_joven <- subset(
  base_modelo,
  age >= 18 & age <= 30
)

# Comprobar tamaño antes/después
dim(base_modelo)
dim(base_modelo_joven)

# Casos eliminados
nrow(base_modelo) - nrow(base_modelo_joven)

# Porcentaje eliminado
round((nrow(base_modelo) - nrow(base_modelo_joven)) / nrow(base_modelo) * 100, 2)


# Descriptivos de edad tras delimitación
summary(base_modelo_joven$age)
psych::describe(base_modelo_joven$age)


par(mfrow = c(2, 2))

hist(base_modelo_joven$depresion, breaks = 40,
     main = "Distribución de depresión",
     xlab = "Depresión",
     col = "orange")

hist(base_modelo_joven$ansiedad, breaks = 40,
     main = "Distribución de ansiedad",
     xlab = "Ansiedad",
     col = "orange")

hist(base_modelo_joven$estres, breaks = 40,
     main = "Distribución de estrés",
     xlab = "Estrés",
     col = "orange")

hist(base_modelo_joven$age, breaks = 15,
     main = "Distribución de edad",
     xlab = "Edad",
     col = "orange")

par(mfrow = c(1, 1))

#Estadisticos descriptivos de las cuantitativas tras reducir muestra de edad:

vars_cont_jovenes <- base_modelo_joven[, c("depresion", "ansiedad", "estres", "age")]
psych::describe(vars_cont_jovenes)

# Una vez comprobada la adecuedad, centrar en media:
# Centrar variables en la media:
base_modelo$depresion_c <- base_modelo$depresion - mean(base_modelo$depresion, na.rm = TRUE)
base_modelo$ansiedad_c  <- base_modelo$ansiedad  - mean(base_modelo$ansiedad, na.rm = TRUE)
base_modelo$estres_c    <- base_modelo$estres    - mean(base_modelo$estres, na.rm = TRUE)
base_modelo$age_c       <- base_modelo$age       - mean(base_modelo$age, na.rm = TRUE)

head(base_modelo)
# ============================================================
# VARIABLE CRITERIO: DEPRESIÓN SEVERA O EXTREMADAMENTE SEVERA
# Punto de corte DASS-42: depresión >= 21
# ============================================================

base_modelo_joven$depresion_severa_num <- as.integer(
  base_modelo_joven$depresion >= 21
)

base_modelo_joven$depresion_severa <- factor(
  base_modelo_joven$depresion_severa_num,
  levels = c(0, 1),
  labels = c("No severa", "Severa")
)

table(base_modelo_joven$depresion_severa)
round(prop.table(table(base_modelo_joven$depresion_severa)) * 100, 2)


# Graficar distribución de depresión y de categorización severa/no severa
par(mfrow = c(1, 2))

# depresión
hist(base_modelo_joven$depresion, breaks = 40,
     main = "Distribución de puntuaciones depresión",
     xlab = "Frecuencia",
     col = "orange")

# severa/no severa
barplot(table(base_modelo_joven$depresion_severa),
        main = "Distribución de clases",
        xlab = "Depresión",
        ylab = "Frecuencia",
        col = "orange",
        border = "gray30")

par(mfrow = c(1, 1))

# Frecuencias y porcentajes, prueba de ajuste
tabla_dep <- table(base_modelo_joven$depresion_severa)
tabla_dep

round(prop.table(tabla_dep) * 100, 2)



chisq.test(tabla_dep, p = c(0.50, 0.50))

# Tamaño del efecto para bondad de ajuste
# w de Cohen
chi_balance <- chisq.test(tabla_dep, p = c(0.50, 0.50))

w_cohen <- sqrt(as.numeric(chi_balance$statistic) / sum(tabla_dep))
w_cohen

# ============================================================
# 10. DESCRIPTIVOS CATEGÓRICOS
# ============================================================
# N y % de categoricas
tabla_cat <- function(x) {
  tabla <- table(x)
  porcentaje <- round(prop.table(tabla) * 100, 2)
  cbind(Frecuencia = tabla, Porcentaje = porcentaje)
}

tabla_cat(base_modelo_joven$gender)
tabla_cat(base_modelo_joven$education)
tabla_cat(base_modelo_joven$urban)

# Barplots de categoricas
par(mfrow = c(1, 3))

barplot(table(base_modelo_joven$gender),
        main = "Distribución de género",
        ylab = "Frecuencia",
        col = "orange")

barplot(table(base_modelo_joven$education),
        main = "Distribución del nivel educativo",
        ylab = "Frecuencia",
        las = 2,
        col = "orange")

barplot(table(base_modelo_joven$urban),
        main = "Distribución de residencia",
        ylab = "Frecuencia",
        col = "orange")

par(mfrow = c(1, 1))

# Recodificar educación en dos grupos
base_modelo_joven$education_rec <- as.character(base_modelo_joven$education)

base_modelo_joven$education_rec[
  base_modelo_joven$education %in% c("Menos_secundaria", "Secundaria")
] <- "No_universitaria"

base_modelo_joven$education_rec[
  base_modelo_joven$education %in% c("Universidad", "Posgrado")
] <- "Universitaria"

base_modelo_joven$education_rec <- factor(
  base_modelo_joven$education_rec,
  levels = c("No_universitaria", "Universitaria")
)

# Comprobar frecuencias
table(base_modelo_joven$education_rec)
round(prop.table(table(base_modelo_joven$education_rec)) * 100, 2)

barplot(table(base_modelo_joven$education_rec),
        main = "Distribución del nivel educativo recodificado",
        ylab = "Frecuencia",
        col = "orange",
        border = "gray30",
        las = 1)

# Base final para modelo, sin género
head(base_modelo_joven)

base <- base_modelo_joven[, c(
  "depresion_severa_num",
  "depresion_severa",
  "ansiedad",
  "estres",
  "depresion_c",
  "ansiedad_c",
  "estres_c",
  "age",
  "age_c",
  "education_rec",
  "urban"
)]
base <- na.omit(base)
# Comprobar
dim(base)
summary(base)
head(base)

# Guardar en SPSS
library(haven)
write_sav(base, "base_spss.sav")

# ============================================================
#  TABLAS CRUZADAS 
# ============================================================
# Depresión severa x nivel educativo
table(base_modelo_joven$depresion_severa, base_modelo_joven$education_rec)
round(prop.table(table(base_modelo_joven$depresion_severa,
                       base_modelo_joven$education_rec), margin = 2) * 100, 2)

# Depresión severa x residencia
table(base_modelo_joven$depresion_severa, base_modelo_joven$urban)
round(prop.table(table(base_modelo_joven$depresion_severa,
                       base_modelo_joven$urban), margin = 2) * 100, 2)

# ============================================================
#  CHI-CUADRADO y TAMAÑO DEL EFECTO
# ============================================================
# Función para V de Cramer
cramer_v <- function(tabla) {
  chi2 <- chisq.test(tabla)$statistic
  n <- sum(tabla)
  k <- min(nrow(tabla), ncol(tabla))
  v <- sqrt(chi2 / (n * (k - 1)))
  return(as.numeric(v))
}

# Depresión severa x nivel educativo

tab_edu_dep <- table(base_modelo_joven$depresion_severa,
                     base_modelo_joven$education_rec)

tab_edu_dep
round(prop.table(tab_edu_dep, margin = 2) * 100, 2)

chi_edu <- chisq.test(tab_edu_dep)
chi_edu

cramer_v(tab_edu_dep)

# Depresión severa x residencia

tab_urban_dep <- table(base_modelo_joven$depresion_severa,
                       base_modelo_joven$urban)

tab_urban_dep
round(prop.table(tab_urban_dep, margin = 2) * 100, 2)

chi_urban <- chisq.test(tab_urban_dep)
chi_urban

cramer_v(tab_urban_dep)


################################################################################
##################### ANALISIS INFERENCIAL #####################################
################################################################################

# ============================================================
# 1. MODELO NULO
# ============================================================

modelo_nulo_dep <- glm(
  depresion_severa_num ~ 1,
  data = base,
  family = binomial
)

summary(modelo_nulo_dep)

# Probabilidad base
prop.table(table(base$depresion_severa_num))

# Odds base
odds_base_dep <- mean(base$depresion_severa_num) /
  (1 - mean(base$depresion_severa_num))

odds_base_dep

# Logit base
log(odds_base_dep)

# Desvianza nula
modelo_nulo_dep$deviance

# ============================================================
# 2. MODELO SINTOMÁTICO BASE
# Ansiedad + Estrés
# ============================================================

modelo_sintomas_dep <- glm(
  depresion_severa_num ~ ansiedad_c + estres_c,
  data = base,
  family = binomial
)

summary(modelo_sintomas_dep)

# Comparación con modelo nulo
anova(modelo_nulo_dep, modelo_sintomas_dep, test = "Chisq")

# Odds ratios
exp(coef(modelo_sintomas_dep))

# Intervalos de confianza de los OR
exp(confint(modelo_sintomas_dep))

# ============================================================
# 3. MODELO CON INTERACCIÓN ANSIEDAD × ESTRÉS
# ============================================================

modelo_interaccion_dep <- glm(
  depresion_severa_num ~ ansiedad_c * estres_c,
  data = base,
  family = binomial
)

summary(modelo_interaccion_dep)

# Comparación con el modelo sintomático base
anova(modelo_sintomas_dep, modelo_interaccion_dep, test = "Chisq")

# Odds ratios
exp(coef(modelo_interaccion_dep))

# IC 95% de los OR
exp(confint(modelo_interaccion_dep))

# ============================================================
# 4. MODELO AMPLIADO
# ============================================================

modelo_ampliado_dep <- glm(
  depresion_severa_num ~ ansiedad_c + estres_c +
  age_c + education_rec + urban,
  data = base,
  family = binomial
)

summary(modelo_ampliado_dep)

# Comparación con modelo sintomático base
anova(modelo_sintomas_dep, modelo_ampliado_dep, test = "Chisq")

# Odds ratios
exp(coef(modelo_ampliado_dep))

# Intervalos de confianza
exp(confint(modelo_ampliado_dep))


# ============================================================
# 4. MODELO AMPLIADO + INTERACCIÓN EDAD X RESIDENCIA
# ============================================================

modelo_edad_residencia <- glm(
  depresion_severa_num ~ ansiedad_c + estres_c +
    age_c * urban + education_rec,
  data = base,
  family = binomial
)

summary(modelo_edad_residencia)

anova(modelo_ampliado_dep, modelo_edad_residencia, test = "Chisq")

exp(coef(modelo_edad_residencia))
exp(confint(modelo_edad_residencia))

# ============================================================
# 4. MODELO AMPLIADO + INTERACCIÓN ESTRÉS X EDCACIÓN
# ============================================================
modelo_estres_educacion <- glm(
  depresion_severa_num ~ ansiedad_c + estres_c * education_rec +
    age_c + urban,
  data = base,
  family = binomial
)

summary(modelo_estres_educacion)
anova(modelo_ampliado_dep, modelo_estres_educacion, test = "Chisq")

# ============================================================
# 4. MODELO AMPLIADO + INTERACCIÓN ANSIEDAD X EDCACIÓN
# ============================================================
modelo_ansiedad_educacion <- glm(
  depresion_severa_num ~ ansiedad_c * education_rec + estres_c +
    age_c + urban,
  data = base,
  family = binomial
)

summary(modelo_ansiedad_educacion)
anova(modelo_ampliado_dep, modelo_ansiedad_educacion, test = "Chisq")

# ============================================================
# 4. MODELO AMPLIADO + INTERACCIÓN ANSIEDAD X EDCACIÓN SIN RESIDENCIA
# ============================================================
modelo_sin_residencia <- glm(
  depresion_severa_num ~ ansiedad_c + estres_c * education_rec + age_c,
  data = base,
  family = binomial
)
summary(modelo_sin_residencia)
anova(modelo_sin_residencia, modelo_estres_educacion, test = "Chisq")

AIC(modelo_sin_residencia, modelo_estres_educacion)
# ============================================================
# TABLA ÚNICA COMPARATIVA DE MODELOS
# ============================================================

tabla_completa_modelos <- data.frame(
  Modelo = c(
    "Nulo",
    "Sintomático",
    "Ansiedad x Estrés",
    "Ampliado",
    "Edad x Residencia",
    "Ansiedad x Educación",
    "Estrés x Educación"
  ),
  
  Predictores = c(
    "Intercepto",
    "Ansiedad + estrés",
    "Ansiedad + estrés + ansiedad x estrés",
    "Ansiedad + estrés + edad + educación + residencia",
    "Ampliado + edad x residencia",
    "Ampliado + ansiedad x educación",
    "Ampliado + estrés x educación"
  ),
  
  Comparacion = c(
    "-",
    "vs. nulo",
    "vs. sintomático",
    "vs. sintomático",
    "vs. ampliado",
    "vs. ampliado",
    "vs. ampliado"
  ),
  
  Desvianza = round(c(
    deviance(modelo_nulo_dep),
    deviance(modelo_sintomas_dep),
    deviance(modelo_interaccion_dep),
    deviance(modelo_ampliado_dep),
    deviance(modelo_edad_residencia),
    deviance(modelo_ansiedad_educacion),
    deviance(modelo_estres_educacion)
  ), 2),
  
  AIC = round(c(
    AIC(modelo_nulo_dep),
    AIC(modelo_sintomas_dep),
    AIC(modelo_interaccion_dep),
    AIC(modelo_ampliado_dep),
    AIC(modelo_edad_residencia),
    AIC(modelo_ansiedad_educacion),
    AIC(modelo_estres_educacion)
  ), 2),
  
  Delta_chi2 = c(
    NA,
    anova(modelo_nulo_dep, modelo_sintomas_dep, test = "Chisq")$Deviance[2],
    anova(modelo_sintomas_dep, modelo_interaccion_dep, test = "Chisq")$Deviance[2],
    anova(modelo_sintomas_dep, modelo_ampliado_dep, test = "Chisq")$Deviance[2],
    anova(modelo_ampliado_dep, modelo_edad_residencia, test = "Chisq")$Deviance[2],
    anova(modelo_ampliado_dep, modelo_ansiedad_educacion, test = "Chisq")$Deviance[2],
    anova(modelo_ampliado_dep, modelo_estres_educacion, test = "Chisq")$Deviance[2]
  ),
  
  gl = c(
    NA,
    anova(modelo_nulo_dep, modelo_sintomas_dep, test = "Chisq")$Df[2],
    anova(modelo_sintomas_dep, modelo_interaccion_dep, test = "Chisq")$Df[2],
    anova(modelo_sintomas_dep, modelo_ampliado_dep, test = "Chisq")$Df[2],
    anova(modelo_ampliado_dep, modelo_edad_residencia, test = "Chisq")$Df[2],
    anova(modelo_ampliado_dep, modelo_ansiedad_educacion, test = "Chisq")$Df[2],
    anova(modelo_ampliado_dep, modelo_estres_educacion, test = "Chisq")$Df[2]
  ),
  
  p = c(
    NA,
    anova(modelo_nulo_dep, modelo_sintomas_dep, test = "Chisq")$`Pr(>Chi)`[2],
    anova(modelo_sintomas_dep, modelo_interaccion_dep, test = "Chisq")$`Pr(>Chi)`[2],
    anova(modelo_sintomas_dep, modelo_ampliado_dep, test = "Chisq")$`Pr(>Chi)`[2],
    anova(modelo_ampliado_dep, modelo_edad_residencia, test = "Chisq")$`Pr(>Chi)`[2],
    anova(modelo_ampliado_dep, modelo_ansiedad_educacion, test = "Chisq")$`Pr(>Chi)`[2],
    anova(modelo_ampliado_dep, modelo_estres_educacion, test = "Chisq")$`Pr(>Chi)`[2]
  )
)

# Formato
tabla_completa_modelos$Delta_chi2 <- round(tabla_completa_modelos$Delta_chi2, 2)

tabla_completa_modelos$p <- ifelse(
  is.na(tabla_completa_modelos$p),
  "-",
  ifelse(tabla_completa_modelos$p < .001, "< .001",
         as.character(round(tabla_completa_modelos$p, 3)))
)

tabla_completa_modelos$gl <- ifelse(
  is.na(tabla_completa_modelos$gl),
  "-",
  tabla_completa_modelos$gl
)

tabla_completa_modelos

# ============================================================
# TABLA DE COEFICIENTES DEL MODELO FINAL
# B, EE, Wald z, p, OR e IC 95%
# ============================================================

# Modelo final
modelo_final <- modelo_sin_residencia

# Coeficientes del summary
coef_sum <- summary(modelo_final)$coefficients

# IC 95% de OR
IC_OR <- exp(confint(modelo_final))

# Tabla final
tabla_coeficientes <- data.frame(
  Predictor = rownames(coef_sum),
  B = round(coef_sum[, "Estimate"], 3),
  EE = round(coef_sum[, "Std. Error"], 3),
  Wald_z = round(coef_sum[, "z value"], 3),
  p_value = coef_sum[, "Pr(>|z|)"],
  OR = round(exp(coef(modelo_final)), 3),
  IC95_inf = round(IC_OR[, 1], 3),
  IC95_sup = round(IC_OR[, 2], 3)
)

# Formato APA para p
tabla_coeficientes$p_value <- ifelse(
  tabla_coeficientes$p_value < .001,
  "< .001",
  round(tabla_coeficientes$p_value, 3)
)

tabla_coeficientes
# Porcentajes automáticos

OR_porcentaje <- exp(coef(modelo_sin_residencia))

porcentaje <- ifelse(
  OR_porcentaje > 1,
  (OR_porcentaje - 1) * 100,
  (1 - OR_porcentaje) * 100
)

round(porcentaje, 1)

########
# R2
######
install.packages("pscl")
library(pscl)
pR2(modelo_sin_residencia)

######
# La interacción EDUCACIÓN X ESTRES
######

library(ggplot2)

# Crear valores
estres_seq <- seq(min(base$estres_c), max(base$estres_c), length.out = 100)

datos_plot <- expand.grid(
  estres_c = estres_seq,
  ansiedad_c = 0,
  age_c = 0,
  education_rec = c("No_universitaria", "Universitaria")
)

datos_plot$prob <- predict(modelo_sin_residencia, newdata = datos_plot, type = "response")

ggplot(datos_plot, aes(x = estres_c, y = prob, color = education_rec)) +
  geom_line(size = 1.2) +
  labs(
    title = "Probabilidad de depresión severa según estrés y nivel educativo",
    x = "Estrés (centrado en la media)",
    y = "Probabilidad",
    color = "Educación"
  ) +
  theme_minimal()



# ============================================================
# EVALUACIÓN DEL MODELO FINAL
# Modelo final: modelo_sin resistencia y con interaccion estres x educación
# ============================================================

##############
# COLINEALIDAD
##############
library(car)
vif(modelo_sin_residencia)

##############
# LINEALIDAD
##############
# ============================================================
# SUPUESTO DE LINEALIDAD DEL LOGIT
# ============================================================

# Crear grupos por cuartiles
base$ansiedad_grupo <- cut(base$ansiedad_c,
                           breaks = quantile(base$ansiedad_c, probs = seq(0, 1, .25), na.rm = TRUE),
                           include.lowest = TRUE)

base$estres_grupo <- cut(base$estres_c,
                         breaks = quantile(base$estres_c, probs = seq(0, 1, .25), na.rm = TRUE),
                         include.lowest = TRUE)

base$age_grupo <- cut(base$age_c,
                      breaks = quantile(base$age_c, probs = seq(0, 1, .25), na.rm = TRUE),
                      include.lowest = TRUE)

# Modelo final
modelo_lineal <- modelo_sin_residencia

# Modelos con términos categorizados añadidos
modelo_ansiedad_cat <- glm(
  depresion_severa_num ~ ansiedad_c + estres_c * education_rec + age_c + ansiedad_grupo,
  data = base,
  family = binomial
)

modelo_estres_cat <- glm(
  depresion_severa_num ~ ansiedad_c + estres_c * education_rec + age_c + estres_grupo,
  data = base,
  family = binomial
)

modelo_age_cat <- glm(
  depresion_severa_num ~ ansiedad_c + estres_c * education_rec + age_c + age_grupo,
  data = base,
  family = binomial
)

anova(modelo_lineal, modelo_ansiedad_cat, test = "Chisq")
anova(modelo_lineal, modelo_estres_cat, test = "Chisq")
anova(modelo_lineal, modelo_age_cat, test = "Chisq")

# ============================================================
# INDEPENDENCIA DE ERRORES
# ============================================================

library(lmtest)

dwtest(modelo_sin_residencia)

res_std <- rstandard(modelo_sin_residencia)

plot(res_std,
     main = "Dispersión de residuos estandarizados",
     xlab = "Índice de observación",
     ylab = "Residuo estandarizado",
     pch = 20,
     col = "gray40")

abline(h = 0, col = "orange", lwd = 2)
abline(h = c(-3, 3), col = "red", lty = 2)

# ============================================================
# DISPERSIÓN / PROPORCIONALIDAD A LA MEDIA
# ============================================================

parametro_escala <- deviance(modelo_sin_residencia) /
  df.residual(modelo_sin_residencia)

parametro_escala

# ============================================================
# ANÁLISIS DE VALORES ATÍPICOS E INFLUYENTES
# Modelo final: modelo_sin_residencia
# ============================================================

# Probabilidades predichas
prob <- fitted(modelo_sin_residencia)

# Valores observados
y <- base$depresion_severa_num

# ============================================================
#  RESIDUOS ESTANDARIZADOS: valores atípicos
# ============================================================

res_est <- (y - prob) / sqrt(prob * (1 - prob))

summary(res_est)

# Casos atípicos: residuos > 3 o < -3
casos_atipicos <- which(abs(res_est) > 3)

length(casos_atipicos)
round(length(casos_atipicos) / nrow(base) * 100, 2)

# Ver casos atípicos
base[casos_atipicos, ]


# ============================================================
# 2. RESIDUOS STUDENTIZADOS: valores influyentes
# ============================================================


# Leverage
h <- hatvalues(modelo_sin_residencia)

# Residuo studentizado según fórmula de las diapositivas
res_stud <- (y - prob) / sqrt(prob * (1 - prob) * (1 - h))

summary(res_stud)

# Casos influyentes: residuos studentizados > 3 o < -3
casos_influyentes <- which(abs(res_stud) > 3)

length(casos_influyentes)
round(length(casos_influyentes) / nrow(base) * 100, 2)

# Ver casos influyentes
base[casos_influyentes, ]


# Gráfico

par(mfrow = c(1, 2))

# Casos atípicos
plot(res_est,
     main = "Casos Atípicos",
     xlab = "Índice de observación",
     ylab = "Residuo estandarizado",
     pch = 20,
     col = "gray40")

abline(h = 0, col = "orange", lwd = 2)
abline(h = c(-3, 3), col = "red", lty = 2)

# Casos influyentes
plot(res_stud,
     main = "Casos Influyentes",
     xlab = "Índice de observación",
     ylab = "Residuo studentizado",
     pch = 20,
     col = "gray40")

abline(h = 0, col = "orange", lwd = 2)
abline(h = c(-3, 3), col = "red", lty = 2)

par(mfrow = c(1, 1))

#  RESUMEN COMPACTO

diagnostico_residuos <- data.frame(
  Indicador = c("Casos atípicos", "Casos influyentes"),
  Criterio = c("|Residuo estandarizado| > 3",
               "|Residuo studentizado| > 3"),
  n = c(length(casos_atipicos),
        length(casos_influyentes)),
  Porcentaje = round(c(length(casos_atipicos),
                       length(casos_influyentes)) / nrow(base) * 100, 2)
)

diagnostico_residuos




# ============================================================
# PRECISIÓN DEL MODELO FINAL
# ============================================================

# Probabilidades predichas
base$prob_pred <- predict(modelo_sin_residencia, type = "response")

# Clasificación con punto de corte .50
base$pred_clase <- ifelse(base$prob_pred >= 0.50, 1, 0)

# Matriz de confusión
matriz_confusion <- table(
  Observado = base$depresion_severa_num,
  Predicho = base$pred_clase
)

matriz_confusion

# Métricas
VP <- matriz_confusion["1", "1"]
VN <- matriz_confusion["0", "0"]
FP <- matriz_confusion["0", "1"]
FN <- matriz_confusion["1", "0"]

accuracy <- (VP + VN) / sum(matriz_confusion)
sensibilidad <- VP / (VP + FN)
especificidad <- VN / (VN + FP)
VPP <- VP / (VP + FP)
VPN <- VN / (VN + FN)

metricas_precision <- data.frame(
  Indicador = c("Accuracy", "Sensibilidad", "Especificidad",
                "Valor predictivo positivo", "Valor predictivo negativo"),
  Valor = round(c(accuracy, sensibilidad, especificidad, VPP, VPN), 3)
)

metricas_precision

# ============================================================
# CURVA ROC Y AUC
# ============================================================

library(pROC)

roc_final <- roc(base$depresion_severa_num, base$prob_pred)

auc(roc_final)

plot(roc_final,
     main = "Curva ROC del modelo",
     col = "orange",
     lwd = 3)

abline(a = 0, b = 1, lty = 2, col = "gray40")