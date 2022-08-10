library(readr) # install.package
library(dplyr)

library(caret) # Para matriz de confusion

library(rpart) # Para arbol
library(rpart.plot) # Para arbol
library(knitr)  # Para tablas amigables con los datos

datos <- read.csv("https://github.com
                  /cristianrodriguez05/ProyectoTelePrediccionFelicidad/blob
                  /main/Aplicativo/datosAplicativo/datos.csv"
                  , encoding = "UTF-8", stringsAsFactors = TRUE)

kable(datos, caption = "Los datos")

summary(datos)

str(datos)

modelo <- rpart(FELICIDAD ~ ., data = datos)

sm <- summary(modelo)

sm

prp(modelo, main="Arbol de regresión",
    nn = TRUE, # display the node numbers
    fallen.leaves = TRUE,  # put the leaves on the bottom of the page
    shadow.col = "gray",   # shadows under the leaves
    branch.lty = 3,        # draw branches using dotted lines
    branch = .5,           # change angle of branch lines
    faclen = 0,            # faclen = 0 to print full factor names
    trace = 1,             # print the auto calculated cex, xlim, ylim
    split.cex = 1.2,       # make the split text larger than the node text
    split.prefix = "is ",  # put "is " before split text
    split.suffix = "?",    # put "?" after split text
    split.box.col = "lightblue",   # lightgray split boxes (default is white)
    split.border.col = "darkgray", # darkgray border on split boxes
    split.round = 0.5)             # round the split box corners a tad

prediccion <- predict(object = modelo, datos, type = "class")
prediccion

datos.R.P <- data.frame(realidad = datos$FELICIDAD, prediccion)

kable(datos.R.P, caption = "Reales / Predicciones")

datos.R.P$realidad <- as.factor(datos.R.P$realidad)

datos.R.P$prediccion <- as.factor(datos.R.P$prediccion)

matriz <- confusionMatrix(datos.R.P$realidad, datos.R.P$prediccion)

matriz

ESTU_GENERO <- c('M')
ESTU_VALORMATRICULAUNIVERSIDAD <- c('Menos de 500 mil')
ESTU_PAGOMATRICULACREDITO <- c('No')
ESTU_SEMESTRECURSA <- c('10')
FAMI_ESTRATOVIVIENDA <- c('Estrato 6')
FAMI_TIENEINTERNET <- c('Si')
FAMI_TIENECOMPUTADOR <- c('Si')
FAMI_TIENESERVICIOTV <- c('Si')
FAMI_TIENEAUTOMOVIL <- c('Si')
FAMI_TIENEMOTOCICLETA = c('Si')
FAMI_NUMPERSONASACARGO = c('0')
ESTU_HORASSEMANATRABAJA = c('0')
PUNT_GLOBAL = c(150)
prediccion = c('?')



datos.nuevos <- data.frame(ESTU_GENERO, ESTU_VALORMATRICULAUNIVERSIDAD, ESTU_PAGOMATRICULACREDITO, ESTU_SEMESTRECURSA, FAMI_ESTRATOVIVIENDA, FAMI_TIENEINTERNET, FAMI_TIENECOMPUTADOR, FAMI_TIENESERVICIOTV, FAMI_TIENEAUTOMOVIL, FAMI_TIENEMOTOCICLETA, FAMI_NUMPERSONASACARGO, ESTU_HORASSEMANATRABAJA, PUNT_GLOBAL)

kable(datos.nuevos, captio = "Datos nuevos")

prediccion.nueva <- predict(object = modelo, newdata = datos.nuevos, type = 'class')
prediccion.nueva

