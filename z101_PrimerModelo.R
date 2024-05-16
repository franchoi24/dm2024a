# Arbol elemental con libreria  rpart
# Debe tener instaladas las librerias  data.table  ,  rpart  y  rpart.plot

# cargo las librerias que necesito
require("data.table")
require("rpart")
require("rpart.plot")

# Aqui se debe poner la carpeta de la materia de SU computadora local
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # Establezco el Working Directory

# cargo el dataset
dataset <- fread("./datasets/ganancia_mia.csv")
dataset <- dataset[,-1]

#dtrain <- dataset[foto_mes == 202107] # defino donde voy a entrenar
#dapply <- dataset[foto_mes == 202109] # defino donde voy a aplicar el modelo

# genero el modelo,  aqui se construye el arbol
# quiero predecir clase_ternaria a partir de el resto de las variables
modelo <- rpart(
        formula = "ganancia_promedio ~ .",
        data = dataset[,ganancia_promedio := ganancia_promedio/1000000], # los datos donde voy a entrenar
        xval = 0,
        cp = -0.3, # esto significa no limitar la complejidad de los splits
        minsplit = 200, # minima cantidad de registros para que se haga el split
        minbucket = 50, # tamaño minimo de una hoja
        maxdepth = 5
) # profundidad maxima del arbol

pdf("output_mio.pdf")
# grafico el arbol
img <- prp(modelo,
        extra = 101, digits = -5,
        branch = 1, type = 4, varlen = 0, faclen = 0
)

dev.off()



# aplico el modelo a los datos nuevos
#prediccion <- predict(
#        object = modelo,
#        newdata = dapply,
#        type = "prob"
#)

# prediccion es una matriz con TRES columnas,
# llamadas "BAJA+1", "BAJA+2"  y "CONTINUA"
# cada columna es el vector de probabilidades

# agrego a dapply una columna nueva que es la probabilidad de BAJA+2
#dapply[, prob_baja2 := prediccion[, "BAJA+2"]]

# solo le envio estimulo a los registros
#  con probabilidad de BAJA+2 mayor  a  1/40
#dapply[, Predicted := as.numeric(prob_baja2 > 1 / 40)]

# genero el archivo para Kaggle
# primero creo la carpeta donde va el experimento
#dir.create("./exp/")
#dir.create("./exp/KA2")

# solo los campos para Kaggle
#fwrite(dapply[, list(numero_de_cliente, Predicted)],
#        file = "./exp/KA2/K101_001.csv",
#        sep = ","
#)
