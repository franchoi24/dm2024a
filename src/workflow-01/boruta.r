options(repos = c("https://cloud.r-project.org/"))
install.packages("Boruta")

require("data.table")
require("yaml")
require("Rcpp")
require("Boruta")

dataset <- fread("~/buckets/b1/flow/wf_sept-046/006-FErf_attributes_base/dataset.csv.gz")

campitos <- c("numero_de_cliente", "foto_mes","clase_ternaria")

dataset[, clase01 := 0L ]
dataset[ clase_ternaria %in% c( "BAJA+2", "BAJA+1"), 
         clase01 := 1L ]

p_value = 0.01
max_runs = 20

set.seed(127567, kind = "L'Ecuyer-CMRG")

campos_buenos <- setdiff(
  colnames(dataset),
  c( campitos, "clase01")
)

azar <- runif(nrow(dataset))

dataset[, entrenamiento :=
          as.integer( foto_mes %in%  c( 202101, 202102, 202103) &
                        (clase01 == 1 | azar < 0.01))]

impute_median <- function(x) {
  x[is.na(x)] <- median(x, na.rm = TRUE)
  return(x)
}

data_imputed_median <- data.frame(lapply(dataset[entrenamiento == TRUE, campos_buenos, with = FALSE], impute_median))
data_imputed_median$clase01 <- dataset[entrenamiento == TRUE, clase01]

boruta <- Boruta(clase01~.,
                 data = data_imputed_median,
                 maxRuns = max_runs,
                 pValue = p_value)

pf_recent_boruta_df <- attStats(boruta)
str(pf_recent_boruta_df)
boruta_rejected <- subset(pf_recent_boruta_df, subset = pf_recent_boruta_df$decision == "Rejected")
rej <- t(boruta_rejected)
rej_names <- c(colnames(rej), "clase01")
dataset[, (rej_names) := NULL]
fwrite(dataset,
       file = paste0("~/buckets/b1/datasets/boruta_", p_value, "_", max_runs, ".csv.gv"),
       sep = "v")