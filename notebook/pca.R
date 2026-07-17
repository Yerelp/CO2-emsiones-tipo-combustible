# Instalar y/o cargar todos los paquetes de una vez
if (!require("pacman")) {
  install.packages("pacman")
}

pacman::p_load(
  factoextra,
  readr,
  FactoMineR,
  GGally,
  ggplot2,
  missMDA,
  corrplot,
  NbClust,
  cluster,
  fpc,
  dendextend,
  readxl,
  visdat,
  Hmisc,      # Load before tidyverse
  psych,      # Load before tidyverse
  tidyverse   # Loads dplyr last, ensuring its functions take precedence
)

df <- read_csv(
  "C:\\Users\\user\\CO2-emsiones-tipo-combustible\\data\\processed\\data_analysis_final.csv"
)

str(df)

correlaciones <- cor(df[, -c(1, 2)])
cor_res <- rcorr(as.matrix(df[, -c(1, 2)]))  
cor_mat <- cor_res$r
p_mat <- cor_res$P

ggpairs(df[, -c(1, 2)], title = "Matriz de dispersión de las variables", upper = list(continuous = wrap("cor", size = 5)))

corrplot.mixed(
  correlaciones,
  lower = "circle",
  tl.cex = 1.2,
  upper = "number",
  tl.pos = "lt",
  tl.col = "black"
)

library(psych)
KMO(df[, -c(1, 2)])

cor_for_bartlett <- cor(df[, -c(1, 2)])
bartlett_res <- cortest.bartlett(cor_for_bartlett, n = nrow(df))
bartlett_res

# Eliminar variables de baja MSA (MSA < 0,5)
kmo_result <- KMO(df[, -c(1, 2)])
msa_values <- kmo_result$MSAi
low_msa_vars <- names(msa_values[msa_values < 0.5])
cat("Eliminar variables de baja MSA:", paste(low_msa_vars, collapse = ", "), "\n")

cols_to_keep <- !colnames(df) %in% low_msa_vars
data_filtered <- df[, cols_to_keep]


correlaciones_filtered <- cor(data_filtered[, -c(1, 2)])
cor_res_filtered <- rcorr(as.matrix(data_filtered[, -c(1, 2)]))  
cor_mat_filtered <- cor_res_filtered$r
p_mat_filtered <- cor_res_filtered$P


det_cor_filtered <- det(cor_mat_filtered)
eig_vals_filtered <- eigen(cor_mat_filtered)$values
var_explained_filtered <- eig_vals_filtered / sum(eig_vals_filtered)
cum_var_filtered <- cumsum(var_explained_filtered)
n_eig_gt1_filtered <- sum(eig_vals_filtered > 1)

#kmo y bartlet a data filtrada

kmo_filtered <- KMO(data_filtered[, -c(1, 2)])
bartlett_filtered <- cortest.bartlett(
  cor(data_filtered[, -c(1, 2)]),
  n = nrow(data_filtered)
)

cat("\n=== Filtered Data Diagnostics ===\n")
cat("KMO Overall MSA:", kmo_filtered$MSA, "\n")

cat("Bartlett p-value:", bartlett_filtered$p.value, "\n")

cat("Determinant:", det_cor_filtered, "\n")

cat("Eigenvalues > 1:", n_eig_gt1_filtered, "\n")

cat(
  "Cumulative variance (first 4 PCs):",
  cum_var_filtered[1:min(4, length(cum_var_filtered))],
  "\n"
)


pca <- prcomp(data_filtered[, -c(1, 2)], scale. = TRUE)
summary(pca)

fviz_eig(
  pca,
  addlabels = TRUE,   
  labelsize = 6       
) +
  theme_classic(base_size = 16) +   
  theme(
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 16),
    plot.title = element_text(size = 18)
  )


#-
eig <- get_eigenvalue(pca)

ggplot(eig, aes(x = seq_along(eigenvalue), y = eigenvalue)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = round(eigenvalue, 2)),
            vjust = -0.4, size = 5) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  labs(x = "Componente principal",
       y = "Eigenvalue") +
  theme_classic(base_size = 16)


#-
data_filtered_df <- as.data.frame(data_filtered)

years <- data_filtered_df$Año
rownames(data_filtered_df) <- years  


#-
fviz_pca_biplot(
  pca,
  geom.ind = "point", # mostrar puntos
  labelsize = 6, # tamaño de letras
  label = "all", #  Mostrar etiquetas por defecto
  col.ind = years, # color por año
  gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), # paleta para años
  col.var = "#009ACD", # color de vectores de variables
  repel = TRUE # evitar solapamiento
) +
  geom_text(
    aes(label = years), # añadir texto de los años
    vjust = -0.8,
    size = 5,
    color = "black"
  )

var_pca <- get_pca_var(pca)

print(var_pca$coord)


