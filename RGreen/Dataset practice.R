Pollen <- read.delim("Table_S7.tab", sep="\t", header = TRUE)

head(Pollen)

plot_colors <- rainbow(3, rev = TRUE)

plot(Pollen$Rainforest....,
     Pollen$Age..ka.BP.,
     pch = Pollen$Sample.label,
     main = "Rainforest percent vs Age",
     xlab = "Rainforest %",
     ylab = "Age (kaBP)",
     col = plot_colors
)

Pollen_hist <- hist(Pollen$Rainforest....)
