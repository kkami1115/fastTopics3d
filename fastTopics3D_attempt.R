install.packages("pacman")
install.packages("BiocManager")
pacman::p_load(scRNAseq, scater, TSCAN, AnnotationHub, scran, scuttle, velociraptor, tidyverse, bluster, Matrix, fastTopics, cowplot, plotly, htmlwidgets, RColorBrewer)

# data from scRNAseq package
sce.sperm <- HermannSpermatogenesisData(strip=TRUE, location=TRUE)
assayNames(sce.sperm)

# Quality control (copied from vignette)
is.mito <- which(seqnames(sce.sperm)=="MT")
sce.sperm <- addPerCellQC(sce.sperm, subsets=list(Mt=is.mito), assay.type="spliced")
qc <- quickPerCellQC(colData(sce.sperm), sub.fields=TRUE)
sce.sperm <- sce.sperm[,!qc$discard]

# Normalization (copied from vignette)
set.seed(10000)
sce.sperm <- logNormCounts(sce.sperm, assay.type="spliced")
dec <- modelGeneVarByPoisson(sce.sperm, assay.type="spliced")
hvgs <- getTopHVGs(dec, n=2500)

# Dimensionality reduction (copied from vignette)
set.seed(1000101)
sce.sperm <- runPCA(sce.sperm, ncomponents=25, subset_row=hvgs)
sce.sperm <- runTSNE(sce.sperm, dimred="PCA")
sce.sperm <- runUMAP(sce.sperm, name = "UMAP")

velo.out <- scvelo(sce.sperm, assay.X="spliced", 
                   subset.row=hvgs, use.dimred="PCA")
velo.out

sce.sperm$pseudotime <- velo.out$velocity_pseudotime

embedded <- embedVelocity(reducedDim(sce.sperm, "TSNE"), velo.out)
grid.df <- gridVectors(reducedDim(sce.sperm, "TSNE"), embedded, resolution=30)

# Draw 2D t-SNE plot and velocity plot 
tsne_plot <- plotTSNE(sce.sperm, colour_by="pseudotime", point_alpha=0.3) +
  geom_segment(data=grid.df, 
               mapping=aes(x=start.1, y=start.2, xend=end.1, yend=end.2), 
               arrow=arrow(length=unit(0.05, "inches"), type="closed")) +
  theme(legend.position = "none")
tsne_plot
ggsave(filename = "tsne_plot.png", width = 9, height = 9)
ggsave(filename = "tsne_plot.pdf", width = 9, height = 9)


# Calculate NMF and draw structure plot (copied from fastTopics vignette)
rawcounts <- t(exp(as.matrix(SingleCellExperiment::logcounts(sce.sperm))))
fit <- fit_topic_model(rawcounts, k = 6)
samples <- sce.sperm$celltype
topic_colors <- c("skyblue","forestgreen","darkmagenta","dodgerblue","gold","darkorange")
str_plot <- structure_plot(fit,colors = topic_colors,topics = 1:6,gap = 25,
                           grouping = samples)
str_plot
ggsave(filename = "structure_plot.png", width = 9, height = 9)
ggsave(filename = "structure_plot.pdf", width = 9, height = 9)


# Map fastTopics results onto t-SNE plot in 3D
x_data = tsne_plot$data$X
y_data = tsne_plot$data$Y
z_data1 = fit$L[,1]
z_data2 = fit$L[,2]
z_data3 = fit$L[,3]
z_data4 = fit$L[,4]
z_data5 = fit$L[,5]
z_data6 = fit$L[,6]

fig <-  plot_ly(x = ~x_data, y = ~y_data, z = ~z_data1, type = 'scatter3d',opacity = 0.4, colors = RColorBrewer::brewer.pal(8, "Set1")[[1]] ) 
fig <- fig %>% plotly::add_markers(x = ~x_data, y = ~y_data, z = ~z_data1+z_data2, opacity = 0.4, colors = RColorBrewer::brewer.pal(8, "Set1")[[2]] )
fig <- fig %>% plotly::add_markers(x = ~x_data, y = ~y_data, z = ~z_data1+z_data2+z_data3, opacity = 0.4, colors = RColorBrewer::brewer.pal(8, "Set1")[[3]]  )
fig <- fig %>% plotly::add_markers(x = ~x_data, y = ~y_data, z = ~z_data1+z_data2+z_data3+z_data4, opacity = 0.4, colors = RColorBrewer::brewer.pal(8, "Set1")[[4]]  )
fig <- fig %>% plotly::add_markers(x = ~x_data, y = ~y_data, z = ~z_data1+z_data2+z_data3+z_data4+z_data5, opacity = 0.4, colors = RColorBrewer::brewer.pal(8, "Set1")[[5]]  )
fig <- fig %>% plotly::add_markers(x = ~x_data, y = ~y_data, z = ~z_data1+z_data2+z_data3+z_data4+z_data5+z_data6, opacity = 0.4, colors = RColorBrewer::brewer.pal(8, "Set1")[[6]] )

fig

saveWidget(fig, "3dscatterplot.html", selfcontained = TRUE, title = "fastTopics in 3D")
