

mat1 = matrix(rnorm(25), nrow = 5)
rownames(mat1) = paste0("A", 1:5)
colnames(mat1) = paste0("B", 1:5)

mat2 = matrix(rnorm(25), nrow = 5)
rownames(mat2) = paste0("A", 1:5)
colnames(mat2) = paste0("C", 1:5)

mat3 = matrix(rnorm(25), nrow = 5)
rownames(mat3) = paste0("B", 1:5)
colnames(mat3) = paste0("C", 1:5)

mat = matrix(0, nrow = 10, ncol = 10)
rownames(mat) = c(rownames(mat2), rownames(mat3))
colnames(mat) = c(colnames(mat1), colnames(mat2))
mat[rownames(mat1), colnames(mat1)] = mat1
mat[rownames(mat2), colnames(mat2)] = mat2
mat[rownames(mat3), colnames(mat3)] = mat3
mat



nm = unique(unlist(dimnames(mat)))
group = structure(gsub("\\d", "", nm), names = nm)
group


grid.col = structure(c(rep(2, 5), rep(3, 5), rep(4, 5)),
                     names = c(paste0("A", 1:5), paste0("B", 1:5), paste0("C", 1:5)))


circos.clear()
chordDiagram(mat, group = group, grid.col = grid.col,
             annotationTrack = c( "grid"),
             preAllocateTracks = list(
               track.height = mm_h(2), ## height of the outer par
               track.margin = c(mm_h(0.5), 0)
             )
             )


circos.track(track.index = 2, panel.fun = function(x, y) {
  sector.index = get.cell.meta.data("sector.index")
  xlim = get.cell.meta.data("xlim")
  ylim = get.cell.meta.data("ylim")
  circos.text(mean(xlim), mean(ylim), sector.index, cex = 0.4, niceFacing = TRUE)
}, bg.border = NA)



highlight.sector(rownames(mat1), track.index = 1, col = "red", 
                 text = "A", cex = 0.5, text.col = "white", niceFacing = F)
highlight.sector(colnames(mat1), track.index = 1, col = "green", 
                 text = "B", cex = 0.8, text.col = "white", niceFacing = TRUE)
highlight.sector(colnames(mat2), track.index = 1, col = "blue", 
                 text = "C", cex = 0.8, text.col = "white", niceFacing = TRUE)
