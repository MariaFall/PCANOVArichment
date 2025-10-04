

DrawScorePlotBuff = function (scores, ColorSet, title, pcaBuff) {
  
  var_explained <- (pcaBuff$sdev)^2 
  var_explained <- round(100 * var_explained / sum(var_explained), 1)

  scores = scores %>% 
    unnest(c(PC1, PC2, Groups))

  plotbuff <- ggplot(scores, aes(x = PC1, y = PC2, color = as.factor(Groups), label = "", fill = as.factor(Groups))) +
    scale_color_manual(values= ColorSet) +
    scale_fill_manual(values= ColorSet) +
    geom_point(size = 5, alpha = 1, show.legend = T) +
    geom_text(vjust = -1, hjust = 0.5, size = 6, show.legend = F) +
    labs(
      title = paste(title),
      x = paste0("PC1 (", var_explained[1], "%)"),
      y = paste0("PC2 (", var_explained[2], "%)")
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 35),
      plot.subtitle = element_text(hjust = 0.5),
      legend.position = "right", axis.title.x = element_text(size = 20),
      axis.title.y = element_text(size = 20)
    ) +
    stat_chull(geom = "polygon", alpha = 0.3, show.legend = FALSE) +
    theme(panel.border = element_rect(
      color = "#000000",
      fill = NA,
      linewidth = 1
    )
    )
  
  return(plotbuff)
}

DrawScreePlotBuff = function (pcaBuff, title) { 
  
  var_explained <- (pcaBuff$sdev)^2 
  var_explained <- round(100 * var_explained / sum(var_explained), 1)

  PCs_to_show = length(var_explained) -10
  pcaDFBuff = data.frame(
    PC = 1:PCs_to_show,
    variance = var_explained[1:PCs_to_show],
    cum_variance = cumsum(var_explained[1:PCs_to_show])
  )
  
  ScreePlotBuff <- ggplot(pcaDFBuff, aes(x = PC)) +
    geom_line(aes(y = variance, group = 1), color = "steelblue", linewidth = 1.2) +  
    geom_point(aes(y = variance), color = "steelblue", size = 3) +   
    geom_text(aes(y = variance, label = paste0(round(variance, 1), "%")), 
              vjust = -0.5, size = 6, color = "black") +
    geom_line(aes(y = cum_variance, group = 1), color = "red", linewidth = 1.2) +
    geom_point(aes(y = cum_variance), color = "red", size = 3) +
    geom_text(aes(y = cum_variance, label = paste0(round(cum_variance, 1), "%")), 
              vjust = -0.5, size = 6, color = "black") +
    labs(
      title = paste(title),
      x = "Principal Components",
      y = "Explained Variance (%)"
    ) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 35),
      axis.title.x = element_text(size = 16),
      axis.title.y = element_text(size = 16),
      axis.text = element_text(size = 12)
    )
  
  return(ScreePlotBuff)
}

DrawLoadingsPlotBuff = function (pcaBuff, title) {
  
  loadings <- as.data.frame(pcaBuff$rotation)
  loadings_long <- loadings %>%
    rownames_to_column("Variable") %>%
    pivot_longer(cols = starts_with("PC"), names_to = "PC", values_to = "Loading")
  
  LoadingsPlotBuff = ggplot(filter(loadings_long, PC %in% c("PC1", "PC2")), 
                            aes(x = Variable, y = Loading, fill = PC)) +
    geom_col(position = "dodge") +
    labs(title = paste(title), y = "Loading Value", x = "Variable") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 20),
          plot.title = element_text(hjust = 0.5, face = "bold", size = 35),
          axis.text.y = element_text(size = 25))
  
  return(LoadingsPlotBuff)
}

FindsGridSize <- function(ListLength) {
  #closest biggest whole root of length of list
  #GridSizeBuff <- ceiling(sqrt(ListLength))
  GridSizeBuff = 1
  return(GridSizeBuff)
}

CreatePlotGrid = function (gridtitle, PlotList_list) {

  GridSize = FindsGridSize(length(PlotList_list)); 
  pdf(paste(gridtitle), width = 22, height = 15)
  grid.arrange(grobs = PlotList_list, ncol =GridSize, nrow =GridSize)
  dev.off()

  return(grid.arrange(grobs = PlotList_list, ncol =GridSize, nrow =GridSize))
}
