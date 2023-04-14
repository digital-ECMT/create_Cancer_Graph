rankTreatments <- function(variantList, maxDepth=2, treatmentList) {
  print(variantList)
  ## calculate combined effect of variants
  # variant_effects %>%
  #   dplyr::filter(source_node %in% example_variant_list[[1]]) %>%
  #   dplyr::filter(pathlength <= maxDepth) %>%
  #   # dplyr::filter(affected_gene == "PIK3CA") %>%
  #   group_by(affected_gene, affected_gene_role) %>%
  #   summarise(
  #     net_effect = sum(link_product)
  #   ) %>%
  #   as.data.frame() %>%
  #   ggplot(aes(x=affected_gene, y="patient X", fill=net_effect)) +
  #   geom_tile(color = "black") +
  #   scale_fill_gradient2(midpoint = 0, low = "blue", mid = "white",high = "red", space = "Lab", name = "Effect on tumour driver gene") +
  #   facet_grid(. ~ affected_gene_role, scales = "free", space = "free") %>%
  #   print()
  
  
  variant_effects %>%
    dplyr::filter(source_node %in% example_variant_list[[1]]) %>%
    dplyr::filter(pathlength <= maxDepth) %>%
    # dplyr::filter(affected_gene == "PIK3CA") %>%
    as.data.frame() %>%
    ggplot(aes(x=affected_gene, y=source_node, fill = link_product/pathlength)) +
    geom_tile(color = "black") +
      scale_fill_gradient2(midpoint = 0, low = "blue", mid = "white",
                           high = "red", space = "Lab", name = "Effect on tumour driver gene", limits = c(-1,1), breaks = c(1,0.5, 0,-0.5, -1), labels = c("Direct activation", "Indirect activation", "No effect", "Indirect inactivation", "Direct inactivation") ) +
      labs(title = paste0("Effect of patient variants on known tumour drivers (max depth:", maxDepth, ")"), x="Driver genes", y="Condition") +
      scale_x_discrete(position = "top") +
      facet_grid(source_node_type ~ affected_gene_role, scales = "free", space = "free") %>%
    print()
  
  
  ## display a single-row summary table under the plot, as per https://stackoverflow.com/questions/62047359/plot-a-table-of-separate-data-below-a-ggplot2-graph-that-lines-up-on-the-x-axis 
  
  
}