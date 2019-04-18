# PROJECT:  COP19 TZA
# AUTHOR:   A.CHAFETZ
# PURPOSE:  create function to visualize cascade
# DATE:     2019-04-18

plot_cascade <- function(prime, filepath_save = NULL){
  
  #graph title
  partner_title <- paste(prime, "Trends")
  
  #grah HTS bar chart
  viz_hts <- df_monthly_hts %>% 
    filter(partner == prime) %>% 
    ggplot(aes(month, val, fill = pd)) +
    geom_hline(yintercept = 0, color = "#bfbfbf", size = .5) +
    geom_col() +
    geom_point(aes(y = max), color = "white", size = 0) +
    geom_text(aes(label = comma(val)),
              family = "Gill Sans MT",
              vjust = -1,
              color = "#595959") +
    scale_y_continuous(labels = comma) +
    scale_fill_manual(values = c("#67A9CF", "#2166AC")) +
    expand_limits(y = 0) +
    facet_grid(. ~indicator, scales = "free_y") +
    labs(x = "", y = "") +
    theme_light() +
    theme(legend.position = "none",
          panel.border = element_blank(),
          text = element_text(family = "Gill Sans MT", color = "#595959"),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          strip.text = element_text(size = 12, face = "bold")
    )
  
  #graph positivity/linkage/retention
  viz_pct <- df_monthly_pct %>% 
    filter(partner == prime) %>% 
    ggplot(aes(month, val, group = pd, color = pd)) +
    geom_hline(yintercept = 0, color = "#bfbfbf", size = .5) +
    geom_point(aes(y = max), color = "white", size = 0) +
    geom_line(size = 1) +
    geom_point(size = 4) +
    geom_text(aes(label = percent(val)),
              family = "Gill Sans MT",
              vjust = -1,
              color = "#595959") +
    expand_limits(y = 0) +
    facet_grid(indicator ~ ., scales = "free_y", switch = "y") +
    scale_y_continuous(labels = percent_format(1)) +
    scale_color_manual(values = c("#67A9CF", "#2166AC")) +
    labs(x = "", y = "") +
    theme_light() +
    theme(legend.position = "none",
          panel.border = element_blank(),
          text = element_text(family = "Gill Sans MT", color = "#595959", size = 12),
          axis.ticks = element_blank(),
          axis.text.y = element_blank(),
          panel.grid.minor.x = element_blank(),
          strip.text = element_text(size = 12, face = "bold"))
  
  #combine
  if(!is.null(filepath_save)){
    plot <- arrangeGrob(viz_hts, viz_pct, ncol = 2, 
                        top = textGrob(partner_title, 
                                       gp = gpar(fontfamily = "Gill Sans MT", fontsize = 13),
                                       hjust = 1,
                                       x = .15),
                        bottom = textGrob(
                          "Source: Tanzania Monthly Reporting Portal, Basic Raw Data, Oct 2018 - Feb 2019",
                          gp = gpar(fontfamily = "Gill Sans MT", fontsize = 9, col = "#595959"),
                          hjust = 1,
                          x = 1
                        ))
  
   } else {
    plot <- grid.arrange(viz_hts, viz_pct, ncol = 2,
                        top = textGrob(partner_title,
                                       gp = gpar(fontfamily = "Gill Sans MT", fontsize = 13),
                                       hjust = 1,
                                       x = .15),
                        bottom = textGrob(
                          "Source: Tanzania Monthly Reporting Portal, Basic Raw Data, Oct 2018 - Feb 2019",
                          gp = gpar(fontfamily = "Gill Sans MT", fontsize = 9, col = "#595959"),
                          hjust = 1,
                          x = 1
                        ))
    return(plot)
    
  }
  
  
  if(!is.null(filepath_save)){
    ggsave(paste0("COP19_TZA_Cascade_v_", format(Sys.time(), "%H%M%OS1"), "_", prime, ".png"), 
           plot,
           path = filepath_save,
           height = 5.5, width = 9.5, units = "in", dpi = 300)
    print(paste(prime, "...saved"))
  }
  
}  
