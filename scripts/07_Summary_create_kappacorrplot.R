create_kappacorrplot <- function(s126, s245, s585, suninformed, dir, ...) {
  
  library(irr)
  library(corrplot)
  
  SSP126 <- s126 %>% 
    as_tibble() %>% 
    dplyr::select(solution_1) %>% 
    dplyr::rename(SSP126 = solution_1)
  SSP245 <- s245 %>% 
    as_tibble() %>% 
    dplyr::select(solution_1) %>% 
    dplyr::rename(SSP245 = solution_1)
  SSP585 <- s585 %>% 
    as_tibble() %>% 
    dplyr::select(solution_1) %>% 
    dplyr::rename(SSP585 = solution_1)
  uninformed <- suninformed %>% 
    as_tibble() %>% 
    dplyr::select(solution_1) %>% 
    dplyr::rename(uninformed = solution_1)
  
  s_list <- list(SSP126, SSP245, SSP585, uninformed)
  y = 1
  s_matrix <- list()
  for(i in 1:4){
    for(j in 1:4){
      kappa_temp <- irr::kappa2(bind_cols(s_list[[i]], s_list[[j]]))
      kappa_corrvalue <- kappa_temp$value
      kappa_pvalue <- kappa_temp$p.value
      s_matrix[[y]] <- cbind(colnames(s_list[[i]]), colnames(s_list[[j]]), kappa_corrvalue, kappa_pvalue)
      y = y+1
    }
  }
  s_matrix_all <- do.call(rbind, s_matrix) %>% 
    as_tibble()
  colnames(s_matrix_all)[1:2] <- c('plan1','plan2')
  
  matrix_final1 <- s_matrix_all %>% 
    as_tibble() %>% 
    dplyr::select(-kappa_pvalue) %>% 
    pivot_wider(names_from = plan2, values_from = kappa_corrvalue) %>% 
    as.matrix()
  
  matrix_final2 <- s_matrix_all %>% 
    as_tibble()
  
  write_csv(matrix_final2, paste0(dir,"kappa_matrix.csv"))
  
  # creating corrplot
  rownames(matrix_final1) <- matrix_final1[,1]
  n <- 4 + 1 # 3 is the number of inputted scenarios
  matrix_final2 <- matrix_final1[,2:n]
  class(matrix_final2) <- "numeric"
  
  col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
  plot <- corrplot(matrix_final2, method = "shade", cl.lim = c(-0.02,1), tl.col = "black", addCoef.col = "black",
                   col=col(200), tl.srt=45)
  return(plot)
}
