library(tidyverse)
library(irr)
library(corrplot)

# creating the correlation matrix of RCE, velocity and delta temp* across scenarios

scenario_list <- c('SSP126', 'SSP245', 'SSP585')

# calling climate metrics' objects
velocity <- 'outputs/05_Climate/Velocity/'
RCE <- 'outputs/05_Climate/RCE/'

for(i in 1:length(scenario_list)){
  velo_temp <- readRDS(paste0(velocity, 'velocity', scenario_list[i], '.rds')) %>% 
    as_tibble() %>% 
    dplyr::select(value) %>% 
    rename(velocity = value)
  RCE_temp <- readRDS(paste0(RCE, 'RCE', scenario_list[i], '.rds')) %>% 
    as_tibble() %>% 
    dplyr::select(value) %>% 
    rename(RCE = value)
  assign(paste0('velocity_',scenario_list[i]) ,velo_temp)
  assign(paste0('RCE_', scenario_list[i]) , RCE_temp)
  rm(velo_temp, RCE_temp)
}

# creating kappa correlation for velocity
v1 <- velocity_SSP126 %>% 
  dplyr::mutate(SSP126 = cut(velocity_SSP126$velocity, breaks = seq(0, 750, by = 25))) %>% 
  dplyr::select(SSP126)
v2 <- velocity_SSP245 %>% 
  dplyr::mutate(SSP245 = cut(velocity_SSP245$velocity, breaks = seq(0, 750, by = 25))) %>% 
  dplyr::select(SSP245)
v3 <- velocity_SSP585 %>% 
  dplyr::mutate(SSP585 = cut(velocity_SSP585$velocity, breaks = seq(0, 750, by = 25))) %>% 
  dplyr::select(SSP585)

v_full <- list(v1, v2, v3)
y = 1
s_matrix <- list()

for(i in 1:3){
  for(j in 1:3){
    kappa_temp <- irr::kappa2(bind_cols(v_full[[i]], v_full[[j]]))
    kappa_corrvalue <- kappa_temp$value
    kappa_pvalue <- kappa_temp$p.value
    s_matrix[[y]] <- cbind(colnames(v_full[[i]]), colnames(v_full[[j]]), kappa_corrvalue, kappa_pvalue)
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

rownames(matrix_final1) <- matrix_final1[,1]
n <- 3 + 1 # 3 is the number of inputted scenarios
matrix_final2 <- matrix_final1[,2:n]
class(matrix_final2) <- "numeric"

col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
plot <- corrplot(matrix_final2, method = "shade", cl.lim = c(-0.1,1), tl.col = "black", addCoef.col = "black",
                 col=col(200), tl.srt=45)

# create kappa correlation for RCE
v1 <- RCE_SSP126 %>% 
  dplyr::mutate(SSP126 = cut(RCE_SSP126$RCE, breaks = seq(0, 1025, by = 25))) %>% 
  dplyr::select(SSP126)
v2 <- RCE_SSP245 %>% 
  dplyr::mutate(SSP245 = cut(RCE_SSP245$RCE, breaks = seq(0, 1025, by = 25))) %>% 
  dplyr::select(SSP245)
v3 <- RCE_SSP585 %>% 
  dplyr::mutate(SSP585 = cut(RCE_SSP585$RCE, breaks = seq(0, 1025, by = 25))) %>% 
  dplyr::select(SSP585)

v_full <- list(v1, v2, v3)
y = 1
s_matrix <- list()

for(i in 1:3){
  for(j in 1:3){
    kappa_temp <- irr::kappa2(bind_cols(v_full[[i]], v_full[[j]]))
    kappa_corrvalue <- kappa_temp$value
    kappa_pvalue <- kappa_temp$p.value
    s_matrix[[y]] <- cbind(colnames(v_full[[i]]), colnames(v_full[[j]]), kappa_corrvalue, kappa_pvalue)
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

rownames(matrix_final1) <- matrix_final1[,1]
n <- 3 + 1 # 3 is the number of inputted scenarios
matrix_final2 <- matrix_final1[,2:n]
class(matrix_final2) <- "numeric"

col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
plot <- corrplot(matrix_final2, method = "shade", cl.lim = c(-0.1,1), tl.col = "black", addCoef.col = "black",
                 col=col(200), tl.srt=45)
