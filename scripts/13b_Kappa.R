# This code was written by Tin Buenafe (2021)
# email: tinbuenafe@gmail.com
# Please do not distribute this code without permission.
# There are no guarantees that this code will work perfectly.

# This code creates corrplot of Cohen's Kappa Coefficient.
# Also saves the correlations and p-values in .csv file.

# This function, solution_summary() requres the following inputs:
# 1. inpdir = where the .rds (sf) files are located & where the .csv file will be saved.
# 2. name_cols = names of the columns. see run for example.

# Function is run at 13c.

#############################
##### KAPPA COEFFICIENT #####
#############################

kappa_corrplot <- function(inpdir, name_cols, ...) {

##################################
### Defining the main packages ###
##################################

# List of packages that we will use
list.of.packages <- c("tidyverse", "irr", "corrplot", "sf")
# If is not installed, install the pacakge
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
# Load packages
lapply(list.of.packages, require, character.only = TRUE)

pattern <- paste0("*.rds")
files <- list.files(path = inpdir, pattern = pattern)

# create an empty list
list <- list()
for(i in 1:length(files)){
  list[[i]] <- readRDS(paste0(inpdir, files[i])) %>% 
    as_tibble() %>% 
    select(solution_1)
}

list_final <- do.call(cbind, list)
colnames(list_final) <- name_cols

# create empty list
matrix_list <- list()
y = 1

for(i in 1:ncol(list_final)){
  for(j in 1:ncol(list_final)) {
    x <- cbind(list_final[i],list_final[j])
    kappa_temp <- irr::kappa2(x)
    kappa_value <- kappa_temp$value
    kappa_pvalue <- kappa_temp$p.value
    matrix_list[[y]] <- cbind(colnames(list_final[i]), colnames(list_final[j]), kappa_value, kappa_pvalue)
    y = y + 1
  }
}

matrix_final <- do.call(rbind,matrix_list)
colnames(matrix_final)[1:2] <- c("scenario1","scenario2")

matrix_final1 <- matrix_final %>% 
  as_tibble() %>% 
  select(-kappa_pvalue) %>% 
  pivot_wider(names_from = scenario2, values_from = kappa_value) %>% 
  as.matrix()

matrix_final2 <- matrix_final %>% 
  as_tibble()

write_csv(matrix_final2, paste0(inpdir,"kappa_matrix.csv"))

rownames(matrix_final1) <- matrix_final1[,1]
n <- length(files) + 1
matrix_final2 <- matrix_final1[,2:n]
class(matrix_final2) <- "numeric"

col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
plot <- corrplot(matrix_final2, method = "color", cl.lim = c(0,1), tl.col = "black", addCoef.col = "black",
         col=col(200), tl.srt=45)

return(plot)
}