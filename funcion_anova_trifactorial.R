library(dplyr)
library(agricolae)

custom_analysis_function <- function(data_frame) {
  factor_columns <- colnames(data_frame)[1:3]
  continuous_columns <- colnames(data_frame)[4:ncol(data_frame)]
  
  results_list <- list()
  
  for (continuous_var in continuous_columns) {
    anova_formula <- paste(continuous_var, "~ producto * panelista * sesion")
    anova_model <- aov(as.formula(anova_formula), data = data_frame)
    
    lsd_test <- LSD.test(anova_model, trt = "producto", alpha = 0.05, group = TRUE)
    compact_letters <- lsd_test$groups
    compact_letters <- rownames_to_column(compact_letters, var = "producto")
    compact_letters <- dplyr::select(compact_letters, producto, groups )
    
    means <- data_frame %>%
      group_by(producto) %>%
      summarise(mean = mean(.data[[continuous_var]], na.rm = TRUE),
                stderr = sd(.data[[continuous_var]], na.rm = TRUE) / sqrt(n()))
    
    result <- left_join(means, compact_letters, by = "producto")
    
    p_values <- c(summary(anova_model)[[1]][["Pr(>F)"]][[1]], 
                  summary(anova_model)[[1]][["Pr(>F)"]][[2]],
                  summary(anova_model)[[1]][["Pr(>F)"]][[3]])
    
    p_values_interactions <- c(summary(anova_model)[[1]][["Pr(>F)"]][[4]],
                               summary(anova_model)[[1]][["Pr(>F)"]][[5]],
                               summary(anova_model)[[1]][["Pr(>F)"]][[6]])
    
    result <- result %>%
      mutate(formatted_mean = sprintf("%.2f", mean),
             formatted_stderr = sprintf("%.2f", stderr),
             combined_info = paste(formatted_mean, " ± ", formatted_stderr, ifelse(is.na(groups), " ", groups), sep = " ")) %>%
      dplyr::select(-mean, -stderr, -groups, -formatted_mean, -formatted_stderr)
    
    p_values_rounded <- sprintf("%.4f", p_values)
    p_values_interactions_rounded <- sprintf("%.4f", p_values_interactions)
    
    result_pvalues <- data_frame(producto = "P-values", combined_info = p_values_rounded)
    result_interactions <- data_frame(producto = "P-values Interactions", combined_info = p_values_interactions_rounded)
    
    result <- bind_rows(result, result_pvalues, result_interactions)
    
    results_list[[continuous_var]] <- result
  }
  
  # Combine the list of data frames into a single data frame
  final_result <- as.data.frame(results_list)
  
  # Remove columns with "producto" in their name, except the first one
  cols_to_remove <- grep("producto", colnames(final_result))[-1]
  final_result <- final_result[, -cols_to_remove]
  
  # Remove "combined_info" from column names and keep only the part before the dot
  colnames(final_result)[-1] <- sub("\\..*", "", colnames(final_result)[-1])
  
  return(final_result)
}

results <- custom_analysis_function(data)

write.csv(results,"results.csv")

