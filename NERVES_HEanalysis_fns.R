
## Function for deterministic analysis
fn_computeicer <- function(i,data,predmat) {
  return(fn_lmregression(fn_imputation(data,predmat)))
}

## Function for bootstrap analysis
fn_booticer <- function(i,data,predmat) {
  iids <- sample(1:157, replace=T) # get sample IDs
  idata <- data[iids,] # subset data
  return(fn_lmregression(fn_imputation(idata,predmat)))
}

## Performs the MI, 
# Calls the post process function, runs regressions, makes predictions and calculates required statistics
fn_imputation <- function(inputdata,predmat) {
  
  ## Perform multiple imputation
  imp1 <- mice(inputdata, 
               m=10, 
               maxit=30,
               meth='pmm',
               printFlag = F,
               predictorMatrix=predmat)
  
  ## Process MI data to get total costs and QALYs
  df <- fn_postmiprocess(imp1) 
  return(df)
}  
    
## Function to estimate statistical models on long post-MI dataframe
fn_lmregression <- function(df) {
 
  ## Setup linear components 
  r1 <- qaly ~ trtalloc + u1
  r2 <- cost_total_fu ~ trtalloc + cost_total_bl

  ## For storing coefficients
  output <- array(dim=c(10,6))
  
  ## Loop MI datasets, fit model and save coefficients
  for(i in (df$.imp)) {
    # Create a dataset name
    coeffnam <- paste0("coeffs_imp", i)
    # Fit model, extract coefficients and assign to the dataset
    assign(coeffnam, rbind(coef(summary(glm(r1, data = df[df$.imp==i,], family = gaussian()))),
                           coef(summary(glm(r2, data = df[df$.imp==i,], family = Gamma(link=log))))))
    output[i,] <- eval(as.symbol(coeffnam))[,1]
  }
  
  ## Pool coefficients across imputation datasets
  pooled_coeffs <- colMeans(output)
  
  ## Extract only the data required for making predictions
  df_pred <- filter(df,.imp==2) %>%
    dplyr::select(rid,trtalloc,u1,cost_total_bl)

  ## Predict QALYs
  qaly_pred_surg <- sum(pooled_coeffs[1] + pooled_coeffs[3]*df_pred$u1)/nsubj
  qaly_pred_injn <- sum(pooled_coeffs[1] + pooled_coeffs[2] + pooled_coeffs[3]*df_pred$u1)/nsubj
  
  ## Predict Costs
  cost_pred_surg <- sum(exp(pooled_coeffs[4] + pooled_coeffs[6]*df_pred$cost_total_bl))/nsubj
  cost_pred_injn <- sum(exp(pooled_coeffs[4] + pooled_coeffs[5] + pooled_coeffs[6]*df_pred$cost_total_bl))/nsubj
  
  ## Compute incremental costs and QALYs
  delta_cost <-  cost_pred_surg - cost_pred_injn
  delta_qaly <-  qaly_pred_surg - qaly_pred_injn
  
  ## Calculate ICER & NMBs
  icer_SvsI <- delta_cost / delta_qaly
  nmb_SvsI <- delta_qaly*lambda - delta_cost
  
  ## Return results
  results <- c(pooled_coeffs,
               cost_pred_surg,cost_pred_injn,qaly_pred_surg,qaly_pred_injn,
               delta_cost,delta_qaly,
               icer_SvsI,nmb_SvsI)  
  return(results)
}

## Function for post-process of mids MI object
# Produces long dataset with total costs and QALYs
fn_postmiprocess <- function(midsimp) {
  
  ## Convert to Long
  imp1_long <- mice::complete(midsimp, action='long', include=TRUE) %>%
    mutate(newid = paste(.imp,.id,sep="_")) 

  ## Create new variables
  imp1_long2 <- imp1_long %>%
    mutate(qaly = unlist(map(unique(imp1_long$newid),~fn_intutilities(data=imp1_long,.)))) %>%
    mutate(cost_crf_fu = rowSums(dplyr::select(., c_gp.2:c_ane.6),na.rm=T)) %>%
    mutate(cost_crf_bl = rowSums(dplyr::select(., c_gp.1:c_ane.1),na.rm=T)) %>%
    mutate(cost_total_bl = cost_crf_bl + cost_conmeds_bl + cost_hes_apc_bl + cost_hes_op_bl) %>%
    mutate(cost_total_fu = cost_crf_fu + cost_conmeds_fu + cost_hes_apc_fu + cost_hes_op_fu) 
  
  imp1_long2_subs <- imp1_long2 %>%
    filter(.imp>0) %>%
    dplyr::select(.imp,.id,newid,rid,site,gender,agegrp,lpweeks,trtalloc,u1,qaly,cost_total_bl,cost_total_fu)
  return(imp1_long2_subs)
}

## Function for compute QALYs - for imputed data long
fn_intutilities <- function(data, id) {
  qaly <- data %>%
    filter(newid==id) %>%
    dplyr::select(u1:u6) %>%
    gather(key,value) %>%
    mutate(time = c(0, 18/52, 30/52, 42/52, 54/52)) %>%
    summarise(x = trapz(time,value))
  return(qaly)
}  
