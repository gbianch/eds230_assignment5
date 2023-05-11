#' Compute Extreme Flows
#'
#' Compute percent error between the minimum observation and model
#' @param  m  model estimates
#' @param  o  observations
#' @param  wy water year
#' @param wts weights for the min and max err
#' @return min_err_trans, max_err_trans, combined
#' 

compute_extremes <- function(m=sager$model, o=sager$obs, wy=sager$wy, wts=c(0.5, 0.5)){
  flow = cbind.data.frame(m, o, wy) 
  wy_flow = flow %>% 
    group_by(wy) %>% 
    summarize(min_obs = min(o),
              min_mod = min(m),
              max_obs = max(o),
              max_mod = max(m))
  
  # 1 over mean error so higher values are better
  min_rel_err = mean((wy_flow$min_mod - wy_flow$min_obs)/wy_flow$min_obs)
  max_rel_err = mean((wy_flow$max_mod - wy_flow$max_obs)/wy_flow$max_obs)
  
  min_err_trans = 1 - min(1.0, abs(min_rel_err))
  max_err_trans = 1 - min(1.0, abs(max_rel_err))
  
  combined = wts[1]*min_err_trans + wts[2]*max_err_trans
  
  
  return(list(min_err_trans, max_err_trans, combined))
  
}
