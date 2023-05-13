#' Compute extreme flow (min and max) errors (separately and a combined metric)
#'
#' Compute relative error between the minimum observation and model. this assumes a data frame called sager with columns model, obs, and wy is read in already if using defaults.
#' @param  m  model estimates. for example, a column from a df like sager$model
#' @param  o  observations. for example, a column from a df like sager$obs
#' @param  wy water year. for example, a column from a df like sager$wy
#' @param wts weights for the min and max err. default is 0.5 and 0.5
#' @return min_err_trans (error for minimum transformed to be between 0 and 1), max_err_trans (error for maximum transformed to be between 0 and 1), combined (combined metric of the previous two metrics, weighted evenly by default)
#' 

compute_extremes <- function(m, o, wy, wts=c(0.5, 0.5)){
  
  # combine relevant information into a data frame
  flow = cbind.data.frame(m, o, wy) 
  
  # group by water year and get mins and maxs for observed and model values for each water year
  wy_flow = flow %>% 
    group_by(wy) %>% 
    summarize(min_obs = min(o),
              min_mod = min(m),
              max_obs = max(o),
              max_mod = max(m))
  
  # get relative errors for min and max flows and average across all water years
  min_rel_err = mean((wy_flow$min_mod - wy_flow$min_obs)/wy_flow$min_obs)
  max_rel_err = mean((wy_flow$max_mod - wy_flow$max_obs)/wy_flow$max_obs)
  
  # transform so higher values are better and limit to cap of 1
  min_err_trans = 1 - min(1.0, abs(min_rel_err))
  max_err_trans = 1 - min(1.0, abs(max_rel_err))
  
  # create combined metric
  combined = wts[1]*min_err_trans + wts[2]*max_err_trans
  
  # return transformed rel errors for mean min and max flows and combined metric
  return(list(min_err_trans=min_err_trans, max_err_trans=max_err_trans, combined=combined))
  
}
