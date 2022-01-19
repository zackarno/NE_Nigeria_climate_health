#' this works for comparing aggregated data to historical aggregated data
#' it's not pixel level so not sure how much use it will be.


group_data <- function(df,group_vars){
  if(length(group_vars)==1){
    group_sym<-sym(group_vars)
    df_grouped<-df %>%
      group_by(!!group_sym,.drop=FALSE)
  }
  if(length(group_vars)>1){
    group_syms <- syms(group_vars)
    df_grouped<-df %>%
      group_by(!!!group_syms,.drop=FALSE)
  }
  
  return(df_grouped)
}

#' compare to historical
#' @param df data frame
#' @param date date column
#' @param value value of interest
#' @param historical_from date to start historical time line
#' @param historical_to date to end historical time line
#' @param from date to start period of interest for comparison
#' @param group_vars variable to aggregate historical data (default= "month_fct")
#' @return data from filtered to dates greater than period of interest `from` date with `historical mean`, `historical sd`, di


compare_to_historical <-  function(df, 
                                   date="date",
                                   value="value",
                                   historical_from,
                                   historical_to, 
                                   group_vars= "month_fct",
                                   from){
  
  
  
  
  historical_monthly <- df |> 
    filter(date>=historical_from & date< historical_to) |> 
    group_data(group_vars = group_vars) |> 
    summarise(
      mean_historical= mean(!!sym(value),na.rm=T),
      sd_historical = sd(!!sym(value),na.rm=T),.groups = "drop"
    )
  
  
  period_of_interest <- df |> 
    filter(date>=from) 
  
  df_j <- period_of_interest|> 
    left_join(historical_monthly, by = group_vars)
  
  assertthat::assert_that(nrow(period_of_interest)==nrow(df_j),msg="Problem with join")
  df_j |> 
    mutate(
      value_diff = !!sym(value)-mean_historical,
      value_z = value_diff/sd_historical
    )
  
  
  
}
