#' set_date_range_ic
#' @description designed as a helper function 
#' @param dat gee data (image collection or image?)
#' @param start_date start date provided as yyyy-mm-dd (default= null)
#' @param end_date end date provided as yyyy-mm-dd (default= null)
#' @return a list with start and end dates which can be provided to `filterDate` to filter IC


set_date_range_ic <-  function(dat, start_date=NULL, end_date=NULL){
  
  filter_range <- list()
  cat("collecting available ic dates\n")
  all_imgs<- rgee::ee_get_date_ic(dat)
  range_available <- all_imgs$time_start |> range()
  if(!is.null(start_date)){
    if(range_available[1]>start_date){
      message("start_date provided occurs before first image in image collection. Setting start_date to first image date\n")
      filter_range$start_date <- range_available[1]
    }else{
      filter_range$start_date <- start_date
    }
    
  }
  if(!is.null(end_date)){
    if(range_available[2]<end_date){
      message("end_date provided occurs after last image image in image collection. Setting end_date to last image date\n")
      filter_range$end_date <- range_available[2]
    }else{
      filter_range$end_date <- end_date
    }
    
  }
  if(is.null(start_date)){
    cat("no start date provided - setting to first image date\n")
    filter_range$start_date <-  range_available[1]
  }
  if(is.null(end_date)){
    cat("no end_date provied- setting to last image date\n")
    filter_range$end_date <-  range_available[2]
  }
  
  return(filter_range)
  
}

