#' ic_join_bands
#' @param x,y	A pair of ImageCollections
#' @param by A character vector of variables to join by.
#' @return
#' An object of the same type as `x`. The output has the following properties:
#' Same number of images as `x`
#' Total number of bands equal the number of bands in `x` plus the number of bands in `y`
#' @importFrom rgee ee

ic_join_bands<- function(x, y, by){
  
  # Define an inner join
  innerJoin = rgee::ee$Join$inner()
  
  # Specify an equals filter for image timestamps.
  filterEq <- rgee::ee$Filter$equals(leftField = by, rightField = by)
  
  # Apply the join.
  innerJoined_ic = innerJoin$apply(x, y, filterEq)
  
  
  # Map a function to merge the results in the output FeatureCollection.
  # in the JavaScript code-editor this seems to auto-convert/get coerced to ImageCollection
  joined_fc = innerJoined_ic$map(function(feature)  {
    ee$Image$cat(feature$get('primary'), feature$get('secondary'))
  })
  
  # with rgee is seems necessary to explicitly convert
  rgee::ee$ImageCollection(joined_fc)
}
