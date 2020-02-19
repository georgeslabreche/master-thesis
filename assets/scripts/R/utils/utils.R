#' Get the time required to traverse a target distance.
#'
#' @param d_traverse target distance in meters.
#' @param get_traverse_time traverse velocity [m/s].
#' @param average_slippage_rate
#' 
#' @return
#' @export
#'
#' @examples
get_traverse_time = function(d_traverse, v_traverse=0.02, average_slippage_rate=0.15){
  
  # Traverse distance in meters, add extra distance to account for slippage.
  d_traverse_actual = d_traverse * (1 + average_slippage_rate) 
  
  # Traverse time.
  t_traverse = (d_traverse_actual /  v_traverse) / 60
  
}
