
# This makes a list. The top level of the list splits by the first entry in group_vars. The second level
# splits by the second entry and so on. At the bottom level when all group_vars are enhausted there should
# be a unique. SORT THE DATA BY X BEFORE USING IT.
make_fast_approximation_function_list = function(dataframe, group_vars, x_var, y_var, approx_func){
  len = length(group_vars)
  if (len == 0){
    return(approx_func(dataframe[,x_var], dataframe[,y_var]))
  }
  this_group = group_vars[1]
  remaining_groups = group_vars[group_vars != this_group]

  group_levels = as.character(unique(dataframe[,this_group]))
  list_ = list()
  for (group in group_levels){
    dataframe2 = dataframe[which(as.character(dataframe[,this_group]) == group), ]
    list_[[group]] = make_fast_approximation_function_list(dataframe = dataframe2, group_vars = remaining_groups, x_var = x_var, y_var = y_var, approx_func = approx_func)
  }
  return(list_)
}

#' make_approx_functions_from_dataframe
#' @export
#' @param dataframe A data.frame with your data.
#' @param group_vars The variable names in the dataframe that subset the data into the various groups.
#' @param x_var The name of the x variable in the dataframe
#' @param y_var The name of the y variable in the dataframe.
#' @param approx_func A function that takes in two arguments, an x vector and a y vector. Make sure it can handle vectors of length 0 (if that can happen in your data).
#' @return A function of the form function(groupvar1, groupvar2, ..., x).
#'
#' @examples
#' # Generating example data.
#' # Consider we have equity prices for several days and times.
#' RICs = c("BARC.L", "VOD.L", "IBM.L")
#' Dates = as.Date(c("11-11-2019", "12-11-2019", "13-11-2019",
#'            "14-11-2019", "15-11-2019"), format="%d-%m-%Y")
#' times = seq(0,28800, length.out = 10) # The number of seconds into the trading day.
#' dd = expand.grid(TIME = times, Date = Dates, RIC = RICs)
#' dd = merge(dd, data.frame(RIC = RICs, PRICE = c(160.00, 162.24, 137.24)))
#' randomness = rlnorm(dim(dd)[1])
#' dd$PRICE = dd$PRICE * cumprod(randomness)
#'
# We can build a list of approxfun functions for each date and stock.
#' approx_func = function(x,y){approxfun(x, y)}
#' dispatched_approxfun = make_approx_functions_from_dataframe(dd,
#'                                 group_vars = c("RIC", "Date"),
#'                                  x_var = "TIME", y_var = "PRICE",
#'                                                       approx_func)
#' dispatched_approxfun("BARC.L", Dates[2], c(100, 156, 6045))
make_approx_functions_from_dataframe= function(dataframe, group_vars, x_var, y_var, approx_func){
  dataframe = dataframe[order(dataframe[,x_var]),]
  len = length(group_vars)
  list_ = make_fast_approximation_function_list(dataframe = dataframe, group_vars = group_vars, x_var = x_var, y_var = y_var, approx_func = approx_func)
  func_text = paste0("f = function(", paste0(paste0("v",1:len), collapse=", "),", xout){\n")
  func_text = paste0(func_text, paste0("list_[[", paste0(paste0("as.character(v",1:len, ")"), collapse="]][["), "]](xout) \n }"))
  ff = eval(parse(text = func_text))
  return(compiler::cmpfun(ff))
}
