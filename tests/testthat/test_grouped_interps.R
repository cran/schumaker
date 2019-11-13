# This makes a fake dataframe given a number of groupings, groups per level and observations per group
make_fake_data = function(groupings, group_per_grouping, obs_per_group, seed = 1){
  set.seed(seed)
  agroup = rep(as.character(1:group_per_grouping),obs_per_group)
  dd = eval(parse(text = paste0("expand.grid(", paste0(rep("agroup",groupings),collapse=", "), ")")))
  dd$x = rnorm(dim(dd)[1])
  dd = dd[order(dd$x),]
  dd$y = (dd$x)/(sqrt(as.numeric(row.names(dd))))
  return(dd)
}

# THis makes an approx function by first subsetting an input dataframe. It is intended to show the performance
#  of the naieve approx of interpolating when you have a dataframe with alot of groups data.
make_approx_function = function(dd, group_vars, x_var, y_var){
    len = length(group_vars)
    func_text = paste0("f = function(", paste0(paste0("v",1:len), collapse=", "),", xout){\n")
    for (i in 1:len){
      new_subset = paste0("    dd = dd[which(dd[,\"", group_vars[i],"\"] == ",paste0("v",i),"),]")
      func_text = paste0(func_text, new_subset, "\n")
    }
    func_text = paste0(func_text, "    return(approx(dd[,\"",x_var,"\"], dd[,\"",y_var,"\"], xout = xout))", "\n}")
    ff = eval(parse(text = func_text))
    return(compiler::cmpfun(ff))
}

dd = make_fake_data(3,3,4,1)
group_vars = c("Var1", "Var2", "Var3")
x_var = "x"
y_var = "y"
dumb_approx = make_approx_function(dd, group_vars, x_var, y_var)
dumb_approx("1", "2", "3", 0.4)
approx_func = function(x,y){approxfun(x, y)}
approxfun_in_lists = make_approx_functions_from_dataframe(dd, group_vars, x_var, y_var, approx_func)
approxfun_in_lists("1", "2", "3", 0.4)

tol = 1e-10
test_that("Testing fast approximations by splitting functions to lists.", {
  # Testing for constant extrapolation.
  expect_true(abs(approxfun_in_lists("1", "2", "3", 0.4) - dumb_approx("1", "2", "3", 0.4)$y) < tol)
  expect_true(abs(approxfun_in_lists("2", "2", "3", 0.4) - dumb_approx("1", "2", "3", 0.4)$y) > tol)
  expect_true(abs(approxfun_in_lists("1", "3", "3", 0.4) - dumb_approx("1", "3", "3", 0.4)$y) < tol)
  expect_true(abs(approxfun_in_lists("1", "3", "3", 0.4) - dumb_approx("1", "2", "3", 0.4)$y) > tol)
  expect_true(abs(approxfun_in_lists("1", "3", "1", 0.4) - dumb_approx("1", "3", "1", 0.4)$y) < tol)
  expect_true(abs(approxfun_in_lists("1", "3", "1", 0.4) - dumb_approx("1", "3", "2", 0.4)$y) > tol)
})

#library(rbenchmark)
#benchmark(replications=rep(10000),
#          dumb_approx("1", "2", "3", rlnorm(1)),
#          approxfun_in_lists("1", "2", "3", rlnorm(1)),
#          columns=c('test', 'elapsed', 'replications'))



