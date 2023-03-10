#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++++++++++Function built-in fSelector Packages++++++++++++++++++++++++++++++
#+++++++++++Used for feature engineering+++++++++++++++++++++++++++++++++++++


### RANDOM FOREST
# classification and regression
# continous and discrete data
# NA deleted

#0. Miscelineaous functions

as.simple.formula <- function(attributes, class) {
  return(as.formula(paste(class, paste(attributes, sep = "", collapse = " + "), sep = " ~ ")))
}

get.data.frame.from.formula <- function(formula, data) {
  d = model.frame(formula, data, na.action = NULL)
  for(i in 1:dim(d)[2]) {
    if(is.factor(d[[i]]) || is.logical(d[[i]]) || is.character(d[[i]]))
      d[[i]] = factor(d[[i]])
  }
  return(d)
}

entropyHelper <- function(x, unit = "log") {
  return(entropy(table(x, useNA="always"), unit = unit))
}
#++++++++++++1. selction of varibale impo using rforest
random.forest.importance <- function(formula, data, importance.type = 1) {
  new_data = get.data.frame.from.formula(formula, data)

  # get rid of NAs
  no_na = rep(TRUE, dim(new_data)[1])
  for(i in 1:dim(new_data)[2]) {
    no_na = no_na & complete.cases(new_data[, i])
  }
  new_data = new_data[no_na, , drop=FALSE]

  forest = randomForest(formula, new_data,
                        ntree = 1000, keep.forest = FALSE, importance = TRUE)

  res = as.data.frame(importance(forest, type = importance.type))
  colnames(res)[1] = "attr_importance"
  return(res)
}
#++++++++++++++++2. Selection of k variables

cutoff.k <- function(attrs, k) {
  if(dim(attrs)[1] == 0)
    return(character(0))
  if(k < 1)
    stop("k too small")
  if(k > dim(attrs)[1])
    k = dim(attrs)[1]
  sorted_names = rownames(attrs)[do.call(order, c(attrs, decreasing = TRUE))]
  return(sorted_names[1:k])
}
#+++++++++++++++++3. Selection of k variables basing on their contribution to the target
cutoff.k.percent <- function(attrs, k) {
  if(dim(attrs)[1] == 0)
    return(character(0))
  if(k <= 0)
    stop("k too small")
  if(k > 1) {
    warning("Assumed k=1")
    k = 1
  }
  sorted_names = rownames(attrs)[do.call(order, c(attrs, decreasing = TRUE))]
  return(sorted_names[1:round((k * length(sorted_names)))])
}
