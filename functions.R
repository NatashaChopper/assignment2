# Sum values in a column of a data frame.
#
# ARGUMENTS:
# d: a data frame or tibble
# var: the name of a column of d, provided as a string
#
# RETURN VALUE:
# if the specified column exists and contains numbers, returns the sum of
# all values in the column; otherwise, returns NULL
sum_column <- function(d, var) {
  # Set a default value to return
  result <- NULL
  # Remember, as we showed in class: there are two ways
  # to access a column, and this is one; you should try
  # and figure out why the other way won't work here,
  # but that's not part of the homework
  if (is.numeric(d[[var]])) { # This tests to see whether the column exists in
    # d; again, you should try and find a way to test
    # this out for yourself to verify that it's true,
    # but that's not part of the homework
    # YOUR CODE HERE: if x contains numbers, set the variable
    # result to be the sum of the values in x
    result <- sum(d[[var]])
    }
    return(result)
}




# Sum values in a vector.
#
# ARGUMENTS:
# x: a vector
#
# RETURN VALUE:
# if the vector contains numbers, returns the sum of
# all values; otherwise, returns NULL
#
# [YOUR FUNCTION HERE]
my_sum <- function(x) {
  result <- NULL
  if (is.numeric(x)) {
    result <- (sum(x)) 
    }
return(result)
}

#x: a vector
#k: a number
sum_divided_by <- function(x, k) {
  result <- NULL
  if (is.numeric(x)&&is.numeric(k)) {
    result <- (sum(x)) / (k)
    }
   return(result)
}

my_mean <- function(x) {
  result <- NULL
  if (is.numeric(x)) {
    result <- sum_divided_by(x, length(x))
  }
  return(result)
}


# Return a violin plot.
#
# ARGUMENTS:
# d: a data frame or tibble
# var: the name of a column of d containing the dependent variable, provided as a
# string
# grouping_var: the name of a column of d containing a grouping variable,
# provided as a string
#
# RETURN VALUE:
# A ggplot plot object containing a violin plot, grouped by the values
# of the grouping variable.
#
grouped_violin_plot <- function(d, var, grouping_var) {
  # Create the base ggplot object
  p <- ggplot2::ggplot(d, ggplot2::aes_string(y=var,
                                              x=grouping_var,
                                              fill=grouping_var)) +
       ggplot2::geom_violin(ggplot2::aes_string(y=var,
                                                x=grouping_var,
                                                fill=grouping_var)) +
       ggplot2::ggtitle("Iris data") + 
       ggplot2::scale_fill_brewer(palette="YlGnBu")
  return(p)
}

# Difference in the medians between two groups.
#
# ARGUMENTS:
# d: a data frame or tibble
# var: the name of a column of d containing the dependent variable, provided as a string
# grouping_var: the name of a column of d containing a grouping variable, provided as a string
# group1: the value of grouping_var that corresponds to the first group
# group2: the value of grouping_var that corresponds to the second group
#
# RETURN VALUE:
# The median value of var for the first group, minus the median value of var for the second
# group.
#
difference_in_medians <- function(d, var, grouping_var, group1, group2) {
  d_1 <- dplyr::filter(d, get(grouping_var) == group1)
  d_2 <- dplyr::filter(d, get(grouping_var) == group2)
  result <- median(d_1$var) - median(d_2$var)
  # YOUR CODE HERE: assign the difference in the medians to to the variable 'result'
  return(result)
}

# Randomize the order of a column.
#
# ARGUMENTS:
# d: a data frame or tibble
# var: the name of a column of d containing the variable to randomize,
# provided as a string
#
# RETURN VALUE:
# A data frame or tibble exactly the same as d, except with the order of
# var permuted randomly.
#
randomize <- function(d, var) {
  d[[var]] <- randomize(d[[var]])
    return(d)
}





# Perform a permutation test for two groups.
#
# ARGUMENTS:
# d: a data frame or tibble
# var: the name of the column in d on which the test statistic will be calculated,
# provided as a string
# grouping_var: the name of the column in d which gives the grouping
# group1: the value of grouping_var corresponding to the first group
# group2: the value of grouping_var corresponding to the second group
# statistic: a function yielding a test statistic, which takes as input
# a data frame, the name of a variable on which to calculate the
# test statistic, the name of a grouping variable, the value of
# the grouping variable corresponding to the first group, and
# the value of the grouping variable corresponding to the second
# group
# n_samples: the number of permutation samples to draw (default: 9999)
#
# RETURN VALUE:
#
# A list containing two elements:
#
# - observed: the value of statistic() in d
# - permuted: a vector containing the values of statistic() under n_samples
# permutations
#
permutation_twogroups <- function(d, var, grouping_var, group1, group2, statistic,
                                  n_samples=9999) {
  observed_statistic <- statistic(d, var, grouping_var, group1, group2)
  permutation_statistics <- rep(0, n_samples)
  for (i in 1:n_samples) {
    # YOUR CODE HERE: use randomize(...) to create a permutation and then
    # fill in the vector permutation_statistics with the
    # value of statistic(...) for this new permutation
  }
  result <- list(observed=observed_statistic,
                 permuted=permutation_statistics)
  return(result)
}

