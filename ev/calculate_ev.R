library(ggplot2)
library(scales)

# X is a random variable representing two outcomes
# p_price is the implied odds we get from the book
# p_true is the true odds of that event happening
#
# So,
#  X = 1 / p_price, with probability p_true
#  X = -1, with probability 1 - p_true
#
# That means the Expected Value of X, or E(X), is the following
#
# E(X) = p_true * ((1 / p_price) - 1) - (1 - p_true), or
#      = p_true * ((1 - p_price) / p_price) - (1 - p_true)
#
# Ex of this:
# p_price = 0.5238095, or -110 odds
# p_true = 0.54
#
# E(X) = (0.54) * ((1 - 0.5238) / 0.5238) - (1 - 0.54)
#      = 0.4909 - 0.46
#      = 0.0309, or 3.09% return on each dollar
#
# Note that the variance of X, or Var(X), is
#
# Var(X) = E((X - E(X))^2)
#        = E(X^2) - E(X)^2
#        = [p_true * ((1 - p_price) / p_price))^2 + (1 - p_true)] -
#          (p_true * ((1 - p_price) / p_price) - (1 - p_true))^2
#
# Now, let's scale this problem a different way.
#
# Suppose we know we have a 1% _probability_ advantage with our model.
# In otherwords, p_true = p_price + 0.01.
#
# Does it make a difference p_price we go after?
# Put more simply, is it better to then bet on dogs with this model or favorites,
# _purely_ in terms of expected return?

exp_value_X = function(p_price, p_true) {
  return(p_true * ((1 - p_price) / p_price) - (1 - p_true))
}

var_X = function(p_price, p_true) {
  term1 = p_true * ((1 - p_price) / p_price) + (1 - p_true)
  term2 = (p_true * ((1 - p_price) / p_price) - (1 - p_true))^2
  
  return(term1 - term2)
}

exp_value_X(110/210, 0.54)
# [1] 0.03090909

var_X(110/210, 0.54)
# [1] 0.9499537

p_trues = seq(0.1, 0.9, by = 0.01)
p_prices = p_trues - 0.01

p_table = data.table(p_true = p_trues, p_price = p_prices)
p_table[, ev := exp_value_X(p_price, p_true)]
p_table[, std_dev := sqrt(var_X(p_price, p_true))]

ggplot(p_table, aes(y = ev, x = p_true)) +
  geom_line() + 
  scale_y_continuous(labels = percent_format()) +
  scale_x_continuous(labels = percent_format())

ggplot(p_table, aes(y = std_dev, x = p_true)) +
  geom_line() + 
  scale_x_continuous(labels = percent_format())
