scalep <- function(factors, divisors, constant = 1, graph = FALSE) {

  # Let user know these are specified
  if (is.null(divisors)) { print('Divisors must be specified') }
  if (is.null(factors)) { print('factors must be specified') }


  # Check divisors and factors are the same length
  if (length(divisors) != length(factors)) {
    warning('Length of divisors argument is not equal to length of factors argument')
  }

  # Divide and multiply by optional scalar
  proportions <- constant*factors/divisors

  # If graph = True, print the graph using either ggplot2 or the hist function
  if (requireNamespace("ggplot2", quietly = TRUE) & graph) {
    print(ggplot2::qplot(proportions, geom='histogram'))
  } else if (graph) {
    print(graphics::hist(proportions))
  }

  # Return the result
  return(proportions)
}