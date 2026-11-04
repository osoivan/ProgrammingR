distance_2d <- function(x1, y1, x2, y2) {
  # Calculate the differences squared
  delta_x_sq <- (x2 - x1)^2
  delta_y_sq <- (y2 - y1)^2
  
  # Calculate the distance
  distance <- sqrt(delta_x_sq + delta_y_sq)
  
  # Plot the two points and the connecting line
  plot(
    c(x1, x2), c(y1, y2),
    type = "b",
    pch = 19,
    col = "blue",
    xlab = "X coordinate",
    ylab = "Y coordinate",
    main = "Distance between two points",
    asp = 1
  )
  return(distance)
}

distance_2d(-2, -2, 2, 6)
