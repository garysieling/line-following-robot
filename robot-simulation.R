move <- function(pos, command) {
  loc <- c(pos$point[1], pos$point[2]) +
         c(command$speed * cos(command$angle),
           command$speed * sin(command$angle));
  list(point = c(loc[1], loc[2]),
       angle = command$angle);
}

angle <- function(goal, p) {
  atan( (goal[2] - p[2]) / (goal[1] - p[1] ) )
}

angle2 <- function(theta) {
  atan2(sin(theta), cos(theta));
}

distance <- function(a, b) {
  sqrt(sum((a - b) * (a - b)))
}

simulate <- function(waypoints, start, angle, f, iter) {
    plot(waypoints, type="o", col="#FD7871");
    command <- list(speed = 0,
                    angle = 0);
    waypoint_idx <- 1;
    position <- list(point = start,
                     angle = angle);
    for (i in 1:iter)
    {
        goal <- c(waypoint_idx, waypoints[waypoint_idx]);
        command <- f(position, goal, command);
        points(x = position$point[1], y = position$point[2],
               type = "o", col = "#FFD8AB");
        position <- move(position, command);
        if (distance(position$point, goal) < 0.1) {
            waypoint_idx <- waypoint_idx + 1;
            command <- list(speed = 0, angle = 0, err = 0);
        }
    }
    for(i in 1:length(waypoints)) {
      points(x = i, y = waypoints[i], type = "o", col = "#FD7871")
    }
}

controller <- function(position, goal, last) {
  v <- -.05;
  err <- angle(position$point, goal) - position$angle;
  neterr <- last$err + err;
  list(speed = v,
       angle = 5 * angle2(position$angle + err),
       err = neterr);
}
