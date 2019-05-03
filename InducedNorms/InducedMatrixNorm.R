# plot unit circles

# initialize a plot
plot(c(-1, 1), c(-1, 1), type = "n")

# 1 norm
x1 = c(seq(-1, 0, length.out = 50),
       seq(-1, 0, length.out = 50),
       seq(0, 1, length.out = 50),
       seq(0, 1, length.out = 50))
y1 = c(seq(0, 1, length.out = 50),
       seq(0, -1, length.out = 50),
       seq(1, 0, length.out = 50),
       seq(-1, 0, length.out = 50))
plot(x1, y1)

# 2 norm
theta = seq(0, 2 * pi, length = 200)
x2 = radius * cos(theta)
y2 = radius * sin(theta)

theta1 = seq(0, pi/ 2, length = 50)
theta2 = seq(pi / 2, pi, length = 50)
theta3 = seq(pi, 1.5 * pi, length = 50)
theta4 = seq(1.5 * pi, 2 * pi, length = 50)

x2 = c(radius * cos(theta1),
       radius * cos(theta2),
       radius * cos(theta3),
       radius * cos(theta4))
y2 = c(radius * sin(theta1),
       radius * sin(theta2),
       radius * sin(theta3),
       radius * sin(theta4))
plot(x2, y2, type = "l")

# infinity norm
xinf = c(rep(-1, 50),
         seq(-1, 1, length.out = 50),
         seq(-1, 1, length.out = 50),
         rep(1, 50))
yinf = c(seq(-1, 1, length.out = 50),
         rep(-1, 50),
         rep(1, 50),
         seq(-1, 1, length.out = 50))
plot(xinf, yinf)


# create data set containing 
unit_balls = data.frame(x1 = x1, y1 = y1,
                        x2 = x2, y2 = y2,
                        xinf = xinf, yinf = yinf,
                        sep = rep(1:4, each = 50))

library(ggplot2)
width = 7

# one norm
ggplot(unit_balls) +
  geom_line(aes(x = x1, y = y1, group = sep)) +
  xlab("x") +
  ylab("y") +
  coord_fixed()
ggsave("unit_circle_1.pdf", width = width, height = 0.9 * width)

# two norm
ggplot(unit_balls) +
  geom_line(aes(x = x2, y = y2, group = sep)) +
  xlab("x") +
  ylab("y") + 
  coord_fixed()
ggsave("unit_circle_2.pdf", width = width, height = 0.9 * width)

# infinity norm
ggplot(unit_balls) +
  geom_line(aes(x = xinf, y = yinf, group = sep)) +
  xlab("x") +
  ylab("y") + 
  coord_fixed()
ggsave("unit_circle_inf.pdf", width = width, height = 0.9 * width)


# define a matrix A that project them into the same 2d space
A = matrix(c(1, 2, 3, 1), ncol = 2)
A_proj = function(x, y) {
  t(apply(rbind(x, y), 2, function (vec) {
    A %*% vec
  }))
}
# test
A_proj(x1, y1)


# create projection data
projections = data.frame(x1 = A_proj(x1, y1)[,1],
                         y1 = A_proj(x1, y1)[,2],
                         x2 = A_proj(x2, y2)[,1],
                         y2 = A_proj(x2, y2)[,2],
                         xinf = A_proj(xinf, yinf)[,1],
                         yinf = A_proj(xinf, yinf)[,2],
                         sep = rep(1:4, each = 50))
# one norm
ggplot(projections) +
  geom_line(aes(x = x1, y = y1, group = sep)) +
  xlab("x") +
  ylab("y") + 
  coord_fixed()
ggsave("A_unit_circle_1.pdf", width = width, height = 0.9 * width)

# include 1, 2, infinity norm on output
ggplot(projections) +
  geom_line(aes(x = x1, y = y1, group = sep)) +
  geom_line(data = unit_balls, 
            aes(x = 4 * x1, y = 4 * y1, group = sep),
            linetype = "dashed", color = "red") +
  geom_line(data = unit_balls,
            aes(x = sqrt(10) * x2, y = sqrt(10) * y2, group = sep),
            linetype = "twodash", color = "green") +
  geom_line(data = unit_balls, 
            aes(x = 3 * xinf, y = 3 * yinf, group = sep),
            linetype = "dotted", color = "blue") +
  xlab("x") +
  ylab("y") + 
  coord_fixed()
ggsave("norms_A_unit_circle_1.pdf", width = width, height = 0.9 * width)


# two norm
ggplot(projections) +
  geom_path(aes(x = x2, y = y2)) +
  xlab("x") +
  ylab("y") + 
  coord_fixed()
ggsave("A_unit_circle_2.pdf", width = width, height = 0.9 * width)

# include 1, 2, infinity norm on output
sigma_max = sqrt(eigen(A %*% t(A))$values[1])

ggplot(projections) +
  geom_path(aes(x = x2, y = y2, group = sep)) +
  # geom_line(data = unit_balls,
  #           aes(x = 4 * x1, y = 4 * y1, group = sep),
  #           linetype = "dashed", color = "red") +
  geom_line(data = unit_balls,
            aes(x = sigma_max * x2, y = sigma_max * y2, group = sep),
            linetype = "twodash", color = "green") +
  geom_line(data = unit_balls,
            aes(x = sqrt(10) * xinf, y = sqrt(10) * yinf, group = sep),
            linetype = "dotted", color = "blue") +
  xlab("x") +
  ylab("y") + 
  coord_fixed()
ggsave("norms_A_unit_circle_2.pdf", width = width, height = 0.9 * width)


# infinity norm
ggplot(projections) +
  geom_line(aes(x = xinf, y = yinf, group = sep)) +
  xlab("x") +
  ylab("y") + 
  coord_fixed()
ggsave("A_unit_circle_inf.pdf", width = width, height = 0.9 * width)

ggplot(projections) +
  geom_line(aes(x = xinf, y = yinf, group = sep)) +
  xlab("x") +
  ylab("y") +
  # geom_line(data = unit_balls,
  #           aes(x = 4 * x1, y = 4 * y1, group = sep),
  #           linetype = "dashed", color = "red") +
  # geom_line(data = unit_balls,
  #           aes(x = sqrt(10) * x2, y = sqrt(10) * y2, group = sep),
  #           linetype = "twodash", color = "green") +
  geom_line(data = unit_balls,
            aes(x = 4 * xinf, y = 4 * yinf, group = sep),
            linetype = "dotted", color = "blue") +
  xlab("x") +
  ylab("y") + 
  coord_fixed()
ggsave("norms_A_unit_circle_inf.pdf", width = width, height = 0.9 * width)


