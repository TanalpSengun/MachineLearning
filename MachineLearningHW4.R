
# Author : Tanalp Sengun ( 600150)
# Course : INTRODUCTION TO MACHINE LEARNING (Fall 2018)
# by Mehmet Gonen
#  the lab and the book from Ethem Alpaydin is used for the 
# homework. The lines are explained again in the report.


# read data into memory
data_set <- read.csv("hw04_data_set.csv", header = TRUE)

# get x and y values
x_train <- data_set$x[1:100]
y_train <- data_set$y[1:100]
x_test <- data_set$x[101:133]
y_test <- data_set$y[101:133]

#all of the test and train data
x_all <- data_set$x[1:133]
y_all <- data_set$y[1:133]

#setting the parameters for the bin width and the origin
bin_width <- 3
origin <- 0;

#the minimum and the maximum value is necessary to know how many rects there
minimum_value <- min(x_train)
maximum_value <- max(x_train)
data_interval <- seq(from = origin, to = 60, by = 0.01)
miny <- min(y_train)
maxy <- max(y_train)
N <- length(data_interval)

#for the color of the data points
point_color = matrix(1:133, nrow = 1, ncol = 133)
point_color[1:100] = "blue";
point_color[101:133] = "red";





# histogram estimator
left_borders <- seq(from = origin, to = maximum_value - bin_width, by = bin_width)
right_borders <- seq(from = origin + bin_width, to = maximum_value, by = bin_width)

p_head <- sapply(1:length(left_borders), function(b) {sum((left_borders[b] < x_train & x_train <= right_borders[b])*y_train) / sum(left_borders[b] < x_train & x_train <= right_borders[b])})

plot(x_all,   y_all, type = "p", pch = 19, col=point_color , 
     ylim = c(miny, maxy), xlim = c(origin, maximum_value),
     ylab = "y", xlab = "x", las = 1, main = sprintf("h = %g", bin_width))
for (b in 1:length(left_borders)) {
  lines(c(left_borders[b], right_borders[b]), c(p_head[b], p_head[b]), lwd = 2, col = "black")
  if (b < length(left_borders)) {
    lines(c(right_borders[b], right_borders[b]), c(p_head[b], p_head[b + 1]), lwd = 2, col = "black") 
  }
}




N_test= length(y_test);

#Calculate the root mean squared error (RMSE) of your regressogram for test data points.

y_predicted = p_head[(x_test/3)+1];

error_func= (sum((y_predicted - y_test)^2)/N_test)^(1/2);

sprintf("Regressogram => RMSE is  %g when h is %g" , error_func ,  bin_width);


#for the sum part : sum(((x - x_train) < (1 * bin_width))*y_train / ((x - x_train) < (1 * bin_width)))
#sum : sum(((x - 0.5* bin_width) < x_train & x_train <= (x +  0.5*bin_width))*y_train)  


# naive estimator

p_head2 <- sapply(data_interval, function(x) {sum((x - 0.5 * bin_width < x_train & x_train <= x+ 0.5 *bin_width)*y_train) / sum(x- 0.5 *bin_width < x_train & x_train <= x+0.5*bin_width)})


plot(x_all, y_all, type = "p", pch = 19, col=point_color , 
     ylim = c(miny, maxy), xlim = c(origin, maximum_value),
     ylab = "y", xlab = "x", las = 1, main = sprintf("h = %g", bin_width))

lines(data_interval, p_head2, type = "l", lwd = 2, col = "black")



#Calculate the root mean squared error (RMSE) of your running mean scorer for test data points.

y_predicted = p_head2[(x_test*100)];

error_func= (sum((y_predicted - y_test)^2)/N_test)^(1/2);

sprintf("Running Mean Smoother => RMSE is  %g when h is %g" , error_func ,  bin_width);









# kernel estimator

bin_width=1;
data_interval_kernel <- seq(from = origin, to = 60, by = 0.0001)
p_head3 <- sapply(data_interval_kernel, function(x) {sum((1 / sqrt(2 * pi)) * exp(-0.5 * (x - x_train)^2 / bin_width^2)*y_train)/(sum((1 / sqrt(2 * pi)) * exp(-0.5 * (x - x_train)^2 / bin_width^2)))}) 

plot(x_all,  y_all, type = "p", pch = 19, col=point_color , 
     ylim = c(miny, maxy), xlim = c(origin, maximum_value),
     ylab = "y", xlab = "x", las = 1, main = sprintf("h = %g", bin_width))
lines(data_interval_kernel, p_head3, type = "l", lwd = 2, col = "black")




#Calculate the root mean squared error (RMSE) of your kernel estimator for test data points.


y_predicted = p_head3[10000*x_test];

error_func3= (sum((y_predicted - y_test)^2)/N_test)^(1/2);


sprintf("Kernel Smoother => RMSE is  %g when h is %g" , error_func3 ,  bin_width);








