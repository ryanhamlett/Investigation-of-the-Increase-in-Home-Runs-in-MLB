# Investigation-of-the-Increase-in-Home-Runs-in-MLB
An exploratory analysis of home run probabilities in 2016 and 2018 using nonparametric regression techniques

```

require(KernSmooth)
require(fields)
require(SemiPar)
require(mgcv)
require(plot3D)

##all data is with launch angle > 10, exit velocity > 75 mph because no HR occurred at less than 75 mph
##or less than 10 degrees

data.2015.hr = read.csv("C:/Users/ryanh/Desktop/255T datasets/2015 Before ASG HR.csv")
data.2015.hr = cbind(data.2015.hr, "HR" = as.integer(rep(1, length(data.2015.hr$launch_speed))))

data.2015.nohr = read.csv("C:/Users/ryanh/Desktop/255T datasets/2015 Before ASG No HR.csv")
data.2015.nohr = cbind(data.2015.nohr, "HR" = as.integer(rep(0, length(data.2015.nohr$launch_speed))))

data.2015 = rbind(data.2015.hr, data.2015.nohr)

data.2015 = data.2015[which(data.2015$launch_speed <= 110),]



                    

attach(data.2015)


h.speed.2015 = bw.nrd(launch_speed)
f.speed.2015 = bkde(launch_speed, bandwidth = h.speed.2015, kernel = "normal")
hist(launch_speed, freq = FALSE, breaks = seq(from = 70, to = 115, by = 2.5), xlab = "Exit Velocity", main = "2015")
lines(f.speed.2015$x, f.speed.2015$y, lwd = 2)


h.angle.2015 = bw.nrd(launch_angle)
f.angle.2015 = bkde(launch_angle, bandwidth = h.angle.2015, kernel = "normal")
hist(launch_angle, freq = FALSE, breaks = seq(from = 0, to = 100, by = 5), xlim = c(0,100), xlab = "Launch Angle", main = "2015")
lines(f.angle.2015$x, f.angle.2015$y, lwd = 2)

#do a bivariate kernel densiy estimate
plot(launch_speed, launch_angle)

f.2d.2015 = bkde2D(cbind(launch_speed, launch_angle), bandwidth = c(h.speed.2015, h.angle.2015))
contour(f.2d.2015$x1, f.2d.2015$x2, f.2d.2015$fhat)
persp(x = f.2d.2015$x1, y = f.2d.2015$x2, z= f.2d.2015$fhat, xlab = "Exit Velocity", ylab = "Launch Angle", zlab = "HR Probability", main = "2015", phi = 15, theta = -60, col = "light blue",ticktype = "detailed")
 

##2015


fit.spm.2015 = spm(HR ~ f(launch_speed) + f(launch_angle), family = "binomial")
#fit.gam.2015 = gam(HR ~ s(launch_speed) + s(launch_angle), family = "binomial")
plot(fit.spm.2015)

angle.grid = seq(from = 15, to = 50, length.out = 500)
speed.grid = seq(from = 90, to = 110, length.out = 500) 
grid = data.frame(cbind("launch_angle" = angle.grid, "launch_speed" = speed.grid))

heatmap.2015 = matrix(nrow = 500, ncol = 500)



for(i in 1:length(angle.grid)){
  
  pred = predict(fit.spm.2015, newdata = data.frame(cbind("launch_speed" = rep(grid[i,2], length(speed.grid)), "launch_angle" = grid[, 1])))
  heatmap.2015[i,] = 1/(1+exp(-(pred)))
  
}

image.plot(speed.grid, angle.grid, heatmap.2015, main = "2015 HR Probability", xlab = "Exit Velocity", ylab = "Launch Angle", col = rainbow(1000, start = 0, end = 5/6), breaks = seq(from = 0, to = 1, by = 0.001))
  

#predict(fit.spm, newdata = grid)
#do for loop at fixed angle for each speed grid point and then transform and stick into a matrix
#plot using image/persp

detach(data.2015)


##2017

data.2017.hr = read.csv("C:/Users/ryanh/Desktop/255T datasets/2017 Before ASG HR.csv")
data.2017.hr = cbind(data.2017.hr, "HR" = as.integer(rep(1, length(data.2017.hr$launch_speed))))

data.2017.nohr = read.csv("C:/Users/ryanh/Desktop/255T datasets/2017 Before ASG No HR.csv")
data.2017.nohr = cbind(data.2017.nohr, "HR" = as.integer(rep(0, length(data.2017.nohr$launch_speed))))

data.2017 = rbind(data.2017.hr, data.2017.nohr)
data.2017 = data.2017[which(data.2017$launch_speed <= 110), ]

attach(data.2017)

h.speed.2017 = bw.nrd(launch_speed)
f.speed.2017 = bkde(launch_speed, bandwidth = h.speed.2017, kernel = "normal")
hist(launch_speed, freq = FALSE, breaks = seq(from = 70, to = 115, by = 2.5), xlab = "Exit Velocity", main = "2017")
lines(f.speed.2017$x, f.speed.2017$y, lwd = 2)


h.angle.2017 = bw.nrd(launch_angle)
f.angle.2017 = bkde(launch_angle, bandwidth = h.angle.2017, kernel = "normal")
hist(launch_angle, freq = FALSE, breaks = seq(from = 0, to = 100, by = 5), xlim = c(0,100), xlab = "Launch Angle", main = "2017")
lines(f.angle.2017$x, f.angle.2017$y, lwd = 2)

#joint density
f.2d.2017 = bkde2D(cbind(launch_speed, launch_angle), bandwidth = c(h.speed.2017, h.angle.2017))
persp(f.2d.2017$x1, f.2d.2017$x2, f.2d.2017$fhat, xlab = "Exit Velocity", ylab = "Launch Angle", zlab = "HR Probability", main = "2017", phi = 15, theta = -60, col = "orange",ticktype = "detailed")


fit.spm.2017 = spm(HR ~ f(launch_speed) + f(launch_angle), family = "binomial")
#fit.gam.2017 = gam(HR ~ s(launch_speed) + s(launch_angle), family = "binomial")
plot(fit.gam.2017)

heatmap.2017 = matrix(nrow = 500, ncol = 500)

for(i in 1:length(angle.grid)){
  
  pred = predict(fit.spm.2017, newdata = data.frame(cbind("launch_speed" = rep(grid[i,2], length(speed.grid)), "launch_angle" = grid[, 1])))
  
  heatmap.2017[i,] = 1/(1+exp(-(pred)))
  
}

image.plot(speed.grid, angle.grid, heatmap.2017, main = "2017 HR Probability", xlab = "Exit Velocity (mph)", ylab = "Launch Angle (degrees)", col = rainbow(1000, start = 0, end = 5/6), breaks = seq(from = 0, to = 1, by = 0.001))

##difference

hmap = heatmap.2017 - heatmap.2015

image.plot(speed.grid, angle.grid, hmap, col = designer.colors(n=1000, col = c("purple", "white", "red")), breaks = seq(from = -0.25, to = 0.25, length.out = 1001), main = "Difference in HR Probability", xlab = "Exit Velocity", ylab = "Launch Angle")

```
