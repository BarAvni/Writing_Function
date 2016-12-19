
# generate some data
x=2

x = seq(from=1, to=100, by=2)
head(x)

#manipulate data - sin is a function in the base package of R
y = sin(x)

#plot - so is plot
plot(x,y)
# most functions have lots of optional arguments
plot(x,y, ylab="Sin of X", col="green", 
     type="l", lwd=2, main="Example")

# more complex data type
x = matrix(c(1,2,3,5,6,7),ncol=2)
x

y = sin(x)
y

#data frames - very useful - like a matrix but more flexible

comb = data.frame( a=seq(from=1,to=10, by=1),
                      firstn = rnorm(n=10, mean=20),  
                      secn = rnorm(n=10, mean=5))

plot(comb$a, comb$firstn)
density(comb$firstn)
summary(comb$firstn)
summary(comb$secn)
summary(comb$firstn+comb$secn)
plot(density(comb$firstn), type="l")
lines(density(comb$secn), col="red")
plot(density(comb$firstn), type="l", xlim=c(0,30))
lines(density(comb$secn), col="red")
lines(density(comb$secn+comb$firstn), col="blue", lty=2)
