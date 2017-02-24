## Load required packages
require(plotly) # For interactive 3D graphics
require(numDeriv) # For Jacobian matrix of partial derivatives

library(ggplot2)
library(data.table)


## Writes a function to get coordinates for a 3D wireframe graphic
## x = Minimum and Maximum x-values
## y = Minimum and Maximum y-values
## func = Function to plot
## pixels = Graph resolution
getSurface = function(x = c(-1,1),
                      y = c(-1,1),
                      func = function(x) x[1]^2 + x[2]^2,
                      pixels = 20) {
  ## Create grid of x and y values
  grid.x <- x[1] + (0:pixels)*(x[2]-x[1])/pixels
  grid.y <- y[1] + (0:pixels)*(y[2]-y[1])/pixels
  grid <- expand.grid(grid.x,grid.y)
  names(grid) <- c("x","y")
  ## Apply the function to each value of x and y (x[1] and x[2])
  grid$z = apply(grid,1,func)
  grid
}

data <- getSurface(x = c(0,100),
                   y = c(0,100),
                   func = function(x) (x[1]^.1)*(x[2]^.1),
                   pixels = 60)

p <- plot_ly(showscale = FALSE, width = 600, height = 600) %>%
  add_mesh(x = data.pos$x, y = data.pos$y, z = data.pos$z, type = 'mesh3d') %>%
  add_mesh(x = data.neg$x, y = data.neg$y, z = data.neg$z, type = 'mesh3d')
p


L1 = function(L2, L=200, rho=1, theta2=.3){
  theta1 = 1-theta2
  L1 = (-(theta2*(L2^rho) - (L^rho))/theta1)^(1/rho)
  return(L1)
}

L1(234)

curve(L1, from=0, to=200)


### migration flows gender
setwd("~/Dropbox/upf_economics_migration/2017/analysis/gender")

d = fread('wb_bmf.csv')

os = d[, list(y1960 = sum(n1960, na.rm=TRUE), y1970 = sum(n1970, na.rm=TRUE), y1980 = sum(n1980, na.rm=TRUE), y1990 = sum(n1990, na.rm=TRUE), y2000 = sum(n2000, na.rm=TRUE)), by=list(o, oc, g, gc)]

osl = melt(os, id.vars=c('o', 'oc', 'g', 'gc'))

names(osl)[5] = 'year'
names(osl)[3] = 'gender'

osl$year = as.integer(substr(osl$year, 2, 5))

unique(paste(osl$oc,osl$o))

D = osl[oc %in% c("ESP", "MEX", "MAR", "POL", "PHL", "ROM")]

p<-ggplot(data=D, aes(x=year, y=value, group=gender)) +
  geom_line(aes(color=gender)) + facet_wrap(~o, ncol=2)

p


ds = d[, list(y1960 = sum(n1960, na.rm=TRUE), y1970 = sum(n1970, na.rm=TRUE), y1980 = sum(n1980, na.rm=TRUE), y1990 = sum(n1990, na.rm=TRUE), y2000 = sum(n2000, na.rm=TRUE)), by=list(d, dc, g, gc)]


dsl = melt(ds, id.vars=c('d', 'dc', 'g', 'gc'))

names(dsl)[5] = 'year'
names(dsl)[3] = 'gender'

dsl$year = as.integer(substr(dsl$year, 2, 5))

unique(paste(dsl$dc,dsl$d))

D = dsl[dc %in% c("ESP", "DEU", "USA", "FRA")]

p<-ggplot(data=D, aes(x=year, y=value, group=gender)) +
  geom_line(aes(color=gender)) + facet_wrap(~d, ncol=2)

p

unique(paste(dsl$dc,dsl$d))




odsl = melt(d, id.vars=c('o', 'oc', 'd', 'dc', 'g', 'gc'))

names(odsl)[7] = 'year'
names(odsl)[5] = 'gender'

odsl$year = as.integer(substr(odsl$year, 2, 5))

D = odsl[dc %in% c("USA") & oc %in% c("MEX", "PHL")]

p<-ggplot(data=D, aes(x=year, y=value, group=gender)) +
  geom_line(aes(color=gender)) + facet_wrap(~o, ncol=2)

p

D = odsl[dc %in% c("DEU") & oc %in% c("TUR", "ESP")]

p<-ggplot(data=D, aes(x=year, y=value, group=gender)) +
  geom_line(aes(color=gender)) + facet_wrap(~o, ncol=2)

p
