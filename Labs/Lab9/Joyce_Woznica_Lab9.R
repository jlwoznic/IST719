#
# Author: Joyce Woznica
# Class: IST 719
# Date: 3/9/2020
# Subject: Lab 9, Week 9
#
# Lab 9
#---------------- Package Load -------------------
library(igraph)
install.packages("rgl")
install.packages("XQuartz")
library(XQuartz)
library(rgl)
# check error and fix it

my.dir <- "/Users/joycewoznica/IST719/Labs/Lab9/"
load(paste0(my.dir, "ist719networkobject.rda"))

coords <- layout_with_kk(g, dim = 3)
rglplot(g, layout = coords)

l <- layout_with_kk(g)
V(g)$x <- l[,1]
V(g)$y <- l[,2]
V(g)$z <- V(g)$bet
rglplot(g)

V(g)$label <- ""

V(g)$x <- coords[,1]
V(g)$y <- coords[,2]
V(g)$z <- coords[,3]
rglplot(g)

par3d(windowRect = c(100, 100, 640, 640))
rgl.bringtotop()
rgl.bg(color = "black")
rgl.viewpoint(0,20)

E(g)$color <- "yellow"
E(g)$width <- .25
V(g)$label <- ""
rglplot(g)

E(g)$color <- "yellow"
E(g)$width <- 2.5
V(g)$label <- ""
rglplot(g)

E(g)$color <- "yellow"
E(g)$width <- .5
V(g)$label <- ""
rglplot(g)

#------------ Section 9.1 (step 3) ------------------ minute 7
out.dir <- "/Users/joycewoznica/IST719/Labs/Lab9/out/"
library(stringr)
install.packages("animation")
library(animation)
install.packages("XQuartz")
install.packages("rgl")
library(rgl)
library(igraph)
library(plotrix)

g
rglplot(g)

my.rgl.out <- paste0(out.dir, "Network3DVisualization.png")
rgl.snapshot(filename = my.rgl.out)

rglplot(g)
par3d(windowRect = c(100,100,500,500))
rgl.bringtotop()
rgl.bg(color = "black")
rgl.viewpoint (0, 0, zoom = .7)

max.loops <- 60
my.angle <- rescale(1:max.loops, c( -90, 90))
for (i in 1:max.loops)
{
  rgl.viewpoint(theta = -my.angle[i], phi = my.angle[i] - .7, zoom  = .75 - 1/(max.loops - 1.7))
  # theta is spinsing around y
  # phi is spinning around x
}

# try more loops
rgl.viewpoint (0, 0, zoom = .7)
max.loops <- 360
my.angle <- rescale(1:max.loops, c( -90, 90))
for (i in 1:max.loops)
{
  rgl.viewpoint(theta = -my.angle[i], phi = my.angle[i] - .7, zoom  = .75 - 1/(max.loops - 1.7))
  # theta is spinsing around y
  # phi is spinning around x
}

# try more loops
rgl.viewpoint (0, 0, zoom = .7)
max.loops <- 360 - 2
my.angle <- rescale(1:max.loops, c( -90, 90))
for (i in 1:max.loops)
{
  rgl.viewpoint(theta = -my.angle[i], phi = my.angle[i] - .7, zoom  = .75 - 1/(max.loops - 1.7))
  # theta is spinsing around y
  # phi is spinning around x
}

# ffmeg
images.out <- out.dir

rglplot(g)
par3d(windowRect = c(100,100,500,500))
rgl.bringtotop()
rgl.bg(color = "black")
my.angle <- rescale(1:max.loops, c(-180,180))
rgl.viewpoint(0,0, zoom = .7)

# can be viewed like a slide show when outputs are ready
for (i in 1:max.loops)
{
  rgl.viewpoint(theta = -my.angle[i], phi = my.angle[i] - .7, zoom  = .75 - 1/(max.loops - 1.7))
  # i <- i + 1
  # theta is spinsing around y
  # phi is spinning around x
  snapshot.fname <- paste0(images.out, "network", str_pad(i, width=4, side = "left", pad = "0"), ".png")
  rgl.snapshot(filename = my.rgl.out)
}

ani.options(interval = .1)
imgs <- list.files(images.out, pattern = "*.png")
saveGIF(
  {
    for (img in imgs)
    {
      im <- magick::image_read(paste0(images.out, img))
      print(im)
    }
  },
  movie.name = paste0(my.rgl.out, "ClassNetwork.gif"))

#--------------------------- 9.2 RGL in Depth -------------------------------------


















