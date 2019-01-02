download.file("https://pbs.twimg.com/media/DO8Rf3yVoAAiPc-.jpg", "img.jpg")
img <- jpeg::readJPEG("img.jpg")
library(ggplot2)
ggplot(iris, aes(Sepal.Length, Sepal.Width, color = Species)) +
  annotation_custom(rasterGrob(img, 
                               width = unit(1,"npc"), 
                               height = unit(1,"npc"))) +
  geom_point()

unlink("img.jpg")
