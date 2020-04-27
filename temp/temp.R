

library(r2d3)
r2d3(data = jsonlite::read_json("bullets.json"), d3_version = 3, container = "div",
     dependencies = c("d3_bullet.js"), script = "bullets.js")

r2d3(data = jsonlite::read_json("flare.json"), d3_version = 4, script = "circlepacking.js")

r2d3(data = read.csv("flare.csv"), d3_version = 4, script = "dendogram.js")


# search packages

install.packages("packagefinder")
packagefinder::findPackage("metabolomic")
