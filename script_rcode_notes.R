###############################################################################
# Scaling Analysis for Respiration Rates across the Yakima River Basin
# Code Notes-How to
################################################################################

# In this file I attempt to collect code snippets related with "how-to" questions
# I had while running the data analysis. 

# ggplot2 second edition
"https://ggplot2-book.org/index.html"

# Logaritmic grid plot with ggplot2
"https://stackoverflow.com/questions/28709331/logarithmic-grid-for-plot-with-ggplot2"

set.seed(5)
x <- rlnorm(1000, meanlog=3.5, sdlog=1)
y <- rlnorm(1000, meanlog=4.0, sdlog=1)
d <- data.frame(x, y)

breaks <- 10^(-10:10)
minor_breaks <- rep(1:9, 21)*(10^rep(-10:10, each=9))

d %>% 
  ggplot(aes(x, y)) +
  geom_point() +
  scale_x_log10(breaks = breaks, minor_breaks = minor_breaks) +
  scale_y_log10(breaks = breaks, minor_breaks = minor_breaks) +
  annotation_logticks() +
  coord_equal() +
  theme_bw()

# Creating a quasi-sequential color palette for discrete categories
# Source: https://medium.com/carbondesign/color-palettes-and-accessibility-features-for-data-visualization-7869f4874fca
# Source: https://www.ibm.com/design/language/color/
# Source: https://www.ibm.com/design/language/color/

# Great discrete color scale in blues:
#https://www.colorhexa.com/a6c8ff

# example
my_dcolors <- c("#edf5ff","#e5f6ff","#d0e2ff","#bae6ff","#a6c8ff","#82cfff","#78a9ff",
                "#33b1ff","#4589ff","#1192e8","#0f62fe","#0072c3","#0043ce","#00539a",
                "#002d9c","#003a6d","#001d6c","#012749","#001141","#061727")

# Creating a nested for loop

# create matrix with NA with 5 
# rows and 5 columns
matrixinp = matrix(data=NA, nrow=5, ncol=5)

# display matrix
print(matrixinp)

# fill the elements with some 
# 90 in a matrix
for(j in 1:5){
  for(i in 1:5){
    matrixinp[i,1] = 90
    matrixinp[i,2] = 91
    matrixinp[i,3] = 92
    matrixinp[i,4] = 93
    matrixinp[i,5] = 94
    }
}

# display filled matrix
print(matrixinp)


par(mfrow = c(3, 3))
for (alph in c(0.25, 0.75))
  image2D(volcano, alpha = alph, 
          main = paste("jet.col, alpha = ", alph))  
image2D(volcano, main = "jet.col")
image2D(volcano, col = jet2.col(100), main = "jet2.col")
image2D(volcano, col = gg.col(100), main = "gg.col")
image2D(volcano, col = gg2.col(100), main = "gg2.col")
image2D(volcano, col = rainbow(100), main = "rainbow")
image2D(volcano, col = terrain.colors(100), main = "terrain.colors")
image2D(volcano, col = ramp.col(c("blue", "yellow", "green", "red")),
        main = "ramp.col")  

par(mfrow=c(1,1))

set.seed(1)
x <- runif(1000, 0, 100)
z <- cut2(x, c(10,20,30))
table(z)
table(cut2(x, g=10))      # quantile groups
table(cut2(x, m=50))   