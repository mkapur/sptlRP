require(gganimate); library(gifski)
require(ggplot2)
require(dplyr)
require(here)
require(reshape2)

#https://stackoverflow.com/questions/10130648/how-to-draw-random-numbers-in-a-circle-and-a-square
## n random points inscribed in circle of radius r
randp <- function(n = 1, r = 1) {
  if (n < 1 || r < 0) return(c())
  x <- rnorm(n)
  y <- rnorm(n)
  r <- r * sqrt(runif(n)/(x^2 + y^2))
  if (n == 1) U <- c(x, y)
  else        U <- cbind(r*x, r*y)
  return(U)
}

mort = exp(-0.2)
## ddep
ddep <- function(naa_py){
  nrec <- 0.15*naa_py
  return(nrec)
}

stay <- c(1,1)
## get population numbers (randp just provides position)
## here we are pretending no fishing etc
nyear = 100
naa <- matrix(NA, nrow = nyear, ncol = 3)
naa[1,] <- c(100,50,1)
#https://semba-blog.netlify.app/10/29/2018/animating-oceanographic-data-in-r-with-ggplot2-and-gganimate/
v.all <- rbind(data.frame(randp(  naa[1,1],0.75)) %>% mutate(Area = "A1"),
               data.frame(randp(  naa[1,2] ,0.5)) %>% mutate(Area = "A2")) %>%
  mutate(year = 1) 

for(y in 2:nyear){
  ## must use whole numbers
  naa[y,1] <- round((naa[y-1,1]*exp(-0.2)+ddep(naa[y-1,1]))*stay[1]+
    (naa[y-1,2]*exp(-0.2)+ddep(naa[y-1,2]))*(1-stay[2]))
  
  naa[y,2] <- round((naa[y-1,2]*exp(-0.2)+ddep(naa[y-1,2]))*stay[2]+
    (naa[y-1,1]*exp(-0.2)+ddep(naa[y-1,1]))*(1-stay[1]))
  
  naa[y,3] <- y
  # v.all <- data.frame(naa) %>%
  #   mutate(year = y) %>%
  #   bind_rows(.,v.all)
  
  v.all <- rbind(data.frame(randp(  naa[y,1],0.75)) %>% mutate(Area = "A1"),
                 data.frame(randp(  naa[y,2] ,0.5)) %>% mutate(Area = "A2")) %>%
    mutate(year = y) %>% 
    bind_rows(.,v.all)
}

plot(naa[,1]);points(naa[,2],col = 'blue')



myp <- ggplot(v.all, aes(x=X1, y = X2, col = Area)) +
  geom_point(size = 2) +
  ggthemes::theme_solid()+
  scale_color_manual(values = c('goldenrod','seagreen4')) +
  facet_wrap(~Area) +
  transition_reveal(year)

animate(myp, duration = 10, fps = 20, width = 6,
        height = 6, unit = 'in', res = 420, renderer = gifski_renderer())
