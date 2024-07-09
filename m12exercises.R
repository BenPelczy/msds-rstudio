library(vctrs)
library(tidyverse)
head(mpg)

mpg %>% 
  ggplot()

mpg %>% 
  ggplot() + 
  aes(x = cyl, y = hwy) +
  geom_point()

mpg %>% 
  ggplot() + 
  aes(x = drv, y = class) +
  geom_point()

x <- seq(-6, 6*pi, length.out = 100)
dat <- data.frame(y = sin(x)/x, x)
dat %>% 
  ggplot() + 
  aes(x = x, y = y) +
  geom_line()

cars %>%
  ggplot() + 
  aes(x = speed, y = dist) +
  geom_point()

cars %>%
  ggplot() + 
  aes(x = speed, y = dist, color = dist > 80) +
  geom_point()

cars %>%
  ggplot() + 
  aes(x = speed, y = dist, color = dist > 80) +
  geom_point() +
  scale_color_manual(values = c("blue", "red"))

cars %>%
  ggplot() + 
  aes(x = speed, y = dist, color = dist > 80) +
  geom_point() +
  scale_color_manual(values = c("blue", "red")) + 
  geom_smooth(method = "loess")

cars %>%
  ggplot() + 
  aes(x = speed) +
  geom_histogram(bins = 10)

cars %>%
  ggplot() + 
  aes(x = dist) +
  geom_histogram(bins = 10)

mpg %>% 
  ggplot() + 
  aes(x = hwy, y = cty) +
  geom_point(size = 1) +
  facet_grid(drv ~ cyl)

iris %>%
  ggplot() +
  aes(x = Sepal.Length, y = Sepal.Width, color = Species, shape = Species) +
  geom_point() +
  geom_density2d() +
  ggtitle("IRIS") +
  theme_light()
