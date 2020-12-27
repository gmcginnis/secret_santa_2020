## Happy Holidays, J!
## Written 26 Dec 2020 by Gillian McGinnis
# Adapted partly from https://www.r-bloggers.com/2018/10/visual-art-with-pi-using-ggplot2-circlize/

library(tidyverse)
library(ggthemes)
library(wesanderson)
library(viridis)


df_raw <- data.frame(val = read_lines("http://www.geom.uiuc.edu/~huberty/math5337/groupe/digits.html"))

df <- df_raw %>%
  mutate(val = as.character(val)) %>%
  slice(-1:-12) %>% #excluding headers
  slice(1:1283) %>% #excluding footers
  separate_rows(val, sep="") %>%
  mutate(val = as.numeric(val)) %>%
  drop_na() %>%
  mutate(val = as.character(val),
         pos = row_number())

n_cols <- 25
n_rows <- 25
n <- n_cols*n_rows
viz_df <- df %>%
  filter(pos < n+1) %>%
  mutate(pos = pos-1) %>%
  mutate(x = pos %% n_cols,
         y = floor(pos/n_cols),
         val_num = as.numeric(val))


plot_discrete <- ggplot(viz_df, aes(x = x, y = y, color = val))+
  scale_y_reverse()+
  #geom_text(aes(label=val))+
  geom_point(size=5)+
  theme_solid()+
  theme(legend.position="none")

plot_scaled <- ggplot(viz_df, aes(x = x, y = y, color = val_num))+
  scale_y_reverse()+
  #geom_text(aes(label=val))+
  geom_point(size=5)+
  theme_solid()+
  theme(legend.position="none")

# Viridis palette plot. Options: https://cran.r-project.org/web/packages/viridis/vignettes/intro-to-viridis.html
viz_vir <- plot_scaled+
  scale_color_viridis(option="magma")

# Brewer palette plot. Options: https://www.r-graph-gallery.com/38-rcolorbrewers-palettes.html
viz_brew <- plot_discrete+
  scale_color_brewer(palette="Spectral")

# Wes palette plot. Options: https://github.com/karthik/wesanderson
pal_wes <- wes_palette("Zissou1", 10, type = "continuous")
viz_wes <- plot_discrete+
  scale_color_manual(values = pal_wes)

size <- 7

ggsave("viridis.png", plot = viz_vir, path="figures/", width=size,height=size)
ggsave("brewer.png", plot = viz_brew, path="figures/", width=size,height=size)
ggsave("wes.png", plot = viz_wes, path = "figures/", width=size,height=size)
