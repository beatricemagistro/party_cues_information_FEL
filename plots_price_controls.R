### Party cues or Policy Information? The differential influence of financial and economic literacy 
### on economic policy preferences
### Beatrice Magistro
### 26 September 2021
# Online Appendix

### This code does the following:
### 1. Plot Figures A1, A2 and A3

#### plots


demand <- function(q) 
  {(-0.01*q + 8)}
supply <- function(q) 
  {(0.01*q)}

# Set a range of quantity: 
x <- 0:800

# Equilibrium quantity: 
q <- uniroot(function(x) demand(x) - supply(x), range(x))$root

# Equilibrium price: 
p <- supply(q)

z <- seq(0, 400, 0.01)

chart <- ggplot() +
  stat_function(aes(x, color = "Demand"), fun = demand) +
  stat_function(aes(x, color = "Supply"), fun = supply) +
  scale_color_manual(values=c("#F8766D", "#619CFF"))
chart

png("before.png", width = 200, height = 150, units='mm',  res = 300)
chart + geom_ribbon(aes(x = z, ymin = supply(z), ymax = p,
                fill = "Producer \nsurplus"), alpha = 0.25) +
  geom_ribbon(aes(x = z, ymin = p, ymax = demand(z),
                  fill = "Consumer \nsurplus"), alpha = 0.25) +
  geom_segment(data = data.frame(x = 400, y=4), 
               aes(x = x, y = 0, xend = x, yend = y), lty = "dotted") +
  geom_segment(data = data.frame(x = 400, y=4), 
               aes(x = 0, y = y, xend = x, yend = y), lty = "dotted") +
  labs(x = "Quantity", y = "Price",
       color = NULL, fill = NULL)+
  theme_bw() +
  theme(
    legend.position = c(0.6, 0.97), 
    legend.justification = c(1, 1),
    legend.spacing = unit(0, "cm"), 
    legend.margin = margin(0, 0, 0, 0, "cm")) +
  scale_fill_manual(values=c("#F8766D", "#619CFF"))
  #scale_colour_grey()+
  #scale_fill_grey()
dev.off()


## after price support with gov

z <- seq(0, 200, 0.01)

w <- seq(0, 600, 0.01)

chart <- ggplot() +
  stat_function(aes(x, color = "Demand"), fun = demand) +
  stat_function(aes(x, color = "Supply"), fun = supply) +
  scale_color_manual(values=c("red", "#619CFF"))
  #scale_colour_grey()
chart

png("after.png", width = 200, height = 150, units='mm',  res = 300)
chart + geom_ribbon(aes(x = w, ymin = supply(w), ymax = 6,
                        fill = "Producer \nsurplus"), alpha = 0.25) +
  geom_ribbon(aes(x = z, ymin = 6, ymax = demand(z),
                  fill = "Consumer \nsurplus"), alpha = 0.25) +
  geom_ribbon(aes(x = c(200:600), ymin = 0, ymax = 6, fill = "Cost to \nGovernment"),
              alpha = 0.25) +
  geom_segment(data = data.frame(x = 400, y=4), 
               aes(x = x, y = 0, xend = x, yend = y), lty = "dotted") +
  geom_segment(data = data.frame(x = 400, y=4), 
               aes(x = 0, y = y, xend = x, yend = y), lty = "dotted") +
  geom_segment(data = data.frame(x = 600, y=6), 
               aes(x = x, y = 0, xend = x, yend = y), lty = "dotted") +
  geom_segment(data = data.frame(x = 600, y=6), 
               aes(x = 0, y = y, xend = x, yend = y), lty = "dotted") +
  geom_segment(data = data.frame(x = 600, y=6), 
               aes(x = 200, y = 0, xend = 200, yend = y), lty = "dotted") +
  labs(x = "Quantity", y = "Price",
       color = NULL, fill = NULL) +
  theme_bw() +
  theme(
    legend.position = c(0.975, 0.74), 
    legend.justification = c(1, 1),
    legend.spacing = unit(0, "cm"), 
    legend.margin = margin(0, 0, 0, 0, "cm")) 
  #scale_fill_grey(start = 0.7, end = 0)
dev.off()


## after price floor no gov

z <- seq(0, 200, 0.01)

w <- seq(0, 200, 0.01)

d <- seq(200, 400, 0.01)

chart <- ggplot() +
  stat_function(aes(x, color = "Demand"), fun = demand) +
  stat_function(aes(x, color = "Supply"), fun = supply) +
  scale_color_manual(values=c("red", "#619CFF"))
chart

png("after_nogov.png", width = 200, height = 150, units='mm',  res = 300)
chart + geom_ribbon(aes(x = w, ymin = supply(w), ymax = 6,
                        fill = "Producer \nsurplus"), alpha = 0.25) +
  geom_ribbon(aes(x = z, ymin = 6, ymax = demand(z),
                  fill = "Consumer \nsurplus"), alpha = 0.25) +
  geom_ribbon(aes(x = d, ymin = supply(d), ymax = demand(d), fill = "DWL"),
              alpha = 0.25) +
  geom_segment(data = data.frame(x = 200, y=6), 
               aes(x = x, y = 0, xend = x, yend = y), lty = "dotted") +
  geom_segment(data = data.frame(x = 200, y=6), 
               aes(x = 0, y = y, xend = x, yend = y), lty = "dotted") +
  geom_segment(data = data.frame(x = 200, y=6), 
               aes(x = 200, y = 0, xend = 200, yend = y), lty = "dotted") +
  labs(x = "Quantity", y = "Price",
       color = NULL, fill = NULL) +
  theme_bw() +
  theme(
    legend.position = c(0.975, 0.74), 
    legend.justification = c(1, 1),
    legend.spacing = unit(0, "cm"), 
    legend.margin = margin(0, 0, 0, 0, "cm")) +
  scale_fill_manual(values=c("#F8766D", "#C77CFF","#619CFF"))
dev.off()

## after price support with no gov

z <- seq(0, 200, 0.01)

w <- seq(0, 600, 0.01)

chart <- ggplot() +
  stat_function(aes(x, color = "Demand"), fun = demand) +
  stat_function(aes(x, color = "Supply"), fun = supply) +
  scale_color_manual(values=c("red", "#619CFF"))
#scale_colour_grey()
chart

png("after_supply.png", width = 200, height = 150, units='mm',  res = 300)
chart + geom_ribbon(aes(x = w, ymin = supply(w), ymax = 6,
                        fill = "Producer \nsurplus"), alpha = 0.25) +
  geom_ribbon(aes(x = z, ymin = 6, ymax = demand(z),
                  fill = "Consumer \nsurplus"), alpha = 0.25) +
  geom_ribbon(aes(x = c(200:600), ymin = 0, ymax = 6, fill = "Cost to \nProducers"),
              alpha = 0.25) +
  geom_segment(data = data.frame(x = 400, y=4), 
               aes(x = x, y = 0, xend = x, yend = y), lty = "dotted") +
  geom_segment(data = data.frame(x = 400, y=4), 
               aes(x = 0, y = y, xend = x, yend = y), lty = "dotted") +
  geom_segment(data = data.frame(x = 600, y=6), 
               aes(x = x, y = 0, xend = x, yend = y), lty = "dotted") +
  geom_segment(data = data.frame(x = 600, y=6), 
               aes(x = 0, y = y, xend = x, yend = y), lty = "dotted") +
  geom_segment(data = data.frame(x = 600, y=6), 
               aes(x = 200, y = 0, xend = 200, yend = y), lty = "dotted") +
  labs(x = "Quantity", y = "Price",
       color = NULL, fill = NULL) +
  theme_bw() +
  theme(
    legend.position = c(0.975, 0.74), 
    legend.justification = c(1, 1),
    legend.spacing = unit(0, "cm"), 
    legend.margin = margin(0, 0, 0, 0, "cm")) +
  scale_fill_manual(values=c("#F8766D", "#C49A00","#619CFF"))
#scale_fill_grey(start = 0.7, end = 0)
dev.off()

