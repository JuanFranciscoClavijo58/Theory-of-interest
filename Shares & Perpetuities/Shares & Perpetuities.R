##########################
#         EXERCISE       #
##########################

##################################################################################################
# An investor buys 500 shares in Company A at a total price of £13,886. The shares are sold      #
# ex-dividend and the investor assumes that they will hold the shares in perpetuity.             #
# The company pays dividends every 6 months, and the latest dividend of £1.10 per share was paid #
# 6 months ago. The investor expects dividends to increase at a rate of 5.5% p.a. with increases #
# applied half-yearly. Future inflation is expected to be 3.5% p.a.                              #
# Calculate, showing all working, the expected annual effective real yield.                      #
##################################################################################################

# hypothesis of the exercise
total_price <- 13886
shares <- 500
price_per_share <- total_price / shares
price_per_share
last_dividend <- 1.10
g_pa <- 0.055      
inflation_pa <- 0.035

#######################################
# Visual explanation of the exercise  #
####################################### 
library(ggplot2)
periods <- 0:8
labels <- c("0", "", "1", "", "2", "", "3", "", "4")


ggplot() +
  
  geom_segment(aes(x = 0, xend = 8.5, y = 0, yend = 0), size = 1.2, color = "black") +
  geom_segment(aes(x = periods, xend = periods, y = -0.1, yend = 0.1), color = "black") +
  annotate("text", x = periods, y = -0.3, label = labels, fontface = "bold", size = 5) +
  
  geom_segment(aes(x = 0.5, xend = 0.5, y = -0.3, yend = -0.1), 
               arrow = arrow(length = unit(0.2, "cm")), color = "red") +
  annotate("text", x = 0.5, y = -0.35, label = "buy", color = "red", size = 4) +
  
  annotate("text", x = 0, y = -0.6, label = "£ 1.10", color = "black", size = 4) +
  annotate("text", x = seq(1, 8, 1), y = 0.3, label = "P", color = "red", fontface = "bold") +
  annotate("text", x = seq(0.5, 7.5, 1), y = 0.6, label = "g", color = "darkblue", size = 4) +

  labs(title = "Timeline: Perpetuity with Half-Yearly Increases",
       subtitle = "Growth (g): 5.5% p.a. | Inflation: 3.5% p.a.") +
  
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        plot.margin = margin(20, 20, 20, 20)) +
  coord_cartesian(ylim = c(-1, 1))

################################################################################

i_star <- last_dividend * (1 + g_pa)^(1/2) / price_per_share
i_star
annual_real_yield <- (((1 + i_star)^2 * (1 + g_pa)) / (1 + inflation_pa)) - 1
annual_real_yield
