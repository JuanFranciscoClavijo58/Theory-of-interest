##########################
#         EXERCISE       #
##########################

#########################################################################################
# An insurance company is required to pay an annuity annually in advance for 4 years.   #
# The first payment is $1.5 million and each subsequent payment is 3% greater than the  #
# previous one.                                                                         #
#########################################################################################
library(ggplot2)
increase <- 0.03
t <- 0:3
i <- 0.08
v <- 1 / (1 + i)

#######################################
# Visual explanation of the exercise #
####################################### 
df_timeline <- data.frame(
  x = 0:3,
  labels = 1:4,
  times = paste0("t = ", 0:3),
  payments = c("1.5", "CF_t1", "CF_t2", "CF_t3")
)
ggplot(df_timeline, aes(x = x, y = 0)) +
    geom_segment(aes(x = 0, xend = 3, y = 0, yend = 0), size = 1.2, color = "Black") +
  
  geom_segment(aes(x = x, xend = x, y = -0.1, yend = 0.1), color = "DarkGreen") +
  # Etiquetas superiores (1, 2, 3, 4)
  geom_text(aes(label = labels, y = 0.25), size = 5, fontface = "bold") +
  # Cash Flow
  geom_text(aes(label = payments, y = 0.5), color = c("chocolate", rep("seagreen", 3)), size = 5) +
  geom_text(aes(label = times, y = -0.25), size = 4.5) +
  annotate("text", x = 0.5:2.5, y = 0.1, label = "3%") +
  annotate("text", x = 1.5, y = 0.8, label = "i = 8% -> v = 1/1.08", size = 5, hjust = 0.5) +
  # Estética del gráfico
  ylim(-1, 1) +
  xlim(-0.5, 3.5) +
  theme_void() +
  theme(plot.margin = margin(20, 20, 20, 20))

##################################################################################
# (i)  Calculate, showing all working and using an effective rate of interest of #
#      8% p.a., the convexity of the annuity. [3]                                #
##################################################################################

CF_t = 1.5 * (1.03)^t
payments <- round(1.5 * (1 + increase)^t,4)
PV <- sum(v^t*payments)
PV

#Convexity
C <- sum(CF_t*(t)*(t+1)*v^(t+2)/PV)
#######################################################################################
# The company is considering investing in a single zero-coupon bond of suitable term  #
# to cover the present value of the annuity.                                          #
# (ii) Explain if it is possible to find a zero-coupon bond such that the fund would  #
#      be immunised against small changes in the rate of interest                     #
#######################################################################################

# No, it's not possible match the convexity of a multiple-flow.
# 
# In a zero-coupon bond, its duration is equal to its maturity, and its cash flow structure is simple,
# which means it cannot be fitted to the annuity





