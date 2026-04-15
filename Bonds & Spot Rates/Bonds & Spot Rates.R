##########################
#         EXERCISE       #
##########################

################################################################################################
# Three bonds, A, B and C, each pay coupons at 4% annually in arrears and are redeemed at 103%.#
# The outstanding terms of the bonds are exactly 1, 2 and 3 years, respectively.               #
# The prices of the bonds per £100 nominal are £104, £105 and £106, respectively.              #
################################################################################################

library(lifecontingencies)
library(ggplot2)

#######################################
# Visual explanation of the exercise #
####################################### 

timeline <- data.frame(
  x = 0:3,
  labels = c("","104","105","106"),
  times = paste0("t = ", 0:3),
  payments = c("12000", "12000", "12000", "")
)

ggplot(timeline, aes(x = x, y = 0)) +
  geom_segment(aes(x = 0, xend = 3, y = 0, yend = 0), size = 1.2, color = "Black") +
  geom_segment(aes(x = x, xend = x, y = -0.1, yend = 0.1), color = "DarkGreen") +
  geom_text(aes(label = labels, y = 0.25), size = 5, fontface = "bold") +
  # Cash Flow
  geom_text(aes(label = payments, y = 0.5), color = c(rep("seagreen", 4)), size = 5) +
  geom_text(aes(label = times, y = -0.25), size = 4.5) +
  annotate("text", x = 0.5:2.5, y = 0.1, label = c("","f_1,2","f_2,3"), color = "red") +
  annotate("text", x = 1.5, y = 0.8, label = "Time line", size = 5, hjust = 0.5) +
  annotate("label",x = 3.5, y = 0.85,label = "Redemption = 103\nNominal = 100\ni = 4%", 
    hjust = 1, size = 4, fontface = "italic"
  )+
  
  ylim(-1, 1) +
  xlim(-0.5, 3.5) +
  theme_void() +
  theme(plot.margin = margin(20, 20, 20, 20))

##############################################################
# (i) Using the information above and showing all working:   #
# (a) Determine all possible discrete spot rates.            #
# (b) Determine all possible discrete future rates.          #
##############################################################

#Spot Rates
S1 <- (107/104)-1

payments1 <- c(4, 107)

S2 <- uniroot(function(x) sum(payments1 / (1 + c(S1, x))^(1:2)) - 105, 
              interval = c(0, 1))$root

payments2 <- c(4,4,107)

S3 <- uniroot(function(x) sum(payments2/(1+ c(S1,S2,x))^(1:3))-106,
              interval = c(0,1))$root


S <- c(S1, S2, S3)

forwards <- numeric(2)

for (i in 2:3) { 
           #   (1+Si)^i/(1+Si)^i-1
           f_actual <- ((1+S[i])^i/(1+S[i-1])^(i-1))-1
           forwards[i-1] <- f_actual
}
names(forwards) <- c("f12","f23")
forwards

# ##############################################################################################################
# A bank offers a 3-year regular savings plan and adds interest in line with the rates calculated in part (i). #
# An individual invests £12,000 at the beginning of each of the 3 years.                                       #  
# (ii) Calculate, showing all working, the total accumulated value of the investments at the end of year three.#
################################################################################################################

inv1 <- 12000 * (1 + S3)^3
inv2 <- 12000 * (1 + forwards["f12"]) * (1 + forwards["f23"])
inv3 <- 12000 * (1 + forwards["f23"])

FinalValue <-inv1 + inv2 + inv3 
FinalValue
