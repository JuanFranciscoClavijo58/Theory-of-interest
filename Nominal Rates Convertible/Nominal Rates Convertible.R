##########################
#         EXERCISE       #
##########################

#############################################################################
# Calculate, showing all workings, the nominal rate of interest per annum   #
# convertible quarterly which is equivalent to:                             #
#############################################################################

########################################################
#(a) an effective rate of interest of 4% per half-year.#
########################################################
library(FinancialMath)
res_a <- rate.conv(rate = 0.08,conv = 2,nom =4)
res_a
# Nominal Rate
res_a[1,1]
# i^(4)
round(res_a[3,1]*100,4)

#################################################################
# (b) a nominal rate of interest of 6% p.a. convertible monthly.#
#################################################################
res_b <- rate.conv(rate = 0.06, conv = 12, nom = 4)
res_b 
# Nominal Rate
res_b[1,1]
# i^(4)
round(res_b[3,1]*100,4)

#######################################################################
# (c) a nominal rate of discount of 6% p.a. convertible every 3 years.#
#######################################################################

res_c <- rate.conv(rate = 0.06, conv = 1/3, type = "discount", nom = 4)
res_c
# Nominal Rate
res_c[1,1]
# i^(4)
round(res_c[3,1]*100,4)






