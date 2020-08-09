############################################################
# R-project                                                #
# Program      : a2-CCO.r                                  #
# Protocol     :                                           #
# Date         :                                           #
# Last         :                                           #
# Programmer   : yoshifumi ukyo                            #
#                                                          #
############################################################
# [Ver.0000]                                               #
# Memorandom   :                                           #
#                                                          #
############################################################


#----- clean-up working directory 
rm(list = (ls(all = TRUE)))
#----- library assignment 
base_dir <- ""
setwd(base_dir)




#----- MMR vacinine vs 
d <- data.frame(A1i = c(0, 0, 1, 1, 1, 0, 0, 1, 1, 0), 
                A0i = c(1, 1, 0, 0, 0, 1, 1 ,0, 0 ,1), 
                T1i = c(21, 0, 21, 21, 21, 21, 21, 21, 21, 21), 
                T0i = c(344, 365, 344, 344, 344, 344, 344, 344, 344, 344))
d$Tpi <- d$T1i + d$T0i 
d$M1i <- d$A1i + d$A0i


IR_mh <- sum(d$A1i * d$T0i / d$Tpi) / sum(d$A0i * d$T1i / d$Tpi)
Var_log_IR_mh <- sum(d$M1i * d$T1i * d$T0i/ (d$Tpi^2)) / (sum(d$A1i * d$T0i / d$Tpi) * sum(d$A0i * d$T1i / d$Tpi))

IR_mh_lower <- exp(log(IR_mh) - qnorm(p = 0.975) * sqrt(Var_log_IR_mh))
IR_mh_upper <- exp(log(IR_mh) + qnorm(p = 0.975) * sqrt(Var_log_IR_mh))





