# Load and attach and list the packages that is needed
library(Rcpp)
library(deSolve)
library(plotly)                                                       
library(reshape2)                                                     
library(ggplot2)                                                      
library(tidyverse)
library(dplyr)
library(erer)                                                         
library(scales)                                                       
library(plyr)
library(openxlsx)                                                     
library(writexl)       
library(readr)
library(tidyr)
library(data.table)


# setwd("D:/checking/Rusha/tornado plot")

setwd("C:/Users/rmondal/Documents/HCV_new") # sets the working directory # can take it out
sourceCpp('p1_scr_SS.cpp')                  # reads R code from the named file
source("parms.R")                           # reads R code from the named file for parameters for sub group of 41-50 with prevalence of HCV screening and confirming a positive rate
source("parms_base.R")                      # reads R code from the named file for parameters of base line in current climate in Thailand
source("inits.R")                           # reads R code from the named file for setting up initial death for chosen sub group of population
source("inits_base.R")                      # reads R code from the named file for setting up initial base case
set.seed(123)                               #For fixed randomization to get the same result



# Defines the server logic which is required to draw a histogram

times <- seq(1999, 2060,by=1)

# Generates a sequence from 1999 to 2060 by 1 as is the time frame the data is from and the scenarios will be projected 

out <- ode( y = inits,times =  times, func = PanHepC, parms = parms)

# Function ode takes a input with a state variable vector
# The time at which the model is output is required and function that gives the rate of change and the parameter vectors
# Out <- ode( y = inits(),times =  times, func = PanHepC, parms = parms(), maxsteps = 5000, rtol = 1, atol = 1)

out_df <- as.data.frame(out)#to make sure the object is in the data frame
out_df

# Gives values of (people?population)in the following compartments
# Gives values on time,S,F0,F1,F2,F3,C1,C2,C3,C4,HCC_A,HCC_B,HCC_C,HCC_D,D,dthC14,dthHCC  C1std_cured
# And C1new_cured  C2new_cured  C3new_cured  C4new_cured prevalence population total_infection
# And total_HCC total_HCV  newdeath incidenceHCC new_tranLiv treat_standard treat_new   screen
# And fibrosis compensate decompensate


############################ Baseline ############################################################

# Baseline is needed for the comparison of the ICER values, values are based on literature on current screening in thailand

times_base <- seq(1999, 2060,by=1)

# Generates a sequence from 1999 to 2060 by 1

out_base <- ode( y = inits_base,times =  times_base, func = PanHepC, parms = parms_base)

# y is the state variable vector which is initial base, the time at which the output is required in time at baseline, rate of change is the function and the parameter is prams base

out_df_base <- as.data.frame(out_base)

#check if out_base is as a data frame

out_df_base

# same as previous for the compartment that are outputted but for baseline this time



################################## Cost of screening, confirming and treatment ################

# These numbers were varied as was looking at different screening programs, hence the different ICER values as is the cost parameter values
# Values depend on scenario that is being studied, hence the values for screening and confirming cost and sensitivity and specificity will change based on scenario and litrature

cost_parms <-c(
  Treatment_Cost = 24000,  # cost of SOF+VEL (current treatment that is used)
  Screening_Cost = 1800,   # cost can vary depending on the screening method chosen which are rapid strip, HCV antibody or rapid HCV RNA
  Confirming_Cost = 1500,  # cost varies depending on confirming method that is chosen which are HCV RNA, core antigen and rapid HCV RNA
  Extra_Cost = 1500+2500,
  cost1 = 4470,
  cost2 = 4380,
  cost3 = 6060,
  cost4 = 9900
  # cost of HCV genotype testing is not taken into out but keeping fibroscan and safety lab after speaking to health care professional
)                          # all costs in TBH
cost_parms

util_parms <-c(Fibrosis_Rate = 0.73,        # Rates of Fibrosis 
               Compensate_Rate = 0.7,       # Rates of Compensation 
               Decompensate_Rate = 0.53,    # Rates of Decompensation 
               Total_HCC_Rate = 0.53)       # Total HCC rate 
util_parms

# Now putting into the cost utility values, which is treatment, screening, confirming and extra as a function
# fibrosis+compensate+decompensate+total_HCC
cost_utility_list <-function(Cost = c(Treatment_Cost,      # Cost values that are used in overall calculation 
                                      Screening_Cost,
                                      Confirming_Cost,
                                      Extra_Cost,  
                                      cost1,
                                      cost2,
                                      cost3,
                                      cost4),
                             Util = c(Fibrosis_Rate,       # Utility values that are used in overall calculation 
                                      Compensate_Rate,
                                      Decompensate_Rate,
                                      Total_HCC_Rate)
){
  screening_people_new <- 11169018                 # This is the capacity based on the age group of 41-50
  Treatment_Cost <- Cost["Treatment_Cost"]         # Assigning the names
  Screening_Cost <- Cost["Screening_Cost"]
  Confirming_Cost <- Cost["Confirming_Cost"]
  Extra_Cost <- Cost["Extra_Cost"]
  Cost1 <- Cost["cost1"]
  Cost2 <- Cost["cost2"]
  Cost3 <- Cost["cost3"]
  Cost4 <- Cost["cost4"]
  # Adding utility variables
  Fibrosis_Rate <- Util["Fibrosis_Rate"]           # Rate of fibrosis F0-F3
  Compensate_Rate <- Util["Compensate_Rate"]       # Compensate rate C1 - C2
  Decompensate_Rate <- Util["Decompensate_Rate"]   # Decompensate rate C3-C4
  Total_HCC_Rate <- Util["Total_HCC_Rate"]
  
  df_base <- out_df_base
  df_new <- out_df
  
  attach(df_base)
  # Then can just use the column names
  # Attaching the set of R objects to the search path therefore will search the database when evaluation of the variable
  
  
  Extra_Cost_base <- 4000 + 2500 + 1500            # This is what is currently used in Thai context screening methods
  treat_coas_base <- Treatment_Cost                # Values are from HCV genotype testing + Fibroscan stiffness testing + Relevant and safety lab respectively
  screening_people <- screen
  
  
  Confirming_base <- round(screen*(94/100)*(98/100)*Confirming_Cost)
  
  # Cost of Confirming test (second stage of screening)
  # This is also accounting for sensitivity and specificity respectively to the confirming screening methods
  # Cost of screening using GeneEXpert
  
  screen_base <- screen*(Screening_Cost + Extra_Cost_base) + Confirming_base
  
  # Cost of treatment using Sof-Vel as this is the treatment normally used and is pangenotypic
  treat_base <- treat_new*treat_coas_base
  
  # Cost of extra lab per treatment
  # extra_base <- -c((diff(fibrosis )+diff(compensate)+diff(decompensate)+diff(total_HCC)))*Extra_Cost_base
  # cost per visit
  # indirect_base <- fibrosis*4470+compensate*4380 + decompensate*6060 + total_HCC*9900
  # cost per visit
  # complicate_base <-  compensate*86236 + decompensate*157755 + total_HCC*197961
  # Total cost from 2019 on wards
  
  total_base <- screen_base[21:62]+treat_base[21:62]
  
  # Total cost with 3% discount
  
  total_base_dis<-total_base*(1/(1+0.03)^(0:41))
  cost_base_df <- data.frame(df_base[c(21:62),1] , screen_base[21:62],treat_base[21:62],total_base,total_base_dis)
  
  # Total utility from 2019 (quality of life loss due to infection and death)
  
  utility_base <- fibrosis[21:62]*Fibrosis_Rate+compensate[21:62]*Compensate_Rate+
    decompensate[21:62]*Decompensate_Rate+total_HCC[21:62]*Total_HCC_Rate+
    diff(dthC14)[20:61]*27+diff(dthHCC)[20:61]*17
  
  # Total Utility with 3% discount rate, rate was confirmed by literature
  
  utility_base_dis <- utility_base*(1/(1+0.03)^(0:41))
  
  detach()                                        # Remove the data base from the search
  HCC_death_base<-df_base[c(21:62),17]-df_base[c(20:61),17]        # names(out_df_base)[34] #21 rows and all 62 columns
  utility_base_df <-data.frame(df_base[c(21:62),1],
                               round(df_base[c(21:62),34]),        # fibrosis - 28080.29
                               round(df_base[c(21:62),35]),        # compensate - 19653.67
                               round(df_base[c(21:62),36]),        # decompensate -1304.039
                               round(df_base[c(21:62),c(25)]),     # total_infection
                               round(df_base[c(21:62),c(26)]),     # total HCC
                               round(df_base[c(21:62),c(29)]),     # incidence
                               round(df_base[c(21:62),c(28)]),     # new death
                               round(HCC_death_base),
                               round(df_base[c(21:62),c(15)]),     # Total death
                               round(df_base[c(21:62),c(17)]),     # Total death HCC
                               round(utility_base),
                               round(utility_base_dis))
  attach(df_new)
  screening_people <- screen*0
  
  screening_people[21] <- screening_people_new
  
  
  # Cost of Confirming
  # Confirming_Cost <- round(screening_people*(dia$screening_sens/100)*(dia$screening_spec/100)*dia$Confirming_Cost)
  
  Confirming_new <- round(screening_people*(94/100)*(98/100)*Confirming_Cost)# confirming cost sensitivity and specificity
  
  # Cost of screening using GeneEXpert
  
  screen_new <- screening_people*(Screening_Cost+Extra_Cost) + Confirming_new
  
  # Cost of treatment using Sof-Vel
  
  treat_new <- treat_new*Treatment_Cost
  
  # Cost of extra lab per treatment
  # extra_new <- -c((diff(fibrosis)+diff(compensate)+diff(decompensate)+diff(total_HCC)))*extra$total_cost
  # Cost per visit
   indirect_new <- fibrosis*Cost1+compensate*Cost2 + decompensate*Cost3 + total_HCC*Cost4
  # Cost per visit
  # complicate_new <-  compensate*86236 + decompensate*157755 + total_HCC*197961
  # total cost from 2019 and onwards 
  
  total_new <- screen_new[21:62]+treat_new[21:62]
  
  # total cost with 3% discount
  
  total_new_dis<-total_new*(1/(1+0.03)^(0:41))
  
  cost_new_df <- data.frame(df_base[c(21:62),1],screen_new[21:62],treat_new[21:62],indirect_new[21:62],total_new,total_new_dis)
  
  # total utility from 2019 (quality of life loss due to infection and death)

  utility_new <- fibrosis[21:62]*(1-Fibrosis_Rate)+compensate[21:62]*(1-Compensate_Rate)
  +decompensate[21:62]*(1-Decompensate_Rate)+total_HCC[21:62]*(1-Total_HCC_Rate)+
    diff(dthC14)[20:61]*27+diff(dthHCC)[20:61]*17
  
  # total Utility with 3% discount
  utility_new_dis <- utility_new*(1/(1+0.03)^(0:41))
  
  detach()                                                         # removing it from the search
  HCC_death_new<-df_new[c(21:62),17]-df_new[c(20:61),17]
  utility_new_df <-data.frame(df_base[c(21:62),1],
                              round(df_new[c(21:62),34]),          # fibrosis
                              round(df_new[c(21:62),35]),          # compensate
                              round(df_new[c(21:62),36]),          # decompenaste
                              round(df_new[c(21:62),c(25)]),       # total infection
                              round(df_new[c(21:62),c(26)]),       # total HCC
                              round(df_new[c(21:62),c(29)]),       # incidence
                              round(df_new[c(21:62),c(28)]),       # new death
                              round(HCC_death_new),
                              round(df_new[c(21:62),c(15)]),       # Total death
                              round(df_new[c(21:62),c(17)]),       # Total death HCC
                              round(utility_new),
                              round(utility_new_dis))
  table_name_cost <-c("Times","screening cost","Treatment cost",
                      "Total cost","Total cost with discount(3%)")
  names(cost_base_df) <- table_name_cost
  names(cost_new_df) <- table_name_cost
  Utility_table_name <-c("Times","Fibrosis","Compensate","Decompensate","Total Infection",
                         "Total HCC","Incidence HCC","New Death","Death HCC","Total Death",
                         "Total Death HCC","Utility","Utility With discount")
  names(utility_base_df) <- Utility_table_name
  names(utility_new_df) <- Utility_table_name
  cost_utility_list <- list(
    "cost_base_df"=cost_base_df,"utility_base_df"=utility_base_df,"cost_new_df"=cost_new_df,"utility_new_df"=utility_new_df
  )
  cost_utility_list
  
}
# dia <-  reactiveValues(screening_name = "",
#                        screening_sens = 0,
#                        screening_spec = 0,
#                        Screening_Cost = 0,
#                        confirming_name = "",
#                        confirming_sens = 0,
#                        confirming_spec = 0,
#                        Confirming_Cost = 0
# )

##################################################ICER###########################################

# ICER is the incremental cost divided by the incremental QUALYs which is calculated for each scenario

icer_calculate <- function(Cost = c(Treatment_Cost,                # Cost that will have overall effect on ICER 
                                    Screening_Cost,
                                    Confirming_Cost,
                                    Extra_Cost,
                                    Cost1,
                                    Cost2, 
                                    Cost3, 
                                    Cost4),
                           Util = c(Fibrosis_Rate,                 # Utility values that will have overall effect on ICER
                                    Compensate_Rate,
                                    Decompensate_Rate,
                                    Total_HCC_Rate))               # Costs that will go into the top fraction of the ICER ratio
{
  CUlist <- cost_utility_list(Cost,Util)
  ncol_cost <- ncol(data.frame(CUlist["cost_base_df"]))            # Return the number of columns presented in the data frame. using the baseline cost and utility as for ICER to compare to new intervention
  ncol_utility <- ncol(data.frame(CUlist["utility_base_df"]))      # Return the baseline utility for the ICER value to compare to new intervention
  
  total_base_dis <- data.frame(CUlist["cost_base_df"])[,ncol_cost]
  total_new_dis <- data.frame(CUlist["cost_new_df"])[,ncol_cost]
  utility_base_dis <- data.frame(CUlist["utility_base_df"])[,ncol_utility]
  utility_new_dis <- data.frame(CUlist["utility_new_df"])[,ncol_utility]
  #ICER_r1_Inc_Cost (10 Million)
  
  ICER_r1_Inc_Cost <- (sum(total_new_dis)-sum(total_base_dis))/1000000
  #DALY averted
  ICER_r1_Qal_gain <- (sum(utility_base_dis)-sum(utility_new_dis))
  ICER_r1 <- data.frame(ICER_r1_Inc_Cost,ICER_r1_Qal_gain)
  ICER_r1
}


CUlist <- cost_utility_list(Cost = cost_parms,Util = util_parms)

ICER_r1 <- icer_calculate(Cost = cost_parms,Util = util_parms)

axis_y1 <- -150000
if(ICER_r1_Inc_Cost < -140000){
  round_cost <- round(ICER_r1_Inc_Cost)-20000
  
  axis_y1 <- round(round_cost,-nchar(round_cost)+3)
}
axis_y2 <- axis_y1/2

WTP.5GDP <- 160000
colours =c("black") #,"blue","purple", "green", "pink","orange", "grey", "darkred","cyan2","blueviolet", "darkgoldenrod4","brown2","lawngreen")
#plot(summary_ICER_PSA_compareWORST_SIIL[1,2,],summary_ICER_PSA_compareWORST_SIIL[1,1,])

plot(0, xlim=c(-1100000, 10000000), ylim=c(axis_y1, 200000), xlab="Incremental QALYs",ylab="Incremental Costs THB (Million)", pch=18, col=colours,
     main="Incremental cost-effectiveness ratio (ICER) plane",yaxt="n",xaxt="n" )

axis(side = 2,at=c(axis_y1,axis_y2,0,50000,100000),labels = c(as.character(axis_y1),as.character(axis_y2),"0","50,000","100,000"))
axis(side = 1,at=c(-1000000,-500000,0,1500000,3000000),labels = c("-1,000,000","-500,000","0","1,500,000","3,000,000"))

qq<-c(-1000000,0,1000000)
ww<-qq*160000/(10^6)
lines(qq,ww,col='brown4',lty=2)

abline(h=0,v=0) # Adds straight line to a plot
points(ICER_r1[1,2],ICER_r1[1,1],pch=15,col="yellow", cex=3) # adds the point of the ICER
text(ICER_r1[1,2], ICER_r1[1,1]-15000,paste(round(ICER_r1[1,2]),",", round(ICER_r1[1,1])),col = 'brown4',cex = 1)

par(ps=16)
legend("topleft", title="Screening scheme",cex = 0.5,
       c("By New Screening Scheme"), fill=c("yellow", "sea green","gold"))

# Can repeat the top code with the various scenarios by the corresponding values 


################# Plotting all the ICER values in one plot ####################################

WTP.5GDP <- 160000

colours =c("black") #,"blue","purple", "green", "pink","orange", "grey", "darkred","cyan2","blueviolet", "darkgoldenrod4","brown2","lawngreen")

#plot(summary_ICER_PSA_compareWORST_SIIL[1,2,],summary_ICER_PSA_compareWORST_SIIL[1,1,])

plot(0, xlim=c(-100000, 1500000), ylim=c(axis_y1, 250000), xlab="Incremental DALYs",ylab="Incremental Costs THB (Million)", pch=18, col=colours,
     main="Incremental cost-effectiveness ratio (ICER) plane",yaxt="n",xaxt="n" )

abline(h=0,v=0) # Adds straight line to a plot


axis(side = 2,at=c(axis_y1,axis_y2,0,100000,200000),labels = c(as.character(axis_y1),as.character(axis_y2),"0","100,000","200,000"))
axis(side = 1,at=c(-100000,0,500000,1000000,1500000),labels = c("-100000","0","500000","1000000","1500000"))



qq<-c(-2000000,0,2000000)                               # Adjusting the scale to see difference in ICER values better for each scenario
ww<-qq*160000/(10^6)                                    # Adding in the threshold value that is 16000 TBH curently 
lines(qq,ww,col='brown4',lty=2)                         # Color of the line and adding in the line types 


points(1359506,209640, pch=20,col="cyan1", cex=2)        # adds scenario 1 point to the ICER
points(1359506,166717, pch=20,col="purple", cex=2)        # adds scenario 2 point to the ICER
points(1353656,160132, pch=20,col="deeppink", cex=2)      # adds scenario 3 point to the ICER
points(1350375,152041, pch=20,col="chartreuse", cex=2)    # adds scenario 4 point to the ICER


###################################Probabilistic Sensitivity Analysis ###########################################################

cost_utility_list_base <- function(){
  
  df_base <- out_df_base
  
  attach(df_base)
  # Then can just use the column names
  # Attaching the set of R objects to the search path therefore will search the database when evaluation of the variable
  
  Treat_coas_base <- 24000                     # Current treatment assuming the use of SOF +VEL
  Screening_cost_base <- 1800                  # This is what is currently used in Thai context screening methods
  Extra_Cost_base <- 4000 + 2500 + 1500        # Values are from HCV genotype testing + Fibroscan stiffness testing + Relevant and safety lab respectively
  Confirming_cost_base <- 1800                 # Keeping the confirming cost in 
  
  screening_people <- screen
  
  
  Confirming_base <- round(screen*(94/100)*(98/100)*Confirming_cost_base) 
  
  # Cost of Confirming test (second stage of screening)
  # This is also accounting for sensitivity and specificity respectively to the confirming screening methods
  # Cost of screening using GeneEXpert
  
  screen_base <- screen*(Screening_cost_base + Extra_Cost_base) + Confirming_base 
  
  # What the total screening base accounts for as extra cost may or may not be included
  # Cost of treatment using Sof-Vel as this is the treatment normally used and is pangenotypic
  
  treat_base <- treat_new*Treat_coas_base
  
  # Cost of extra lab per treatment
  # extra_base <- -c((diff(fibrosis )+diff(compensate)+diff(decompensate)+diff(total_HCC)))*Extra_Cost_base
  # cost per visit
  # indirect_base <- fibrosis*4470+compensate*4380 + decompensate*6060 + total_HCC*9900
  # cost per visit
  # complicate_base <-  compensate*86236 + decompensate*157755 + total_HCC*197961
  # Total cost from 2019 on wards
  
  total_base <- screen_base[21:62]+treat_base[21:62]
  
  total_base_dis<-sum(total_base*(1/(1+0.03)^(0:41)))
  
  # Total cost with 3% discount and accounting for screening baseline and the treatment base line
  # Total utility from 2019 (quality of life loss due to infection and death)
  
  utility_base <- fibrosis[21:62]*0.73+compensate[21:62]*0.7+
    decompensate[21:62]*0.53+total_HCC[21:62]*0.53+
    diff(dthC14)[20:61]*27+diff(dthHCC)[20:61]*17
  
  # Total Utility with 3% discount rate, rate was confirmed by literature along with the corresponding rates with the specific age group 
  
  utility_base_dis <- sum(utility_base*(1/(1+0.03)^(0:41)))
  
  detach()
  
  cost_utility_list <- list(
    "total_base_dis"=total_base_dis,"utility_base_dis"=utility_base_dis
  )
  cost_utility_list
}


cost_utility_list2 <-function(Treatment_Cost = Treatment_Cost,         # Incorporates all the variables that may have an effect on the ICER 
                              Screening_Cost = Screening_Cost,
                              Confirming_Cost = Confirming_Cost,
                              Extra_Cost = Extra_Cost,
                              Fibrosis_Rate = Fibrosis_Rate,
                              Compensate_Rate = Compensate_Rate,
                              Decompensate_Rate = Decompensate_Rate,
                              Total_HCC_Rate = Total_HCC_Rate,
                              Sens = 0.985                             
                              
){
  parms$sens <- Sens
  out <- ode( y = inits,times =  times, func = PanHepC, parms = parms)
  df_new <- as.data.frame(out)
  screening_people_new <- 11169018         # This is the capacity based on the age group of 41-50
  Treatment_Cost <- Treatment_Cost         # Assigning the names
  Screening_Cost <- Screening_Cost
  Confirming_Cost <- Confirming_Cost
  Extra_Cost <- Extra_Cost
  # Adding utility variables
  Fibrosis_Rate <- Fibrosis_Rate           # Rate of fibrosis F0-F3
  Compensate_Rate <- Compensate_Rate       # Compensate rate C1 - C2
  Decompensate_Rate <- Decompensate_Rate   # Decompensate rate C3-C4
  Total_HCC_Rate <- Total_HCC_Rate         # Total HCC rate 
  
  Confirming_new_vec <- NULL
  screen_new_vec <- NULL
  Treatment_new_vec <- NULL
  total_new_dis_vec <- NULL
  
  Fibrosis_new_vec <- NULL
  Compensate_new_vec <- NULL
  Decompensate_new_vec <- NULL
  Total_HCC_new_vec <- NULL
  Utility_new_dis_vec <- NULL
  
  attach(df_new)
  screening_people <- screen*0
  
  screening_people[21] <- screening_people_new
  
  
  # Cost of Confirming
  # Confirming_Cost <- round(screening_people*(dia$screening_sens/100)*(dia$screening_spec/100)*dia$Confirming_Cost)
  
  for(Conf in Confirming_Cost){
    Confirming_new <- round(screening_people[21:62]*(94/100)*(98/100)*Conf) # confirming cost sensitivity and specificity
    Confirming_new_vec <- c(Confirming_new_vec,Confirming_new)
    
  }
  
  for(i in 1:length(Screening_Cost)){
    # Cost of screening using GeneEXpert
    screen_new <- screening_people[21:62]*(Screening_Cost[i]+Extra_Cost[i])
    screen_new_vec <- c(screen_new_vec,screen_new)
  }
  
  
  for(Treat in Treatment_Cost){
    treat_cost <- treat_new[21:62]*Treat
    Treatment_new_vec <- c(Treatment_new_vec,treat_cost)
  }
  
  # Cost of extra lab per treatment
  # extra_new <- -c((diff(fibrosis)+diff(compensate)+diff(decompensate)+diff(total_HCC)))*extra$total_cost
  # Cost per visit
  # indirect_new <- fibrosis*4470+compensate*4380 + decompensate*6060 + total_HCC*9900
  # Cost per visit
  # complicate_new <-  compensate*86236 + decompensate*157755 + total_HCC*197961
  # total cost from 2019 onwards+Treatment_new_vec
  
  total_new <- Confirming_new_vec+screen_new_vec+Treatment_new_vec
  
  # total cost with 3% discount
  total_new_dis<-total_new*(1/(1+0.03)^(0:41))      # Takes into account the discounting factor
  total_new_dis_vec <- colSums(matrix(total_new_dis,42,length(Screening_Cost)))
  
  for(RateF in Fibrosis_Rate){
    Fibrosis_new <- fibrosis[21:62]*RateF
    Fibrosis_new_vec <- c(Fibrosis_new_vec,Fibrosis_new)
  }
  
  for(RateC in Compensate_Rate){
    Compensate_new <- compensate[21:62]*RateC
    Compensate_new_vec <- c(Compensate_new_vec,Compensate_new)
  }
  
  for(RateD in Decompensate_Rate){
    Decompensate_new <- decompensate[21:62]*RateD
    Decompensate_new_vec <- c(Decompensate_new_vec,Decompensate_new)
  }
  
  for(RateH in Total_HCC_Rate){
    Total_HCC_new <- total_HCC[21:62]*RateH
    Total_HCC_new_vec <- c(Total_HCC_new_vec,Total_HCC_new)
  }
  
  utility_new <-Fibrosis_new_vec+Compensate_new_vec+
    Decompensate_new_vec+Total_HCC_new_vec+
    diff(dthC14)[20:61]*27+diff(dthHCC)[20:61]*17
  
  # total Utility with 3% discount
  
  utility_new_dis <- utility_new*(1/(1+0.03)^(0:41))
  Utility_new_dis_vec <- colSums( matrix(utility_new_dis,42,length(Screening_Cost)))
  
  detach()                                                         # removing it from the search
  
  cost_utility_list <- list(
    "total_new_dis"=total_new_dis_vec,"utility_new_dis"=Utility_new_dis_vec
  )
  cost_utility_list
}


icer_calculate2 <- function(Treatment_Cost = Treatment_Cost,           # Top ratio treatment cost
                            Screening_Cost = Screening_Cost,           # Top ratio screening cost
                            Confirming_Cost = Confirming_Cost,         # Top ratio confirming cost 
                            Extra_Cost = Extra_Cost,                   # Top ratio extra cost
                            Fibrosis_Rate = Fibrosis_Rate,             # Bottom ratio fibrosis rate
                            Compensate_Rate = Compensate_Rate,         # Bottom ratio compensate rate
                            Decompensate_Rate = Decompensate_Rate,     # Bottom ratio decompensate rate
                            Total_HCC_Rate = Total_HCC_Rate,           # Bottom ratio total HCC rate
                            Sens = 0.985                               # Sensitivity compounded
                            
)                             
{
  CUlist_base <- cost_utility_list_base()
  CUlist_new <- cost_utility_list2(Treatment_Cost = Treatment_Cost,
                                   Screening_Cost = Screening_Cost,
                                   Confirming_Cost = Confirming_Cost,
                                   Extra_Cost = Extra_Cost,
                                   Fibrosis_Rate = Fibrosis_Rate,
                                   Compensate_Rate = Compensate_Rate,
                                   Decompensate_Rate = Decompensate_Rate,
                                   Total_HCC_Rate = Total_HCC_Rate,
                                   Sens = Sens
                                   
  )
  # cost_utility_list <- list(
  #   "total_base_dis"=total_base_dis,"utility_base_dis"=utility_base_dis
  # )
  # cost_utility_list <- list(
  #   "total_new_dis"=total_new_dis_vec,"utility_new_dis"=Utility_new_dis_vec
  # )
  total_base_dis <- unlist(CUlist_base["total_base_dis"])      # as given a list structure, unlist will simplify it to produce a vector containing the atomic component 
  total_new_dis <- unlist(CUlist_new["total_new_dis"])
  utility_base_dis <- unlist(CUlist_base["utility_base_dis"])
  utility_new_dis <- unlist(CUlist_new["utility_new_dis"])
  #ICER_r1_Inc_Cost (1 Million)
  ICER_r1_Inc_Cost <- (total_new_dis-total_base_dis)/1000000
  ICER_r1_Qal_gain <- utility_new_dis - utility_base_dis
  ICER_r1 <- data.frame(ICER_r1_Inc_Cost,ICER_r1_Qal_gain)
  ICER_r1
}

PSA_start <- 1                                                    # Starting the PSA 
PSA_stop <- 1000                                                  # PSA having thousand iterations 
deltat<- 1                                                        # Sampling times of the time series 
tps <- seq(PSA_start , PSA_stop , by = deltat)                    # Forming the sequence of 1000 iterations 
Fibrosis_Rate     <- rbeta(length(tps),0.73,0.5)                  # Rates and having a beta distribution 
Compensate_Rate   <- rbeta(length(tps),0.7,0.5)                   # Rates taken from exiting literature 
Decompensate_Rate <- rbeta(length(tps),0.53,0.5)                  # Beta distribution used as models continuous random variables which has a range between 0-1
Total_HCC_Rate    <- rbeta(length(tps),0.53,0.5)                  # Beta distribution is used for rate and gamma is used for cost

targetmean <- 24000                                               # Mean cost of the treatment 
shape <- 1                                                        # Shape 1 as non- negative parameters of the Beta distribution 
scale<-targetmean/shape                                           # Hence 24000
rgamma(length(tps),shape=shape,scale=scale)                       # Gamma distribution is used also in conjunction with literature 
Treatment_Cost    <- rgamma(length(tps),shape=shape,scale=scale)  # SOF+VEL
targetmean <- 1800                                                # Mean cost of screening 
shape <- 1
scale<-targetmean/shape
Screening_Cost    <- rgamma(length(tps),shape=shape,scale=scale)  
targetmean <- 1500                                                # Mean cost of confirming 
shape <- 1
scale<-targetmean/shape
Confirming_Cost   <- rgamma(length(tps),shape=shape,scale=scale)  # HCV RNA
targetmean <- 4000
shape <- 1
scale<-targetmean/shape
Extra_Cost        <- rgamma(length(tps),shape=shape,scale=scale)  # With safety lab and fibro scan

# cost_utility_list2(Treatment_Cost = c(1),
#                    Screening_Cost = Screening_Cost,
#                    Confirming_Cost = Confirming_Cost,
#                    Extra_Cost = Extra_Cost,
#                    Fibrosis_Rate = Fibrosis_Rate,
#                    Compensate_Rate = Compensate_Rate,
#                    Decompensate_Rate = Decompensate_Rate,
#                    Total_HCC_Rate = Total_HCC_Rate
#
# )
result <- icer_calculate2(Treatment_Cost = Treatment_Cost,          # All costs are accounted for going into the ICER
                          Screening_Cost = Screening_Cost,               
                          Confirming_Cost = Confirming_Cost,
                          Extra_Cost = Extra_Cost,
                          Fibrosis_Rate = Fibrosis_Rate,            # All utilities that are accounted for going into the ICER
                          Compensate_Rate = Compensate_Rate,
                          Decompensate_Rate = Decompensate_Rate,
                          Total_HCC_Rate = Total_HCC_Rate
)

axis_y1 <- -150000                                                  # Used for plotting the graph 
axis_y2 <- axis_y1/2                                                # Used for plotting the graph

WTP.5GDP <- 160000

colours =c("black") #,"blue","purple", "green", "pink","orange", "grey", "darkred","cyan2","blueviolet", "darkgoldenrod4","brown2","lawngreen")

#plot(summary_ICER_PSA_compareWORST_SIIL[1,2,],summary_ICER_PSA_compareWORST_SIIL[1,1,])

plot(0, xlim=c(-100000, 1500000), ylim=c(axis_y1, 250000), xlab="Incremental DALYs",ylab="Incremental Costs THB (Million)", pch=18, col=colours,
     main="Incremental cost-effectiveness ratio (ICER) plane",yaxt="n",xaxt="n" )

abline(h=0,v=0) # Adds straight line to a plot


axis(side = 2,at=c(axis_y1,axis_y2,0,100000,200000),labels = c(as.character(axis_y1),as.character(axis_y2),"0","100,000","200,000"))
axis(side = 1,at=c(-100000,0,500000,1000000,1500000),labels = c("-100000","0","500000","1000000","1500000"))



qq<-c(-2000000,0,2000000)                               # Adjusting the scale to see difference in ICER values better for each scenario
ww<-qq*160000/(10^6)                                    # Adding in the threshold value that is 16000 TBH curently 
lines(qq,ww,col='brown4',lty=2)                         # Color of the line and adding in the line types 


#WTP.5GDP <- 160000                                                  # This is the threshold value in Thailand in TBH
#colours =c("blue") #,"blue","purple", "green", "pink","orange", "grey", "darkred","cyan2","blueviolet", "darkgoldenrod4","brown2","lawngreen")
#plot(summary_ICER_PSA_compareWORST_SIIL[1,2,],summary_ICER_PSA_compareWORST_SIIL[1,1,])

#plot(0, xlim=c(-100000, 300000), ylim=c(axis_y1, 100000), xlab="Incremental QALYs",ylab="Incremental Costs THB (Million)", pch=18, col=colours,
#     main="Incremental cost-effectiveness ratio (ICER) plane",yaxt="n",xaxt="n" )

#axis(side = 2,at=c(axis_y1,axis_y2,0,50000,100000))               # Adding the axis range and lables 
#axis(side = 1,at=c(-1000000,-500000,0,100000,200000,300000 ))     # Adding the axis range and lables 

#qq<-c(-1000000,0,1000000)                                         # Adding the intercention line 
#ww<-qq*160000/(10^6)                                              # Adding the threshold 
#lines(qq,ww,col='brown4',lty=2)

#abline(h=0,v=0)                                                   # Adds straight line to a plot

result


points(result[,2],result[,1],pch=21, cex=1)                       # Used for plotting restults and shape 


Incremental_QUALYs <- result[,2]
Incremental_Cost <- result[,1]

plot(Incremental_QUALYs,Incremental_Cost)                         # Can now see the overall points in the PSA


######################Checking to see if overall makes sense #######################################


cost_parms <-c(
  Treatment_Cost = 24000,                         # cost of SOF+VEL (current treatment that is used)
  Screening_Cost = 1800,                          # cost can vary depending on the screening method chosen which are rapid strip, HCV antibody or rapid HCV RNA
  Confirming_Cost = 1500,                         # cost varies depending on confirming method that is chosen which are HCV RNA, core antigen and rapid HCV RNA
  Extra_Cost = 0                                  # cost of HCV genotype testing is not taken into out but can keep fibroscan and safety lab after speaking to health care professional
)

util_parms <-c(Fibrosis_Rate = 0.73,              # Rate of Fibrosis 
               Compensate_Rate = 0.7,             # Rate of Compensation
               Decompensate_Rate = 0.53,          # Rate of Decompensation 
               Total_HCC_Rate = 0.53)             # Rate of total HCC 

icer_calculate(Cost=cost_parms,Util = util_parms) # As ICER is cost / utility 
# ICER_r1_Inc_Cost ICER_r1_Qal_gain
# 1        -35644.75           161849

icer_calculate2(Treatment_Cost = 24000,
                Screening_Cost = 1800,
                Confirming_Cost = 1500,
                Extra_Cost = 0,
                Fibrosis_Rate = 0.73,
                Compensate_Rate = 0.7,
                Decompensate_Rate = 0.53,
                Total_HCC_Rate = 0.53
                
)
# ICER_r1_Inc_Cost ICER_r1_Qal_gain
# total_base_dis        -35644.75         161844.7

parms$sens <- 0.7

out <- ode( y = inits,times =  times, func = PanHepC, parms = parms)

out_df <- as.data.frame(out)

icer_calculate(Cost=cost_parms,Util = util_parms)

# ICER_r1_Inc_Cost ICER_r1_Qal_gain
# 1        -35610.62           148978

icer_calculate2(Treatment_Cost = 24000,
                Screening_Cost = 1800,
                Confirming_Cost = 1500,
                Extra_Cost = 0,
                Fibrosis_Rate = 0.73,
                Compensate_Rate = 0.7,
                Decompensate_Rate = 0.53,
                Total_HCC_Rate = 0.53,
                Sens = 0.7
                
)

# ICER_r1_Inc_Cost ICER_r1_Qal_gain
# total_base_dis        -35610.62         148975.6



############################Tornado plot for the one-way sensitivity analysis  ######################################################

# putting in the upper and lower bounds for the eight parameters chosen for the one way sensitivity analysis 

bound_Treatment_Cost <- icer_calculate2(Treatment_Cost = c(min(Treatment_Cost),max(Treatment_Cost)),
                                        Screening_Cost = c(1800,1800),
                                        Confirming_Cost = c(1500,1500),
                                        Extra_Cost = c(4000,4000),
                                        Fibrosis_Rate = c(0.73,0.73),
                                        Compensate_Rate = c(0.7,0.7),
                                        Decompensate_Rate = c(0.53,0.53),
                                        Total_HCC_Rate = c(0.53,0.53),
                                        Sens = 0.985
)
# Doing for all eight parameters with the outcome of Incremental cost effective ratio 

bound_Treatment_Cost <- transpose(bound_Treatment_Cost%>% 
                                    transmute(ICER_Treatment = ICER_r1_Inc_Cost /ICER_r1_Qal_gain))
bound_Treatment_Cost <- bound_Treatment_Cost%>% 
  mutate(UL_Difference = abs(V1 - V2))
colnames(bound_Treatment_Cost) <- c("Lower_Bound", "Upper_Bound", "UL_Difference")

bound_Screening_Cost <- icer_calculate2(Treatment_Cost = c(24000,24000),
                                        Screening_Cost = c(min(Screening_Cost),max(Screening_Cost)),
                                        Confirming_Cost = c(1500,1500),
                                        Extra_Cost = c(4000,4000),
                                        Fibrosis_Rate = c(0.73,0.73),
                                        Compensate_Rate = c(0.7,0.7),
                                        Decompensate_Rate = c(0.53,0.53),
                                        Total_HCC_Rate = c(0.53,0.53),
                                        Sens = 0.985
)

bound_Screening_Cost <- transpose(bound_Screening_Cost%>% 
                                    transmute(ICER_Treatment = ICER_r1_Inc_Cost /ICER_r1_Qal_gain))
bound_Screening_Cost <- bound_Screening_Cost%>% 
  mutate(UL_Difference = abs(V1 - V2))
colnames(bound_Screening_Cost) <- c("Lower_Bound", "Upper_Bound", "UL_Difference")

bound_Confirming_Cost <- icer_calculate2(Treatment_Cost = c(24000,24000),
                                         Screening_Cost = c(1800,1800),
                                         Confirming_Cost = c(min(Confirming_Cost),max(Confirming_Cost)),
                                         Extra_Cost = c(4000,4000),
                                         Fibrosis_Rate = c(0.73,0.73),
                                         Compensate_Rate = c(0.7,0.7),
                                         Decompensate_Rate = c(0.53,0.53),
                                         Total_HCC_Rate = c(0.53,0.53),
                                         Sens = 0.985)


bound_Confirming_Cost <- transpose(bound_Confirming_Cost%>% 
                                     transmute(ICER_Treatment = ICER_r1_Inc_Cost /ICER_r1_Qal_gain))
bound_Confirming_Cost <- bound_Confirming_Cost%>% 
  mutate(UL_Difference = abs(V1 - V2))
colnames(bound_Confirming_Cost) <- c("Lower_Bound", "Upper_Bound", "UL_Difference")

bound_Extra_Cost <- icer_calculate2(Treatment_Cost = c(24000,24000),
                                    Screening_Cost = c(1800,1800),
                                    Confirming_Cost = c(1500,1500),
                                    Extra_Cost = c(min(Extra_Cost),max(Extra_Cost)),
                                    Fibrosis_Rate = c(0.73,0.73),
                                    Compensate_Rate = c(0.7,0.7),
                                    Decompensate_Rate = c(0.53,0.53),
                                    Total_HCC_Rate = c(0.53,0.53),
                                    Sens = 0.985)


bound_Extra_Cost <- transpose(bound_Extra_Cost%>% 
                                transmute(ICER_Treatment = ICER_r1_Inc_Cost /ICER_r1_Qal_gain))
bound_Extra_Cost <- bound_Extra_Cost%>% 
  mutate(UL_Difference = abs(V1 - V2))
colnames(bound_Extra_Cost) <- c("Lower_Bound", "Upper_Bound", "UL_Difference")


bound_Fibrosis_Rate <- icer_calculate2(Treatment_Cost = c(24000,24000),
                                       Screening_Cost = c(1800,1800),
                                       Confirming_Cost = c(1500,1500),
                                       Extra_Cost = c(4000,4000),
                                       Fibrosis_Rate = c(min(Fibrosis_Rate),max(Fibrosis_Rate)),
                                       Compensate_Rate = c(0.7,0.7),
                                       Decompensate_Rate = c(0.53,0.53),
                                       Total_HCC_Rate = c(0.53,0.53),
                                       Sens = 0.985)

bound_Fibrosis_Rate <- transpose(bound_Fibrosis_Rate%>% 
                                   transmute(ICER_Treatment = ICER_r1_Inc_Cost /ICER_r1_Qal_gain))
bound_Fibrosis_Rate <- bound_Fibrosis_Rate%>% 
  mutate(UL_Difference = abs(V1 - V2))
colnames(bound_Fibrosis_Rate) <- c("Lower_Bound", "Upper_Bound", "UL_Difference")

bound_Compensate_Rate <- icer_calculate2(Treatment_Cost = c(24000,24000),
                                         Screening_Cost = c(1800,1800),
                                         Confirming_Cost = c(1500,1500),
                                         Extra_Cost = c(4000,4000),
                                         Fibrosis_Rate = c(0.73,0.73),
                                         Compensate_Rate = c(min(Compensate_Rate),max(Compensate_Rate)),
                                         Decompensate_Rate = c(0.53,0.53),
                                         Total_HCC_Rate = c(0.53,0.53),
                                         Sens = 0.985)

bound_Compensate_Rate <- transpose(bound_Compensate_Rate%>% 
                                     transmute(ICER_Treatment = ICER_r1_Inc_Cost /ICER_r1_Qal_gain))
bound_Compensate_Rate <- bound_Compensate_Rate%>% 
  mutate(UL_Difference = abs(V1 - V2))
colnames(bound_Compensate_Rate) <- c("Lower_Bound", "Upper_Bound", "UL_Difference")

bound_Decompensate_Rate <- icer_calculate2(Treatment_Cost = c(24000,24000),
                                           Screening_Cost = c(1800,1800),
                                           Confirming_Cost = c(1500,1500),
                                           Extra_Cost = c(4000,4000),
                                           Fibrosis_Rate = c(0.73,0.73),
                                           Compensate_Rate = c(0.7,0.7),
                                           Decompensate_Rate = c(min(Decompensate_Rate),max(Decompensate_Rate)),
                                           Total_HCC_Rate = c(0.53,0.53),
                                           Sens = 0.985
)

bound_Decompensate_Rate <- transpose(bound_Decompensate_Rate%>% 
                                       transmute(ICER_Treatment = ICER_r1_Inc_Cost /ICER_r1_Qal_gain))
bound_Decompensate_Rate <- bound_Decompensate_Rate%>% 
  mutate(UL_Difference = abs(V1 - V2))
colnames(bound_Decompensate_Rate) <- c("Lower_Bound", "Upper_Bound", "UL_Difference")

bound_Total_HCC_Rate <- icer_calculate2(Treatment_Cost = c(24000,24000),
                                        Screening_Cost = c(1800,1800),
                                        Confirming_Cost = c(1500,1500),
                                        Extra_Cost = c(4000,4000),
                                        Fibrosis_Rate = c(0.73,0.73),
                                        Compensate_Rate = c(0.7,0.7),
                                        Decompensate_Rate = c(0.53,0.53),
                                        Total_HCC_Rate = c(min(Total_HCC_Rate),max(Total_HCC_Rate)),
                                        Sens = 0.985
)

bound_Total_HCC_Rate <- transpose(bound_Total_HCC_Rate%>% 
                                    transmute(ICER_Treatment = ICER_r1_Inc_Cost /ICER_r1_Qal_gain))
bound_Total_HCC_Rate <- bound_Total_HCC_Rate%>% 
  mutate(UL_Difference = abs(V1 - V2))
colnames(bound_Total_HCC_Rate) <- c("Lower_Bound", "Upper_Bound", "UL_Difference")



df_all <- data.frame(rbind(bound_Treatment_Cost,
                           bound_Screening_Cost,
                           bound_Confirming_Cost,
                           bound_Extra_Cost,
                           bound_Fibrosis_Rate,
                           bound_Compensate_Rate,
                           bound_Decompensate_Rate,
                           bound_Total_HCC_Rate))

df_all <- as_tibble(cbind(Parameter=c("Treatment Cost",
                                      "Screening Cost",
                                      "Confirming Cost",
                                      "Extra Cost",
                                      "Fibrosis Rate",
                                      "Compensate Rate",
                                      "Decompensate Rate",
                                      "Total HCC Rate"),df_all))

base_ICER <- -0.4962831

order.parameters <- df_all %>% arrange(UL_Difference) %>%
  mutate(Parameter=factor(x=Parameter, levels=Parameter)) %>%
  select(Parameter) %>% unlist() %>% levels()

width <- 0.95

df_all_2 <-df_all %>% 
  
  # gather columns Lower_Bound and Upper_Bound into a single column using gather
  
  gather(key='type', value='output.value', Lower_Bound:Upper_Bound) %>%
  
  # just reordering columns
  
  select(Parameter, type, output.value, UL_Difference) %>%
  
  # create the columns for geom_rect
  
  mutate(Parameter=factor(Parameter, levels=order.parameters),
         ymin=pmin(output.value, base_ICER),
         ymax=pmax(output.value, base_ICER),
         xmin=as.numeric(Parameter)-width/2,
         xmax=as.numeric(Parameter)+width/2)

# Plotting 

ggplot() + 
  geom_rect(data = df_all_2, 
            aes(ymax=ymax, ymin=ymin, xmax=xmax, xmin=xmin, fill=type,alpha=type)) +
  theme_bw() + 
  theme(axis.title.y=element_blank(), legend.position = 'bottom',
        legend.title = element_blank()) + 
  geom_hline(yintercept = base_ICER) +
  scale_x_continuous(breaks = c(1:length(order.parameters)), 
                     labels = order.parameters)+
  scale_alpha_manual( values = c(0.7, 0.7))+
  coord_flip()

#####################ICER for risk groups ####################################
################# Plotting all the ICER values in one plot ####################################

WTP.5GDP <- 160000

colours =c("black") #,"blue","purple", "green", "pink","orange", "grey", "darkred","cyan2","blueviolet", "darkgoldenrod4","brown2","lawngreen")

#plot(summary_ICER_PSA_compareWORST_SIIL[1,2,],summary_ICER_PSA_compareWORST_SIIL[1,1,])

plot(0, xlim=c(-100000, 800000), ylim=c(axis_y1, 100000), xlab="Incremental DALYs",ylab="Incremental Costs THB (Million)", pch=18, col=colours,
     main="Incremental cost-effectiveness ratio (ICER) plane",yaxt="n",xaxt="n" )

abline(h=0,v=0) # Adds straight line to a plot


axis(side = 2,at=c(axis_y1,axis_y2,0,50000,100000),labels = c(as.character(axis_y1),as.character(axis_y2),"0","50,000","100,000"))
axis(side = 1,at=c(-100000,0,100000,200000,300000,400000,500000,600000,700000,800000),labels = c("-100000","0","100000","200000","300000","400000","500000","600000","700000","800000"))



qq<-c(-2000000,0,2000000)                               # Adjusting the scale to see difference in ICER values better for each scenario
ww<-qq*160000/(10^6)                                    # Adding in the threshold value that is 16000 TBH curently 
lines(qq,ww,col='brown4',lty=2)                         # Color of the line and adding in the line types 


points(347687,-3954, pch=20,col="green", cex=2)        # adds HIV                    1 point to the ICER
points(771068,-3684, pch=20,col="forestgreen", cex=2)           # adds IDU                    2 point to the ICER
points(284954,-4894, pch=20,col="greenyellow", cex=2)           # adds MSM                    3 point to the ICER
points(231529,-3237, pch=20,col="lightgreen", cex=2)           # adds HCV blood transfusion  4 point to the ICER
points(269050,-3208, pch=20,col="darkgreen", cex=2)       # adds Prisoners              5 point to the ICER

########################Coverage look###################################################


WTP.5GDP <- 160000

colours =c("black") #,"blue","purple", "green", "pink","orange", "grey", "darkred","cyan2","blueviolet", "darkgoldenrod4","brown2","lawngreen")

#plot(summary_ICER_PSA_compareWORST_SIIL[1,2,],summary_ICER_PSA_compareWORST_SIIL[1,1,])

plot(0, xlim=c(-100000, 800000), ylim=c(axis_y1, 100000), xlab="Incremental DALYs",ylab="Incremental Costs THB (Million)", pch=18, col=colours,
     main="Incremental cost-effectiveness ratio (ICER) plane",yaxt="n",xaxt="n" )

abline(h=0,v=0) # Adds straight line to a plot


axis(side = 2,at=c(axis_y1,axis_y2,0,50000,100000),labels = c(as.character(axis_y1),as.character(axis_y2),"0","50,000","100,000"))
axis(side = 1,at=c(-100000,0,100000,200000,300000,400000,500000,600000,700000,800000),labels = c("-100000","0","100000","200000","300000","400000","500000","600000","700000","800000"))



qq<-c(-2000000,0,2000000)                               # Adjusting the scale to see difference in ICER values better for each scenario
ww<-qq*160000/(10^6)                                    # Adding in the threshold value that is 16000 TBH curently 
lines(qq,ww,col='brown4',lty=2)                         # Color of the line and adding in the line types 


points(347687,-3954, pch=20,col="green", cex=2)        # adds HIV                    1 point to the ICER
points(344089,-3896, pch=20,col="green", cex=2)
points(337052,-3784, pch=20,col="green", cex=2)
points(317161,-3473, pch=20,col="green", cex=2)

points(771068,-3684, pch=20,col="forestgreen", cex=2)           # adds IDU                    2 point to the ICER
points(760744,-3630, pch=20,col="forestgreen", cex=2)
points(740573,-3526, pch=20,col="forestgreen", cex=2)
points(683588,-3236, pch=20,col="forestgreen", cex=2)

points(284954,-4894, pch=20,col="greenyellow", cex=2)           # adds MSM                    3 point to the ICER
points(282340,-4823, pch=20,col="greenyellow", cex=2)
points(277254,-4684, pch=20,col="greenyellow", cex=2)
points(262901,-4299, pch=20,col="greenyellow", cex=2)

points(231529,-3237, pch=20,col="lightgreen", cex=2)           # adds HCV blood transfusion  4 point to the ICER
points(229774,-3190, pch=20,col="lightgreen", cex=2)
points(226328,-3098, pch=20,col="lightgreen", cex=2)
points(216642,-2843, pch=20,col="lightgreen", cex=2)

points(269050,-3208, pch=20,col="darkgreen", cex=2)             # adds Prisoners              5 point to the ICER
points(266696,-3136, pch=20,col="darkgreen", cex=2)
points(262104,-3070, pch=20,col="darkgreen", cex=2)
points(249145,-2817, pch=20,col="darkgreen", cex=2)

#######Alternative way to look at coverage would be to screen all groups and then have the 100/50/25/10 coverages 

WTP.5GDP <- 160000

colours =c("black") #,"blue","purple", "green", "pink","orange", "grey", "darkred","cyan2","blueviolet", "darkgoldenrod4","brown2","lawngreen")

#plot(summary_ICER_PSA_compareWORST_SIIL[1,2,],summary_ICER_PSA_compareWORST_SIIL[1,1,])

plot(0, xlim=c(-100, 1500000), ylim=c(axis_y1, 20000), xlab="Incremental DALYs",ylab="Incremental Costs THB (Million)", pch=18, col=colours,
     main="Incremental cost-effectiveness ratio (ICER) plane",yaxt="n",xaxt="n" )

abline(h=0,v=0) # Adds straight line to a plot


axis(side = 2,at=c(axis_y1,axis_y2,0,50000,100000),labels = c(as.character(axis_y1),as.character(axis_y2),"0","50,000","100,000"))
axis(side = 1,at=c(-100000,0,100000,200000,300000,400000,500000,600000,700000,800000),labels = c("-100000","0","100000","200000","300000","400000","500000","600000","700000","800000"))



qq<-c(-2000000,0,2000000)                               # Adjusting the scale to see difference in ICER values better for each scenario
ww<-qq*160000/(10^6)                                    # Adding in the threshold value that is 16000 TBH curently 
lines(qq,ww,col='brown4',lty=2)                         # Color of the line and adding in the line types 


points(1421007,19020, pch=20,col="green", cex=2)        # adds HIV                    1 point to the ICER
points(1400362,18743, pch=20,col="yellow", cex=2)
points(1360055,18205, pch=20,col="orange", cex=2)
points(1246148,16711, pch=20,col="red", cex=2)


##means that if the coverage was altered then the should not fall below 25

