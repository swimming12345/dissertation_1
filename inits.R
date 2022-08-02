# S <-reactive({
#   1*(parms()$P0 - caset0)
# })

P0 <- 61623143
caset0 <- 0.03*P0

inits <- c(
    # 1*(parms()$P0 - caset0)
    S=1*(P0-caset0),
    F0=0.2825*caset0,
    F1=0.2825*caset0,
    F2=0.184*caset0,
    F3=0.124*caset0,
    C1=0.03175*caset0,
    C2=0.03175*caset0,
    C3=0.03175*caset0,
    C4=0.03174*caset0,
    HCC_A=0,
    HCC_B=0,
    HCC_C=0,
    HCC_D=0,
    D=0,
    dthC14=0
    ,dthHCC=0
    ,C1std_cured=0,
    C1new_cured=0,
    C2new_cured=0,
    C3new_cured=0,
    C4new_cured=0
    #set up initial death
    
    
  )


