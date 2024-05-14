source(here::here("utilities/prob_dist.R"))

## Function to simulate (estimate) variables
## Arguments: outputData := Output Dataframe to be merged with inputData
##            inputData  := Input Dataframe with metadata of variables to be simulated (estimated) 
##            N          := Number of simulation to be performed for each variable

estimate_variables <- function(outputData, input, N){
  
  for(i in 1:nrow(input)){
    
    if(is.na(input$Distribution[i]) | input$Distribution[i] =="None" ){
      outputData$variable <-0
      
    }else if (input$Distribution[i] == "Point estimate"){
      outputData$variable <- input$Other[i]
      
    }else if (input$Distribution[i]== "Uniform"){
      outputData$variable <- runif(N,input$Min[i],input$Max[i])
      
    }else if (input$Distribution[i]=="Triangular"){
      outputData$variable <- mc2d::rtriang(N, min=input$Min[i], mod=input$Mean_Mode[i], max=input$Max[i])
      
    }else if (input$Distribution[i]== "Pert"){
      outputData$variable <- rpert(N, input$Min[i], input$Mean_Mode[i], input$Max[i])
      
    }else if (input$Distribution[i] =="Normal"){
      if(input$Truncated[i]=="Yes"){
        outputData$variable <- rnormt(N, min=input$Min[i], max=input$Max[i], input$Mean_Mode[i], input$SD[i])
      }else {
        outputData$variable <- rnorm(N, input$Mean_Mode[i], input$SD[i])
      }
      
    }else if (input$Distribution[i]== "Beta"){
      if(input$Truncated[i]=="Yes"){
        outputData$variable <- rbetat(N,min=input$Min[i], max=input$Max[i],shape1=input$Other[i],shape2=input$Other2[i])
      }else {
        outputData$variable <- rbeta(N,shape1=input$Other[i],shape2=input$Other2[i])
      }
      
    }else if (input$Distribution[i]== "Poisson"){
      if(input$Truncated[i]=="Yes"){
        outputData$variable <- rpoist(N, min=input$Min[i], max=input$Max[i], input$Other[i])
      }else {
        outputData$variable <- rpois(N, input$Other[i])
      }
      
    }else if (input$Distribution[i]== "Negbin"){
      outputData$variable <- rnegbin(N, mu=input$Mean_Mode[i], theta=input$Other[i])
      
    }else if (input$Distribution[i]== "Binomial"){
      outputData$variable <- rbinom(N, 1, prob=input$Other[i])
      
    }else if (input$Distribution[i] =="Laplace"){
      if(input$Truncated[i]=="Yes"){
        outputData$variable <- rtrunclaplace(N, mean=input$Mean_Mode[i], scale=input$Other[i], lower=input$Min[i], upper=input$Max[i])
      }else {
        outputData$variable <- rlaplace(N, mu=input$Mean_Mode[i], sigma=input$Other[i])
      }
      
    }else if (input$Distribution[i] =="LogNormal" ){
      if(input$Truncated[i] =="Yes"){
        outputData$variable <- rlnormTrunc(N, meanlog=input$Mean_Mode[i], sdlog=input$SD[i], min=input$Min[i], max=input$Max[i])
      }else {
        outputData$variable <- rlnorm(N, meanlog=input$Mean_Mode[i], sdlog=input$SD[i])
      }
      
    }else if (input$Distribution[i] =="LogLogistic" ){
      if(input$Truncated[i] =="Yes"){
        outputData$variable <- rllogt(N, shape=input$Other[i], scale=input$Other2[i], min=input$Min[i], max=input$Max[i])
      }else {
        outputData$variable <- rllog(N, shape=input$Other[i], scale=input$Other2[i])
      }
      
    }else if (input$Distribution[i] =="Shifted LogLogistic" ){
      if(input$Truncated[i] =="Yes"){
        outputData$variable <-  rllog3t(N, shape=input$Other[i], scale=input$Other2[i], thres=input$Other3[i], min=input$Min[i], max=input$Max[i])
      }else {
        outputData$variable <- rllog3(N, shape=input$Other[i], scale=input$Other2[i], thres=input$Other3[i])
      }
      
    }else if (input$Distribution[i] =="Gamma"){
      if(input$Truncated[i] =="Yes"){
        outputData$variable <- rgammat(N, min=input$Min[i], max=input$Max[i], shape=input$Other[i], scale=input$Other2[i])
      }else {
        outputData$variable <- rgamma(N, shape=input$Other[i], scale=input$Other2[i])
      }
      
    }
    
    colnames(outputData)[ncol(outputData)]<- input$Variable[i] 
    
  }
  
  outputData
}

