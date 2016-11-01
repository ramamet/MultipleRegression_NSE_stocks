#+++++++++++++++++++++++++++++++++++++++++++++++++++++
# Multiple Regression on National Stock Exhange (NSE), India data from yahoofinance
# NSE bank stocks vs NIFTY50 
# Predictive analytics

#Author: Ramanathan Perumal
#Mail: ramamet4@gmail.com
#Modified: 20/08/2016

#++++++++++++++++++++++++++++++++++++++++++++++++++++

	  #-----------------------------------------------------
	  # Step-0: Problem statement
	  #-----------------------------------------------------
	  
	  # In this study, I try various sector of stocks and their influence on NIFTY50 index movement.
	  # For a better optimization, model has been iterated over many different configurations.
          # Main focus will be on how some stocks are important to monitor.
             
	  

	  #-----------------------------------------------------
	  # Step-1: Collecting data
	  #-----------------------------------------------------

#Library
library(lubridate)
library(ggplot2)
library(quantmod)
library(reshape2)
library(RColorBrewer)
library(arules)
library(dplyr)
library(arulesViz)
library(colorspace)
library(readxl)


	  
#timeline
START=c("2015-10-28")
END=c("2016-10-28")

   #NSEscreener.xlsx
   df=read.csv("ind_niftybanklist.csv")
   #df=data.frame(df0)
   df=mutate(df,id=paste0(Symbol,".NS",sep=""))
   df=na.omit(df)
   df1=df
   #df[,11]=as.numeric(as.character(df[,11]))
   #df=filter(df,Market.Cap>0)
   #df=filter(df,Close>50)
   
#    df1=filter(df1,Symbol!="WELSPUNIND")
   
   
  #NIFTY data collection
  getSymbols("^NSEI",from=START,to=END,adjust=TRUE)
  nifty <- (Cl(get("NSEI")))
  #nifty=nifty[-(1:15),]
  nifty <- data.frame(Date=as.POSIXct(index(nifty)), nifty)
  nifty$Date= as.Date(nifty$Date)
  rownames(nifty)=NULL
  colnames(nifty)[2]="nifty"
  
  #df2=df1[1:10,]
  #df2=df1
  symbols=df1$id
  getSymbols(symbols[1:length(symbols)],from=START,to=END,adjust=TRUE)
  
  prices0 <- (Cl(get(symbols[1])))
    
    for(i in 2:length(symbols))
    prices0 <- merge(prices0,(Cl(get(symbols[i]))))   
    colnames(prices0) <- symbols
        
    
    	#-----------------------------------------------------
	# Step-2: Data cleaning 
	#-----------------------------------------------------
    
    df <- data.frame(Date=as.POSIXct(index(prices0)), prices0)
    df$Date= as.Date(df$Date)
    colnames(df)=gsub(".NS","",colnames(df))
    rownames(df)=NULL
    #df=df[-(1:15),]
    
    #+++++++++++++
    ram=merge(nifty,df,id="Date")
       
    dff1=ram[,-1]
    
    # visulizin the relationships among features - pair correlations
    library(psych)   
    png("png/pairpanel_bank.png", width = 10, height = 10, units = 'in', res = 200)
    pairs.panels(dff1)
    dev.off()
    
    
     #weekday relationships
     dff2=mutate(ram,wDay=wday(Date,label=T))	
     dff2=mutate(dff2,wNumb=wday(Date))	
     dff2$wNumb= as.numeric(as.character(dff2$wNumb))
     dff2=dff2[order(dff2$wNumb), ]
      
     png("png/weekDay.png", width = 10, height = 10, units = 'in', res = 200)	
     p=ggplot(dff2, aes(x=wDay, y=nifty)) +
         geom_boxplot(color="black",fill="darkgreen",alpha=0.4) +
         theme_bw()+
         theme(axis.text.x = element_text(angle = 45, hjust = 1))        
     print(p)
     dev.off() 
     
     #month relationships
     dff3=mutate(ram,mDay=month(Date,label=T))	
     dff3=mutate(dff3,mNumb=month(Date))	
     dff3$mNumb= as.numeric(as.character(dff3$mNumb))
     dff3=dff3[order(dff3$mNumb), ]
      
     png("png/month.png", width = 10, height = 10, units = 'in', res = 200)	
     p2=ggplot(dff3, aes(x=mDay, y=nifty)) +
         geom_boxplot(color="black",fill="darkgreen",alpha=0.4) +
         theme_bw()+
         theme(axis.text.x = element_text(angle = 45, hjust = 1))        
     print(p2)
     dev.off()   
           
        #-----------------------------------------------------
	# Step-3: Training a model on the data
	#-----------------------------------------------------
	#Building the model
	# m <- lm(dv ~ iv, data=mydata)
	# dv- dependent variable
	# iv- independent variable
	# mydata- selected dataframe
		
	nse_model <- lm(nifty ~ . , data= dff1)
	summary(nse_model)
	       
 
         #------------------------------------------------
	# Step-4: Improving model performance 
	#-----------------------------------------------
	
	#Selecting predictors
	#Stepwise regression algorithms
	
              library(MASS)
			    
	      #Get the Independent Variables
	      #(and exclude hp dependent variable)
	      indep_vars <-paste(names(dff1)[-which(names(dff1)=="nifty")],
				collapse="+")
	      #Turn those variable names into a formula
	      upper_form = formula(paste("~",indep_vars,collapse=""))
	      #~mpg + cyl + disp + drat + wt + qsec + vs + am + gear + carb
              
	      mod <- lm(nifty~.,data=dff1)
	      #Step Backward and remove one variable at a time
	      reg_back = stepAIC(mod,direction = "backward",trace = T)
	      
	      #Create a model using only the intercept
	      mod_lower = lm(nifty~1,data=dff1)
	      #Step Forward and add one variable at a time
	      reg_forward = stepAIC(mod_lower,direction = "forward",
		      scope=list(upper=upper_form,lower=~1))
		      
	      #Step Forward or Backward each step starting with a intercept model
	      reg_both = stepAIC(mod_lower,direction = "both",
		      scope=list(upper=upper_form,lower=~1))
        
        #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        
        
        
