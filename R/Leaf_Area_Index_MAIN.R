##############################################################################################
#                                                                                            #
#      -------------------------------------------------                                     #
#       Computing leaf area index from PAR measurements                                      #
#      -------------------------------------------------                                     #
#                                                                                            #
#       Author: Milan Fischer                                                                #
#       email: fischer.milan@gmail.com                                                       #
#                                                                                            #
##############################################################################################


	rm(list=ls())
	source('BeamFraction.R')
	source('LightPenetration.R')
	source('ZenithAngle.R')

	# Move one directory up
	setwd('..'); MainWD_link=getwd()

	# Move to directory with the data
	setwd(paste(getwd(),'/Data',sep=''))

	# Create a list of input data files
	Input_data_file<-list.files(path=getwd(),pattern=c('*Data*','\\.dat$'))
	Input_parameters_file<-list.files(path=getwd(),pattern=c('*Parameters*','\\.dat$')) 

	# Read the input data files
	Input_data<-read.table(Input_data_file,sep=',',header=TRUE)
	Input_parameters<-read.table(Input_parameters_file,sep=',',header=TRUE)

	Timestamp=Input_data$Time
	Transmitted=Input_data$Transmitted
	Incident=Input_data$Incident
	Beam_fraction=Input_data$Beam_fraction
	LeafAbsorption=Input_parameters$LeafAbsorption
	ELADP=Input_parameters$LeafAngleDistnParameter
	Zenith_angle=ZenithAngle(Timestamp)

	Beam_fraction=BeamFraction(Incident,Zenith_angle)
	
	# LAI estimate through optimization of the light penetration model
	LAI=numeric()
	for(i in 1:length(Transmitted))
	{
	LAI[i]=optim(par=0,function(LAI){LightPenetration(Zenith_angle[i],LAI,Beam_fraction[i],Transmitted[i],Incident[i])},
	method='Brent',lower=0,upper=50)$par[1]
	}

	write.table(data.frame(Timestamp,LAI),paste(MainWD_link,'/Output/LAI_data.csv',sep=''),sep=',',row.names=FALSE,col.names=TRUE)

