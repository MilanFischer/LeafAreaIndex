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

	# Latitude (degrees)
	latitude=Input_parameters$Latitude

	# Longitude (degrees)
	longitude=Input_parameters$Longitude

	# Longitude of the centre of the local time zone (degrees west from Greenwich)
	Lz=360-Input_parameters$TimeZoneCentreLongitude

	#Longitude of the measurement site (degrees west from Greenwich)
	Lm=360-longitude

	Zenith_angle=ZenithAngle(Timestamp)

	Transmitted=Input_data$Transmitted
	Incident=Input_data$Incident
	Beam_fraction=Input_data$Beam_fraction
	
	LeafAbsorption=Input_parameters$LeafAbsorption
	ELADP=Input_parameters$LeafAngleDistnParameter
	
	# LAI estimate through optimization of the light penetration model
	LAI=numeric()
	for(i in 1:length(Transmitted))
	{
	LAI[i]=optim(par=0,function(LAI){LightPenetration(Zenith_angle[i],LAI,Beam_fraction[i],Transmitted[i],Incident[i])},
	method='Brent',lower=0,upper=50)$par[1]
	}


