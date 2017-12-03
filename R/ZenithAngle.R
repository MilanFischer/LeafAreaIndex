ZenithAngle<-function(Timestamp)
{

#Date
Date=as.POSIXlt(as.character(Timestamp),origin = '01/01/1970 00:00:00.1',format='%m/%d/%Y %H:%M',tz='UTC')
DOY<-as.numeric(strftime(Date, format = '%j',tz='UTC'))
Year<-as.numeric(strftime(Date, format = '%Y',tz='UTC'))
Minute<-as.numeric(strftime(Date, format = "%M",tz="UTC"))
Hour<-as.numeric(strftime(Date, format = '%H',tz='UTC'))+Minute/60

#Decide wheather the year is leap or not
Leap<-Year %% 4 == 0 & (Year %% 100 != 0 | Year %% 400 == 0)

#Number of day in year
nd<-rep(NA,length(Leap))

for(i in 1:length(Leap)){
if(Leap[i]==TRUE){
nd[i]=366
}else{
nd[i]=365
}
}

#Inverse relative distance Earth-Sun
dr=1+0.033*cos(2*pi/nd*DOY)

#Solar declination (radians)
delta=0.409*sin(2*pi/nd*DOY-1.39)

#Latitude (degrees)
latitude=Input_parameters$Latitude

#Longitude (degrees)
longitude=Input_parameters$Longitude

#Latitude (radians)
latitude_r=pi/180*latitude

#Parameter for seasonal correction of solar time
b=2*pi*(DOY-81)/364

#Seasonal correction for solar time
Sc=0.1645*sin(2*b)-0.1255*cos(b)-0.025*sin(b)

#Longitude of the centre of the local time zone (degrees west from Greenwich)
Lz=360-Input_parameters$TimeZoneCentreLongitude

#Longitude of the measurement site (degrees west from Greenwich)
Lm=360-longitude

#Solar time angle
w=pi/12*((Hour+0.06667*(Lz-Lm)+Sc)-12)

#Zenith angle (degrees)
zenith_angle=acos(sin(latitude_r)*sin(delta)+cos(latitude_r)*cos(delta)*cos(w))*180/pi

return(zenith_angle)
}
