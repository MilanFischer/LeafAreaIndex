BeamFraction<-function(Incident,Zenith_angle)
{

# PAR solar constant (µmol m-2 s-1)
PAR_solar_constant=2550

Zenith_angle_rad=Zenith_angle/180*pi
PAR_potential=PAR_solar_constant*cos(Zenith_angle_rad)

# Fraction of potential PAR reaching the probe
r=ifelse(Incident/PAR_potential>=0.82,0.82,ifelse(Incident/PAR_potential<=0.2,0.2,Incident/PAR_potential))

b=1.395+r*(-14.43+r*(48.57+r*(-59.024+r*24.835)))

Beam_fraction=ifelse(Zenith_angle_rad>1.5,0,b)

return(Beam_fraction)
}