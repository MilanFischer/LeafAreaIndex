# Light penetration model

LightPenetration<-function(Zenith_angle,LAI,Beam_fraction,Transmitted,Incident)
{
	Zenith_angle_rad=Zenith_angle/180*pi
	
	# Extinction coefficient
	K=(sqrt(ELADP^2+tan(Zenith_angle_rad)^2))/(ELADP+1.702*(ELADP+1.12)^-0.708)
	
	# Transmitted fraction of incident direct light
	tau_dir=exp(-K*LAI)

	A=1/(0.15*ELADP^1.38+0.007)
	B=4.32+2.6*exp(-2.75*ELADP)
	C=0.57-0.23*exp(-1.4*ELADP)

	# Transmitted fraction of incident diffuse light
	tau_diff=exp(-LAI)+A*LAI^3*exp(-B*LAI^C)

	P=1+0.4*exp(-0.1*ELADP)*(atan(0.9*ELADP)-0.95)
	Q=0.255*atan(ELADP)+0.6
	R=exp(-ELADP)

	tau_sphere=exp(-P*(LAI^Q+R*log(1+LAI)))
	g_dirr=exp(-1.5*ELADP)*(-0.2+0.7*Zenith_angle_rad^2)+0.2*Zenith_angle_rad^5+0.3
	g_diff=0.5
	LAdiff=LAI*(1-g_diff*(1-LeafAbsorption))
	LAdir=LAI*(1-g_dirr*(1-LeafAbsorption))
	tau_direct=Beam_fraction*exp(-K*(1-g_dirr*(1-LeafAbsorption))*LAI)
	tau_diffuse=(1-Beam_fraction)*(exp(-LAdiff)+A*LAdiff^3*exp(-B*LAdiff^C))
	tau_total=tau_direct+tau_diffuse
	tau_total_target=Transmitted/Incident

	abs(tau_total_target-tau_total)
}