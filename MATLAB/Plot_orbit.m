%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Produce the plot of the orbit of a star projected on different frames of
% reference. In the case of a single bar, the user can choose to plot the
% orbit in the non-inertial fame of reference corotating with the bar.
%
% Coded by L.J. Rossi (2014).
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
close all
whitebg('k')

% Read the inputN.dat file, defining the mass model
Read_inputN

disp (' Insert the name of the input file: ')
Name_new = input('Input file = ','s');

if MBAR1 > 0 && MBAR2 == 0 && DELTA_SP == 0
    disp('Choice of the frame of reference:')
    disp('1 = Inertial frame of reference') 
    disp('2 = Non-inertial corotating frame of reference')
    Style = input('Style = ');
else
    Style = 1;
end
    
% Read the input file with the ephemeris.
if exist('Name') == 0 || strcmp(Name,Name_new) == 0
    
    data = load (Name_new);
    t = data(:,1);
    x = data(:,2);
    y = data(:,3);
    z = data(:,4);
    vx = data(:,5);
    vy = data(:,6);
    vz = data(:,7);
    R = sqrt(x.^2 + y.^2 + z.^2);

	Name = Name_new;
end

% Transform the coordinates in the corotating frame of reference
if Style == 1
    
    l = 1;
    clear VX X
    for i = 2 : length(t)        
        if y(i)>0 && y(i-1)<0
            X(l) = x(i);
            VX(l) = vx(i);
            l = l+1;
        end
    end
    
elseif Style == 2
    
    xr = cos(PHI0_B1+OMEGA_B1*t).*x-sin(PHI0_B1+OMEGA_B1*t).*y;
    yr = +sin(PHI0_B1+OMEGA_B1*t).*x+cos(PHI0_B1+OMEGA_B1*t).*y;
    zr = z;
    vxr = vx - OMEGA_B1*y;
    vyr = vy + OMEGA_B1*x;
    vzr = vz;
    R=sqrt(xr.^2+yr.^2);
    
    l = 1;
    clear VX X
    for i = 2 : length(t)        
        if yr(i)>0 && yr(i-1)<0
            X(l) = xr(i) ;
            VX(l) = vxr(i);
            l = l+1;
        end
    end
                  
end

% Plot the results. 

if Style == 1
    
    subplot(2,2,1),  plot(x,y,'w')
    axis square
    set(gca,'fontsize',7)
    axis([min(x)-2 max(x)+2 min(x)-2 max(x)+2])
    xlabel('x (kpc)','fontsize',8)
    ylabel('y (kpc)','fontsize',8)
        
    subplot(2,2,2),  plot(x,z,'w')
    axis square
    set(gca,'fontsize',7)
    axis([min(x)-2 max(x)+2 min(x)-2 max(x)+2])
    xlabel('x (kpc)','fontsize',8)
    ylabel('z (kpc)','fontsize',8)
    
    subplot(2,2,3), plot(R,z,'w')
    axis square
    set(gca,'fontsize',7)
    xlabel('R (kpc)','fontsize',8)
    ylabel('z (kpc)','fontsize',8)
    axis([min(R)-2 max(R)+2 min(z)-0.5 max(z)+0.5])
    
    subplot(2,2,4),  plot(X,VX,'w.')
    axis square
    set(gca,'fontsize',7)
    xlabel('x (kpc)','fontsize',8)
    ylabel('v_x (km s^{-1})','fontsize',8)
    axis([min(X)-2 max(X)+2 min(VX)-50 max(VX)+50])
    
elseif Style == 2
        
    subplot(2,2,1),  plot(xr,yr,'w')
    hold on
    Xb=linspace(-ABAR1,ABAR1,1000);
    Yb_up=BBAR1*sqrt(1-Xb.^2/ABAR1^2);
    Yb_down=-BBAR1*sqrt(1-Xb.^2/ABAR1^2);
    plot(Xb,Yb_up,'r')
    plot(Xb, Yb_down,'r')
    
    axis square
    set(gca,'fontsize',7)
    axis([min(x)-2 max(x)+2 min(x)-2 max(x)+2])
    xlabel('x (kpc)','fontsize',8)
    ylabel('y (kpc)','fontsize',8)
    

    subplot(2,2,2), plot(xr,zr,'w')
    hold on
    Xb=linspace(-ABAR1,ABAR1,1000);
    Yb_up=CBAR1*sqrt(1-Xb.^2/ABAR1^2);
    Yb_down=-CBAR1*sqrt(1-Xb.^2/ABAR1^2);
    plot(Xb,Yb_up,'r')
    plot(Xb, Yb_down,'r')
    
    axis square
    set(gca,'fontsize',7)
    axis([min(x)-2 max(x)+2 min(x)-2 max(x)+2])
    xlabel('x (kpc)','fontsize',8)
    ylabel('z (kpc)','fontsize',8)
    
    subplot(2,2,3), plot(R,z,'w')
    axis square
    set(gca,'fontsize',7)
    xlabel('R (kpc)','fontsize',8)
    ylabel('z (kpc)','fontsize',8)
    axis([min(R)-2 max(R)+2 min(z)-0.5 max(z)+0.5])
    
    subplot(2,2,4),  plot(X,VX,'w.')
    axis square
    set(gca,'fontsize',7)
    xlabel('x (kpc)','fontsize',8)
    ylabel('v_x (km s^{-1})','fontsize',8)
    axis([min(X)-2 max(X)+2 min(VX)-50 max(VX)+50])
    

end 

