%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Produce the plot of the configuration of the particles at different
% times. The user can chose between a 2D plot, a 3D plot and a smoothed
% hostigram showing the distribution of the particles projected on the
% galactic plane. The input files have to be in the current working directory.
%
% Coded by L.J. Rossi (Melbourne, 2014).
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
close all
whitebg('k')

disp (' Insert the name of the input file: ')
Name_new = input('Input file = ','s');

disp('Choice of the plot style:')
disp('1 = 3D plot')
disp('2 = Smoothed histogram plot')
Style = input('Style = ');

if Style == 2
    disp('Choice of the positive scalar smoothing parameter Lambda:')
    disp('higher values lead to more smoothing, values close to zero lead')
    disp('to a plot that is essentially just the raw data')
    Lambda = input('Lambda = ');
end
    
% Read the input Orbits.dat with the ephemeris, and separates it in
% matrices. Each of them contains the coordinates and velocities of the 
% paricles at each output time chosen by the user.
if exist('Name') == 0 || strcmp(Name,Name_new) == 0
    
    data = load (Name_new);
    t = data(:,1);
    t(length(t)+1) = max(t)+1;
    n = length(t);
    k = 1;
    l = 1;

    for i = 2 : n    
        if t(i)>t(i-1)        
            eval(['Snap_' num2str(l)  '= data(k:i-1,:);']);
            k = i;
            T(l) = t(i-1);
            l = l+1;      
        end    
    end
l = l-1;
Name = Name_new;

end

% Plot the results. 
N_plot = input('Choice of the number of snapshots to be plotted. The number must be even. N = ');
step = floor(l/N_plot);
c = 1;

for i=1:step:l
    
    if c <= N_plot
        M=eval(['Snap_' num2str(floor(i))]);   
    
        x = M(:,2);
        y = M(:,3);
        z = M(:,4);   
    
        X = [x y];
        
        if Style == 1            
            subplot(2,N_plot/2,c),  plot(x,y,'wo','markersize',0.001)
            title(['t=',num2str(T(i)/1000) ' Gyr'],'fontsize',8);    
            axis square
            set(gca,'fontsize',7)
            axis([-20 20 -20 20 -20 20])
            xlabel('x (kpc)','fontsize',8)
            ylabel('y (kpc)','fontsize',8)
            zlabel('z (kpc)','fontsize',8)
            c = c+1;
        
        elseif Style == 2
            subplot(2,N_plot/2,c),  smoothhist2D(X,Lambda, [1000, 1000],[],'surf'); 
            title(['t=',num2str(T(i)/1000) ' Gyr'],'fontsize',8);    
            axis square
            set(gca,'fontsize',7)
            axis([-20 20 -20 20])
            grid off
            box on
            xlabel('x (kpc)','fontsize',8)
            ylabel('y (kpc)','fontsize',8)
            c = c+1;
        
        else
            disp('Plot style not recognized!!')
        end

    end
end 

