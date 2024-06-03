%----------------------------------------------------------------------
%Ecosystem services along ecological network seascapes
%Guimaraes&Melian@KB May 2024
%----------------------------------------------------------------------
%function DynS()  

    show=true;
    showEach = 1;
    countheat = 0;
    countgen = 0;
    heat = zeros(1,5);
    %gamma=[];
    T=100;%time    
    %As = [0.00625 0.0125 0.025 0.05 0.075 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1];%amplitude
    %GPTs =[100 500 1000]; % GPT=1 is Static 1 5 10 50 
    %GPTs = [0.1 0.5 1 5 10 50 100 500 1000 2000 5000 10000 12000 15000 20000];
    As = linspace(0.0001,10,100);
    GPTs = linspace(0.001,20000,100);
    
    
    %Short test
    %As = [0.025 0.05 0.075 0.1 0.2];%amplitude
    %GPTs =[100 500 1000]; % GPT=1 is Static 1 5 10 50 
    %GPTs = [1 5 10 50 100];
    
    
    R_k={};  %Composicion de species @ each Site and each Generation k  
    countAs = 0;
   
 for i = 1:length(As); % i refersw to values of amplitude
    countAs = countAs + 1;
    for j = 1:length(GPTs); % j refers to values of frequency


%Niche overlap matrix
v = 0.1; % variance
sigma = sqrt(v); % standard deviation
mu = 0.1; % mean
%n = 1000;% pairs
N = 50;%Number species
S = zeros(N,N);
for s = 1:N;
    for s1 = s+1:N;
        X(s,s1) = sigma .* randn(1, 1) + mu;
        if X(s,s1) < 0;
           X(s,s1) = -1*(X(s,s1));
           X(s1,s) = -1*(X(s,s1));
        end
    end
end

%size(X)
%pause
%index = find(X>0.3);
%numberOfElements = length(index)
%Check mean-std niche overlap matrix
%X
%pause        
%stats = [mean(X) std(X) var(X)]
%hist(X)


        
        %give the parameters a specific combination of values 
        A = As(1,i);
        GPT = GPTs(1,j); 
        f = 1/GPT;
        r=zeros(T,4);
    
        for k = 1:T;  %Generations...one combination A,f        
        %A = 100;%amplitude, is the peak deviation: 
        %350 to match simulations in random landscapes
        %f = 1;%ordinary frequency, number of 
        %cycles that occur each second of time
        sig = 0;%the phase
        countgen = countgen + 1;
        %r = A*sin(2*pi*f*countgen + sig) + A;%starting point with r approx.
        r(k,2) = A/2*(sin(2*pi*f*countgen + sig)+1);
        r(k,1) = k;
        %2. Check sinusoidal with boundary conditions considering continuous A and f
        %Check r_min == 0 and r_max == max distance ij
        count = 0;
        %Counting number of species present at time t
        for p = 1:N;
            %find(X(:,r(k,2))); 
            %X(p,:)
            %e = r(k,2)
            presence = find(X(p,:)>r(k,2));
            if ~isempty(presence);
            %p
              count = count + 1;
               NSt = count;
               %pause
            end
            
            
        end
        NSt;
        r(k,3) = NSt;
        
        
%boolean networks -- following the species presence at time k
        
 %% 1- basic simulation features/parameters
%environment=1; % no fluctuating environment, 1 fluctuacting environment
%S=29; %species richness == now number of species dp of environmental state -- NSt
E= 1; %number of ecosystem services
dim=NSt+E;%S+E; %matrix size
p=0.2; %connectance
nsims=1; %number of simulations
scenario=4; %1- random, 2 - reciprocal negative, 3- trophic layer (2 layers) + consumer service, 4- trophic layer (2 layers) + top service
tfim=10; %final timestep
theta=randn(1,1); % environmentgal optima
resultadosfinais=zeros(nsims,4); %store results
%1- mean species richness
%2 - variance in species presence
%3 - mean ecosystem service supply
%4 - variance in ecosystem service supply

for simulacoes=1:nsims;
    Resultados=zeros(tfim,dim); %record results from a similuation

    kE=3; %number of species directly supporting the ecosystem service
    ninteractions=round(p*NSt*(NSt-1)); %number of interactions in both matrices.
    nintp=round(ninteractions)./2; %number of positive effects
    nintn=round(ninteractions)./2; %number of negative effects
    % Interactions are directed links
    % in both matrices below the last rows/columns describe the ecosystem
    % services
    matrizp=zeros(dim); %matrix of positive effects, columns affect rows
    matrizn=zeros(dim); %matrix of negative effects, columns affect rows
    z=randn(NSt,1); %traits of the species
    estados1=round(rand(dim,1)); %all nodes start on
    cp=zeros(dim,1); %cutoff for positive values
    cn=zeros(dim,1); %cutoff for negative values
    mp=zeros(dim,1); %record positive ms
    mn=zeros(dim,1); %record negative ms
    %a - three distinct network structures
    cp(end)=2;

    %% random distribution of links
    if scenario==1;

        while nintp>0
            x=randi(NSt);
            y=randi(NSt);
            if x ~= y
                if matrizp(x,y)==0
                    matrizp(x,y)=1;
                    nintp=nintp-1;
                end
            end
        end

        for l=1:NSt
            x=randi(sum(matrizp(l,:))+2);
            cp(l)=x-1;
        end

        %assign interactions among species fueling ecosystem services - for sake of
        %simplicity we are not assuming feedbacks between ecosystem services and
        %species presences or direct negative effects of species on ecosystem
        %services

        while kE>0
            y=randi(NSt);
            if matrizp(end,y)==0

                matrizp(end,y)=1;
                kE=kE-1;
            end
        end

        %assign negative interactions among species in the positive matrix
        while nintn>0
            x=randi(NSt);
            y=randi(NSt);
            if x ~= y
                if matrizp(x,y)==0
                    if matrizn(x,y)==0
                        matrizn(x,y)=1;
                        nintn=nintn-1;
                    end
                end
            end
        end
    end
    for m=1:NSt
        x=randi(sum(matrizn(m,:))+2);
        cn(m)=x-1;
    end

    %% reciprocal interactions (+/-)
    if scenario==2
        layer1=round(NSt/2);
        while nintp>0
            x=randi(NSt);
            y=randi(NSt);
            if matrizp(x,y)==0
                if matrizp(y,x)==0
                    matrizp(x,y)=1;
                    matrizn(y,x)=1;
                    nintp=nintp-1;
                end
            end
        end

        for v=1:NSt
            x=randi(sum(matrizp(v,:))+2);
            cp(v)=x-1;
        end

        for vv=1:NSt
            x=randi(sum(matrizn(vv,:))+2);
            cn(vv)=x-1;
        end
    end

    %trophic layer - bottom up ecosystem service
    if scenario==3
        %split the matrices in two
        layer1=round(NSt/2);
        while nintp>0
            x=randi(NSt-layer1);
            y=randi(layer1);
            if matrizp(layer1+x,y)==0
                matrizp(layer1+x,y)=1;
                matrizn(y,layer1+x)=1;
                nintp=nintp-1;
            end
        end
        for n=layer1+1:NSt
            if sum(matrizp(n,:))>0
                x=randi(sum(matrizp(n,:)));
            else
                x=0;
            end

            cp(n)=x;
        end

        for d=1:NSt
            x=randi(sum(matrizn(d,:))+2);
            cn(d)=x-1;
        end
        while kE>0
            y=randi(layer1);
            if matrizp(end,y)==0

                matrizp(end,y)=1;
                kE=kE-1;
            end
        end
    end
    %two layers - topdown ecosystem service
    if scenario==4
        layer1=round(NSt/2);
        while nintp>0
            x=randi(NSt-layer1);
            y=randi(layer1);
            if matrizp(layer1+x,y)==0
                matrizp(layer1+x,y)=1;
                matrizn(y,layer1+x)=1;
                nintp=nintp-1;
            end
        end
        for e=layer1+1:NSt
            if sum(matrizp(e,:))>0
                x=randi(sum(matrizp(e,:)));
                cp(e)=x;
            else
                cp(e)=0;
            end

        end

        for e=1:NSt
            x=randi(sum(matrizn(e,:))+2);
            cn(e)=x-1;
        end
    end

    simulacoes; % tell the number of the simulations

    %% simulation
    for t=1:tfim;
        t;
        Resultados(t,:)=estados1'; %record the state of the system
        estadoanterior=estados1; %store the current state


        %% compute the positive and negative effects
        for ii = 1:dim;
            mp(ii)=matrizp(ii,:)*estadoanterior;
            if mp(ii)>=cp(ii)
                mp(ii)=1;
            else
                mp(ii)=0;
            end
            mn(ii)=matrizn(ii,:)*estadoanterior;

            if mn(ii)>cn(ii)
                mn(ii)=0;
            else
                mn(ii)=1;
            end

        end

        for ii=1:dim;
            estados1(ii)=mp(ii).*mn(ii);
        end



        %%%fluctuating environment
        %if environment==1
        %    theta=theta+0.1.*randn(1,1);
        %    for i=1:S
        %        x=exp(-2.*((theta-z(i)).*(theta-z(i))));
        %        q=rand(1,1);
        %        if q<(1-x)
        %            estados1(i)=0;
        %        end
        %    end
        %end

    end

        especiesd=Resultados(:,1:end-1);
        riqueza=sum(especiesd,2);


    resultadosfinais(simulacoes,1)=mean(riqueza);
    resultadosfinais(simulacoes,2)=var(riqueza);
    resultadosfinais(simulacoes,3)=mean(Resultados(:,end));
    resultadosfinais(simulacoes,4)=var(Resultados(:,end));
     r(k,4) = mean(Resultados(:,end));
    
    
end
        %----------------------------------------------------------
        
        end%k
        A
        f
        countheat = countheat + 1;
      heat(countheat,1) = A; 
      heat(countheat,2) = f;  
      heat(countheat,3) = mean(r(k,3)); 
      heat(countheat,4) = std(r(k,3));
      heat(countheat,5) = mean(r(k,4));%Services
      %pause
        %subplot(1,1,1)
        %plot(r(:,1),r(:,2))
        %hold on
        %title("A")
        %xlabel('Time',"fontsize",26)
        %ylabel('Environmental threshold',"fontsize",26)
        %set(gca,'fontsize',24);
        %print -color -F:20 Fig5a.eps
        %subplot(1,1,3)
        
        %plot(r(:,1),r(:,3))
        %hold on
        %axis([0 1000 0 50])%N
        %hold on
        %title("B")
        %xlabel('Time',"fontsize",26)
        %ylabel('Species richness',"fontsize",26)
        %set(gca,'fontsize',24);
        %print -color -F:20 Fig5b.eps
        %pause
    end
 end       
         
        %x=linspace(min(heat(:,1)),max(heat(:,1)),150);
        %y=linspace(min(heat(:,2)),max(heat(:,2)),150);
        %[X,Y]=meshgrid(x,y);
        
x = heat(:,1);
y = heat(:,2);
z = heat(:,3);
z1 = heat(:,5);
%Z = zeros(length(x),length(x));
%countn = 0;
%a = linspace(1,length(x),length(x))
%for u = 1:length(x);
%   for v = 1:length(y);
%       %countn = countn + 1
       %pause
%       Z(u,v) = x(u,1),y(v,2);
%   end
%end
%n = 256;
%[X, Y] = meshgrid(linspace(min(x),max(x),n), linspace(min(y),max(y),n));
%Z = griddata(x,y,z,X,Y);
%// Remove the NaNs for imshow:
%Z(isnan(Z)) = 0;
%imshow(Z)
%m = min(Z(Z~=0));
%M = max(Z(Z~=0));
%imshow((Z-m)/(M-m));
 
 
x = linspace(min(x),max(x),length(x)); % 1-by-1000
y = linspace(min(y),max(y),length(y)); % 1-by-1000
%Zspe = reshape(z,length(As),length(As));%species
Z = reshape(z,length(As),length(As));%species
Z1 = reshape(z1,length(As),length(As));%species


%z = ; % 1000-by-1000
subplot(2,2,1)
fig = figure();
imagesc(x,y,Z);
xlabel("x");
ylabel("y");
title("Species richness");
colorbar();
xlabel('A',"fontsize",26)
ylabel('\omega',"fontsize",26)
title("C) Species Richness")
set(gca,'fontsize',24);
print -color -F:20 SpeciesRichness.eps

subplot(2,2,2)
fig = figure();
imagesc(x,y,Z1);
xlabel("x");
ylabel("y");
title("C) Persistence in ecosystem services");
colorbar();
xlabel('A',"fontsize",26)
ylabel('\omega',"fontsize",26)
set(gca,'fontsize',24);
print -color -F:20 EcosystemServices.eps




%end
      
