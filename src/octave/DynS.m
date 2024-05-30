%----------------------------------------------------------------------
%Ecosystem services in ecological network seascapes
%Guimaraes&Melian@KB May 2024
%----------------------------------------------------------------------
function DynS()  

    show=true;
    showEach = 1;
    countgen = 0;
    %gamma=[];
    T=1000;%time    
    As = [0.025 0.05 0.075 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1];%amplitude
    GPTs =[100 500 1000]; % GPT=1 is Static 1 5 10 50 
    R_k={};  %Composicion de species @ each Site and each Generation k  
  
 for i = 1:length(As); % i refersw to values of amplitude
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
        r=zeros(T,2);
    
        for k = 1:T,  %Generations...one combination A,f        
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
        %pause
        end%k
        A
        f
        %subplot(1,1,1)
        %plot(r(:,1),r(:,2))
        %hold on
        %title("A")
        %xlabel('Time',"fontsize",26)
        %ylabel('Environmental threshold',"fontsize",26)
        %set(gca,'fontsize',24);
        %print -color -F:20 Fig5a.eps
        %subplot(1,1,3)
        plot(r(:,1),r(:,3))
        hold on
        axis([0 1000 0 50])%N
        hold on
        title("B")
        xlabel('Time',"fontsize",26)
        ylabel('Species richness',"fontsize",26)
        set(gca,'fontsize',24);
        print -color -F:20 Fig5b.eps
        %pause
    end
 end       
      
