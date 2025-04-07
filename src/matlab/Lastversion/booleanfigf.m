%% 1- basic simulation features/parameters
environment=1; % no fluctuating environment, 1 fluctuacting environment
S=29; %species richness
E= 1; %number of ecosystem services
dim=S+E; %matrix size
p=0.2; %connectance
nsims=100; %number of simulations
scenario=3; %1- random, 2 - reciprocal negative, 3- trophic layer (2 layers) + consumer service, 4- trophic layer (2 layers) + top service
tfim=100; %final timestep
theta=randn(1,1); % environmentgal optima
resultadosfinais=zeros(nsims,4); %store results
%1- mean species richness
%2 - variance in species presence
%3 - mean ecosystem service supply
%4 - variance in ecosystem service supply

for simulacoes=1:nsims
    Resultados=zeros(tfim,dim); %record results from a similuation

    kE=3; %number of species directly supporting the ecosystem service
    ninteractions=round(p*S*(S-1)); %number of interactions in both matrices.
    nintp=round(ninteractions)./2; %number of positive effects
    nintn=round(ninteractions)./2; %number of negative effects
    % Interactions are directed links
    % in both matrices below the last rows/columns describe the ecosystem
    % services
    matrizp=zeros(dim); %matrix of positive effects, columns affect rows
    matrizn=zeros(dim); %matrix of negative effects, columns affect rows
    z=randn(S,1); %traits of the species
    estados1=round(rand(dim,1)); %all nodes start on
    cp=zeros(dim,1); %cutoff for positive values
    cn=zeros(dim,1); %cutoff for negative values
    mp=zeros(dim,1); %record positive ms
    mn=zeros(dim,1); %record negative ms
    %a - three distinct network structures
    cp(end)=2;

    %% random distribution of links
    if scenario==1

        while nintp>0
            x=randi(S);
            y=randi(S);
            if x ~= y
                if matrizp(x,y)==0
                    matrizp(x,y)=1;
                    nintp=nintp-1;
                end
            end
        end

        for i=1:S
            x=randi(sum(matrizp(i,:))+2);
            cp(i)=x-1;
        end

        %assign interactions among species fueling ecosystem services - for sake of
        %simplicity we are not assuming feedbacks between ecosystem services and
        %species presences or direct negative effects of species on ecosystem
        %services

        while kE>0
            y=randi(S);
            if matrizp(end,y)==0

                matrizp(end,y)=1;
                kE=kE-1;
            end
        end

        %assign negative interactions among species in the positive matrix
        while nintn>0
            x=randi(S);
            y=randi(S);
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
    for i=1:S
        x=randi(sum(matrizn(i,:))+2);
        cn(i)=x-1;
    end

    %% reciprocal interactions (+/-)
    if scenario==2
        layer1=round(S/2);
        while nintp>0
            x=randi(S);
            y=randi(S);
            if matrizp(x,y)==0
                if matrizp(y,x)==0
                    matrizp(x,y)=1;
                    matrizn(y,x)=1;
                    nintp=nintp-1;
                end
            end
        end

        for i=1:S
            x=randi(sum(matrizp(i,:))+2);
            cp(i)=x-1;
        end

        for i=1:S
            x=randi(sum(matrizn(i,:))+2);
            cn(i)=x-1;
        end
    end

    %trophic layer - bottom up ecosystem service
    if scenario==3
        %split the matrices in two
        layer1=round(S/2);
        while nintp>0
            x=randi(S-layer1);
            y=randi(layer1);
            if matrizp(layer1+x,y)==0
                matrizp(layer1+x,y)=1;
                matrizn(y,layer1+x)=1;
                nintp=nintp-1;
            end
        end
        for i=layer1+1:S
            if sum(matrizp(i,:))>0
                x=randi(sum(matrizp(i,:)));
            else
                x=0;
            end

            cp(i)=x;
        end

        for i=1:S
            x=randi(sum(matrizn(i,:))+2);
            cn(i)=x-1;
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
        layer1=round(S/2);
        while nintp>0
            x=randi(S-layer1);
            y=randi(layer1);
            if matrizp(layer1+x,y)==0
                matrizp(layer1+x,y)=1;
                matrizn(y,layer1+x)=1;
                nintp=nintp-1;
            end
        end
        for i=layer1+1:S
            if sum(matrizp(i,:))>0
                x=randi(sum(matrizp(i,:)));
                cp(i)=x;
            else
                cp(i)=0;
            end

        end

        for i=1:S
            x=randi(sum(matrizn(i,:))+2);
            cn(i)=x-1;
        end
    end

    simulacoes % tell the number of the simulations

    %% simulation
    for t=1:tfim
        t
        Resultados(t,:)=estados1'; %record the state of the system
        estadoanterior=estados1; %store the current state


        %% compute the positive and negative effects
        for i = 1:dim
            mp(i)=matrizp(i,:)*estadoanterior;
            if mp(i)>=cp(i)
                mp(i)=1;
            else
                mp(i)=0;
            end
            mn(i)=matrizn(i,:)*estadoanterior;

            if mn(i)>cn(i)
                mn(i)=0;
            else
                mn(i)=1;
            end

        end

        for i=1:dim
            estados1(i)=mp(i).*mn(i);
        end



        %%%fluctuating environment
        if environment==1
           % theta=theta+0.1.*randn(1,1);
            for i=1:S
                x=exp(-2.*((theta-z(i)).*(theta-z(i))));
                q=rand(1,1);
                if q<(1-x)
                    estados1(i)=0;
                end
            end
        end

    end

        especiesd=Resultados(:,1:end-1);
        riqueza=sum(especiesd,2);


    resultadosfinais(simulacoes,1)=mean(riqueza);
    resultadosfinais(simulacoes,2)=var(riqueza);
    resultadosfinais(simulacoes,3)=mean(Resultados(:,end));
    resultadosfinais(simulacoes,4)=var(Resultados(:,end));
end
dlmwrite('resultados_figfinal_modelo3env.txt',resultadosfinais) %print results
%dlmwrite('resultados_figfinal_modelo4_environment.txt',resultadosfinais) %print results