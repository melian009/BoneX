%% 0 - the Palmito boolean network
%24-09-03

%% 1- basic simulation features/parameters
environment=0; % 0 no  environment filtering, 1  environment filtering
flutuacao=0;  % 0 no  environment fluctuation, 1  environment fluctuation
S= 11; % palmito, tucano, falcao relogio, 3 frugivoros, 3 plants, 2 herbívoros
E= 1; %number of ecosystem services
dim=S+E; %matrix size
nsims=100; %number of simulations
tfim=100; %final timestep
theta=randn(1,1); % environmentgal optima
resultadosfinais=zeros(nsims,4); %store results
%1- mean species richness
%2 - variance in species presence
%3 - mean ecosystem service supply
%4 - variance in ecosystem service supply

%% The palmito network - truth tables


for simulacoes=1:nsims
    Resultados=zeros(tfim,dim); %record results from a similuation

    % in both matrices below the last row/column describes the ecosystem
    % services

    z=randn(S,1); %traits of the species
    estados1=round(rand(dim,1)); %starts randomly the state of each species
    %estados1=ones(dim,1);
    cp=zeros(dim,1); %cutoff for positive values
    cp(1)=1; %palmito está on se tiver pelo menos dois dispersores "on"
    cp(2)=1; %tucano está on se tiver pelo menos duas plantas "on"
    cp(3)=1; %falcao está on se tiver pelo menos dois dispersores on
    cn=zeros(dim,1); %cutoff for negative values
    mp=zeros(dim,1); %record positive ms
    mn=zeros(dim,1); %record negative ms


    %1 - palmito
    %2 - tucano
    %3 - Falcao
    %4,5,6 - frugivoros
    %7,8,9 - plantas
    %10,11 - herbivoros
    %12 - servico

    matrizp=zeros(dim); %matrix of positive effects, columns affect rows

    %palmito
    matrizp(1,2)=1;
    matrizp(1,4)=1;
    matrizp(1,5)=1;
    matrizp(1,6)=1;

    %tucano
    matrizp(2,1)=1;
    matrizp(2,7)=1;
    matrizp(1,8)=1;
    matrizp(1,9)=1;

    %falcao
    matrizp(3,2)=1;
    matrizp(3,4)=1;

    %Frug2
    matrizp(4,1)=1;
    matrizp(4,7)=1;
    matrizp(4,8)=1;

    %Frug3
    matrizp(5,1)=1;
    matrizp(5,7)=1;

    %Frug4
    matrizp(6,1)=1;

    %Plan2
    matrizp(7,2)=1;
    matrizp(7,4)=1;
    matrizp(7,5)=1;

    %Plan3
    matrizp(8,2)=1;
    matrizp(8,4)=1;

    %Plan4
    matrizp(9,2)=1;

    %herb1
    matrizp(10,7)=1;

    %herb1
    matrizp(11,9)=1;

    %ecosystem
    matrizp(12,1)=1;

    matrizn=zeros(dim); %matrix of negative effects, columns affect rows

    %tucano
    matrizn(2,3)=1;

    %Frug2
    matrizn(4,3)=1;

    %Plan2
    matrizn(7,10)=1;

    %Plan4
    matrizn(9,11)=1;


    simulacoes % tell the number of the simulations

    %% simulation
    for t=1:tfim
        t
        Resultados(t,:)=estados1'; %record the state of the system
        estadoanterior=estados1; %store the current state


        %% compute the positive and negative effects
        for i = 1:dim
            mp(i)=matrizp(i,:)*estadoanterior;
            if mp(i)>cp(i)
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

            if flutuacao==1

                theta=theta+0.1.*randn(1,1);
            end

            for i=1:S
                x=exp(-0.2.*((theta-z(i)).*(theta-z(i))));
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
dlmwrite('resultados_figfinal_5_com_perturbacao_flutuacao.txt',resultadosfinais) %print results
%dlmwrite('resultados_figfinal_modelo4_environment.txt',resultadosfinais) %print results