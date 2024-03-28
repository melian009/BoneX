%log
%24-02-21:creation
%24-02-21:I combined previous files to generate a single script
%24-02-22: I added the "statistical mechanics" approach to the truth tables
% run simulations
nsims=10; %number of simulations
tfim=1000;

for simulacoes=1:nsims
simulacoes
     %final timestep

 %% Generate A+ and A-

% I am assuming a random network in which the indegree is fixed. later we
% can change that for random networks

    %generate a regular network in which the in-degree
    dim=24; %number of species
    k=3; %in-degree
    matriz=zeros(dim); % matrix of interactions

    for i=1:dim
        bandeira=0;%flag
        while bandeira==0
            indices_ones = randperm(dim, k); %randomly indicate the species that affect i
            verifica1= any(indices_ones == i);
            if verifica1==0
                matriz(i,indices_ones)=1;
                bandeira=1;
            end
        end
    end
    positiva=matriz;
    matriz=zeros(dim); % matrix of interactions

    for i=1:dim
        bandeira=0;%flag
        while bandeira==0
            indices_ones = randperm(dim, k); %randomly indicate the species that affect i
            verifica1= any(indices_ones == i);
            if verifica1==0
                bandeira=1;
                bandeira2=0;
                for j=1:k
                    if positiva(i,j)==1
                        bandeira2=1;
                    end
                end


                if bandeira2==0
                    matriz(i,indices_ones)=1;
                    bandeira=1;
                end

            end
        end
        negativa=matriz;

    end

% generate the cut-off values
mu=0.5;
%cut-offs follow an exponential distribution
cpositiva= round(exprnd(mu,[dim 1]))+1; 
cnegativa= round(exprnd(mu,[dim 1]))+1;
Mpositiva=zeros(dim,1);
Mnegativa=zeros(dim,1);
riqueza=zeros(nsims,1); %species richness at each time step

estados1=ones(dim,1); %initial vector - all species starts "on"




Resultados=zeros(dim, tfim); %store the states of the nodes at each time step
simulacoes % tell the number of the simulations

%% simulation
for t=1:tfim
    t
Resultados(:,t)=estados1; %record the state of the system


estadoanterior=estados1; %store the current state
estadoatual=zeros(dim,1); %vector to record the state

%% update the states
for i = 1:dim
    vetori=positiva(i,:);
Mpositiva(i)=vetori*estados1;
if Mpositiva(i)>=cpositiva(i)
    Mpositiva(i)=1;
else
    Mpositiva(i)=0;
end

    vetori=negativa(i,:);
Mnegativa(i)=vetori*estados1;

if Mnegativa(i)>=cnegativa(i)
    Mnegativa(i)=0;
else
    Mnegativa(i)=1;
end
estadoatual(i)=Mnegativa(i).*Mpositiva(i);

end




estados1=estadoatual; %update all node states

end


% Define the number of last time steps to compare
num_last_columns = 100;

fimmatriz=Resultados(:,end-num_last_columns+1:end);
estadoH=entropy(fimmatriz); %compute the entropy of the time steps for the entire community
entropiaV=zeros(dim,1); %compute the entropy for each species
for i=1:dim
    entropiaV(i)=entropy(fimmatriz(i,:));
end
estado=mean(entropiaV); %mean entropy of each species

% Create a black and white plot
imagesc(Resultados);

% Set the colormap to black and white
colormap(flipud(gray));

% Display the colorbar (optional)
colorbar;


% Add title and labels (optional)


% title('Dynamics of the ecological network');
% xlabel('Time');
% ylabel('Species activity');
% filename = sprintf('black_and_white_plot_dim%d_k%d_estado%d.jpg', dim, k,estado);
% 
% saveas(gcf, filename);

riquezafinal=sum(Resultados(:,end)); %number of species
resultadossimulados(simulacoes,1)=estadoH; % store the entropy
resultadossimulados(simulacoes,2)=riquezafinal; %store species richness
end
dlmwrite('redealeatoria.txt',resultadossimulados) %print results