%log
%24-02-21:creation
%24-02-21:I combined previous files to generate a single script

% run simulations
nsims=10 %number of simulations

for simulacoes=1:nsims

    tfim=1000; %final timestep

    %% Generate regular networks

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
    riqueza=zeros(nsims,1); %species richness at each time step

    %% compute the truth table
    dados_struct = struct(); %structure in which we will store the truth tables
    for i=1:dim
        % create one random truth table for each species

        elementos_selecionados=find(matriz(i,:)>0);
        r=sum(matriz(i,:));
        matriz_combinacoes = dec2bin(0:2^r-1, r) - '0'; % generate the combinations of states 
        matrizT=2.*ones(2^r,dim); %matrix with rows describing combinations of states and all species in which the truth table will be stored. 2 means that the species does not affect the other species


        for j=1:r
            matrizT(:,elementos_selecionados(j))=matriz_combinacoes(:,j); %store the combinations
        end

        vetor_aleatorio = randi([0 1], 1, size(matrizT,1));
        v=vetor_aleatorio'; % vector describing the state of species i for each combination of other species
        matrizT=[matrizT v]; % truth table

        % store the 
        dados_struct.(['matriz_' num2str(i)]) = matrizT;%store the truth table in the strcuture
        i;
    end

   
estados1=ones(dim,1); %initial vector - all species starts "on"
Resultados=zeros(dim, tfim); %store the states of the nodes at each time step
simulacoes % tell the number of the simulations

%% simulation
for t=1:tfim
Resultados(:,t)=estados1; %record the state of the system


estadoanterior=estados1; %store the current state
estadoatual=zeros(dim,1); %vector to record the state

%% update the states
for i = 1:dim
    nome_campo = ['matriz_' num2str(i)]; %field in which the matrix is stored
    matrizT = dados_struct.(nome_campo); %load the matrix
    definidores=find(sum(matrizT(:,1:dim)) < 2.*size(matrizT,1)); %find the fields in which the species affect the focal species i
    matrizT1=matrizT; %

    for j=1:size(definidores,2)
    linhas_encontradas = find(matrizT1(:,definidores(j)) ==estadoanterior(definidores(j))); % find the combination of states of the partners
    matrizT1=matrizT1(linhas_encontradas,:);%extract just the actual truth table
    end

    if size(matrizT1,1)>1
        ERRO=1
    else
        estadoatual(i)=matrizT1(1,dim+1); %update the state of the node i
    end
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