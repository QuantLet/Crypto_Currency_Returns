

%% Global Commands

clear;clc;

% specify directoy for the files
directory='/Users/johnnyjheng/Documents/MSCA & ASE/04 Crypto Currency Returns/CC returns/Code/20250324 CC Genus Proximum';
addpath(genpath(directory))
cd(directory)

%set global commands for font size and line width
size_font=9;
size_line=1.5;
set(0,'DefaultAxesFontSize',size_font,'DefaultTextFontSize',size_font);
set(0,'defaultlinelinewidth',size_line)

% figures
set(0, 'defaultFigurePaperType', 'A4')
set(0, 'defaultFigurePaperUnits', 'centimeters')
set(0, 'defaultFigurePaperPositionMode', 'auto')
figure_wide=[680 678 800 420];

% colors
color_blue=[0 102 204]./255;
color_green=[0 204 102]./255;
color_red=[204 0 0]./255;
color_black=[0 0 0];

% reset rngs before running
rng(1)

%% Data

% load data_dynamic_jan_2021.mat

data_static = readtable('24D.csv');

is_num_static = varfun(@isnumeric, data_static, 'OutputFormat','uniform');
stats_s       = data_static{:, is_num_static};
est_labels    = data_static.Properties.VariableNames(is_num_static);

data_dynamic = readtable('data_dynamic.csv');

if ~isdatetime(data_dynamic.Date)
    data_dynamic.Date = datetime(data_dynamic.Date);
end

waves    = unique(data_dynamic.wave);
t_max    = numel(waves);
n_assets = sum(data_dynamic.wave == waves(1));

date_unique = zeros(t_max,1);
for k = 1:t_max
    rows_k          = data_dynamic.wave == waves(k);
    date_unique(k)  = datenum(data_dynamic.Date(find(rows_k,1)));
end

%%

%% Expanding window factor model

t_start=ceil(t_max/3);
t_start=1;
user_factor=2;

mu=zeros(1,24);
% init
[loadings,FF,f2] = factor_an_static(stats_s);

for j = 1:(t_max - t_start + 1)
    time(j,1) = datetime(date_unique(j + t_start - 1,1), 'ConvertFrom', 'datenum');

    curr_wave  = waves(j + t_start - 1);
    rows_wave  = data_dynamic.wave == curr_wave;

    stats_t    = data_dynamic{rows_wave, est_labels};

    % nan assets
    assets_nan             = sum(isnan(stats_t),2) > 0;
    stats_t(assets_nan,:)  = 0;
    stats_t(~isfinite(stats_t)) = 0;
    parms_nan              = sum(isnan(stats_t),1) > 0;
    stats_t(:,parms_nan)   = 0;

    Rho = corrcoef(stats_t);
    n1  = length(stats_t);
    m   = mean(stats_t);
    zz  = (stats_t - repmat(m,n1,1)) ./ repmat(sqrt(var(stats_t)), n1, 1);

    F   = zz * f2;
    %F(:,2) = -F(:,2);

    type_info      = data_dynamic.type_assets(rows_wave);
    type_crypto    = contains(type_info, 'Crypto');
    type_crypto_mod = type_crypto + 1;

c=zeros(size(type_crypto_mod));
df=n_assets-1;

 


% mu = [ mu ; grpstats(stats_t,type_crypto==1)];

g = (type_crypto == 1);
mu_classic = mean(stats_t(~g,:), 1);
mu_crypto  = mean(stats_t(g,:), 1);
mu = [mu ; mu_classic ; mu_crypto];

for i=1
  mdl=fitglm(F(:,1),type_crypto,'linear','Distribution','binomial','link','logit');
  tbl = devianceTest(mdl);
   
end
    Dev_F1(j,1)=table2array (tbl(2,1));

    p_val_F1(j,1)=table2array (tbl(2,4));
    
for i=2
   mdl=fitglm(F(:,2),type_crypto,'linear','Distribution','binomial','link','logit');
   tbl = devianceTest(mdl);
end
    Dev_F2(j,1)=table2array (tbl(2,1));

    p_val_F2(j,1)=table2array (tbl(2,4));
    
 
for i=3
   mdl=fitglm(F(:,3),type_crypto,'linear','Distribution','binomial','link','logit');
   tbl = devianceTest(mdl);
end
    Dev_F3(j,1)=table2array (tbl(2,1));

    p_val_F3(j,1)=table2array (tbl(2,4));
 
   
end

%% Plot Likelihood Ratios and p-values

h=figure()
y_lim=[0 1200];


ax1 = subplot(3,1,1); % top subplot


plot(time,Dev_F1,'color',color_blue);
%ylim(y_lim);
ylabel('Likelihood Ratio');
xlabel('Time');
title(ax1,'Tail Factor');

ax2 = subplot(3,1,2); % bottom subplot
plot( time,Dev_F2,'color',color_blue);
%ylim(y_lim);
ylabel('Likelihood Ratio');
xlabel('Time');

title(ax2,'Memory Factor')

ax3 = subplot(3,1,3); % bottom subplot
plot(time,Dev_F3,'color',color_blue);
%ylim(y_lim);
ylabel('Likelihood Ratio');
xlabel('Time');

title(ax3,'Moment Factor');
set(h,'PaperUnits','points');
set(h,'PaperPosition',[0 0 480 360]);
print(h,'LR Factors.png','-dpng','-r300');



y_lim=[0 1];

h=figure()


ax1 = subplot(3,1,1); % top subplot


plot(time,p_val_F1,'color',color_blue);
ylim([-0.5, 0.5]);
ylabel('P-value');
xlabel('Time');
title(ax1,'Tail Factor');

ax2 = subplot(3,1,2); % bottom subplot
plot( time,p_val_F2,'color',color_blue);
ylim(y_lim);
ylabel('P-value');
xlabel('Time');

title(ax2,'Memory factor')

ax3 = subplot(3,1,3); % bottom subplot
plot(time,p_val_F3,'color',color_blue);
ylim(y_lim);
ylabel('P-value');
xlabel('Time');

title(ax3,'Moment Factor');
set(h,'PaperUnits','points');
set(h,'PaperPosition',[0 0 480 360]);
print(h,'LR p-values.png','-dpng','-r300');

%% Stats dynamics by type of assets
mu(1,:) = [];
mu_classical= mu(1:2:end,:);
mu_cryptos= mu(2:2:end,:);
h=figure()

for i=1:24

ax1 = subplot(24,1,i);
plot(time,mu_cryptos(:,i) ,'color',color_green);
hold on
plot(time,mu_classical(:,i),'color',color_black);
%legend('Cryptos','Classical assets');
title(est_labels((i)));


end


%% Chart with some of the variables
h=figure()
ax1 = subplot(3,1,1); % top subplot
plot(time,mu_cryptos(:,20),'LineWidth',2,'color',color_green);

hold on
plot(time,mu_classical(:,20)*100,'LineWidth',2 ,'color',color_black);

ylabel(est_labels((20)));
xlabel('Time');
ax1 = subplot(3,1,2); % top subplot
plot(time,mu_cryptos(:,21),'LineWidth',2,'color',color_green);
hold on
plot(time,mu_classical(:,21) ,'LineWidth',2,'color',color_black);
ylabel(est_labels((21)));
xlabel('Time');
ax1 = subplot(3,1,3); % top subplot
plot(time,mu_cryptos(:,22) ,'LineWidth',2,'color',color_green);
hold on
plot(time,mu_classical(:,22),'LineWidth',2,'color',color_black);
ylabel(est_labels((22)));
xlabel('Time');
%{
ax1 = subplot(4,1,4); % top subplot
plot(time,mu_cryptos(:,17) ,'color',color_green);
hold on
plot(time,mu_classical(:,17),'color',color_black);
ylabel(est_labels((17)));

hold off
%}
% add a bit space to the figure
fig = gcf;
fig.Position(3) = fig.Position(3) + 250;
% add legend
legend('Cryptos','Classical assets');
Lgnd = legend('show');
Lgnd.Position(1) = 0.01;
Lgnd.Position(2) = 0.4;
%
%% Chart with quantiles
h=figure()
ax1 = subplot(3,1,1); % top subplot
plot(time,mu_cryptos(:,2),'LineWidth',2,'color',color_green);

hold on
plot(time,mu_classical(:,2),'LineWidth',2 ,'color',color_black);

ylabel(est_labels((2)));
xlabel('Time');
ax1 = subplot(3,1,2); % top subplot
plot(time,mu_cryptos(:,3),'LineWidth',2,'color',color_green);
hold on
plot(time,mu_classical(:,3) ,'LineWidth',2,'color',color_black);
ylabel(est_labels((3)));
xlabel('Time');
ax1 = subplot(3,1,3); % top subplot
plot(time,mu_cryptos(:,4) ,'LineWidth',2,'color',color_green);
hold on
plot(time,mu_classical(:,4),'LineWidth',2,'color',color_black);
ylabel(est_labels((4)));
xlabel('Time');
%{
ax1 = subplot(4,1,4); % top subplot
plot(time,mu_cryptos(:,17) ,'color',color_green);
hold on
plot(time,mu_classical(:,17),'color',color_black);
ylabel(est_labels((17)));

hold off
%}
% add a bit space to the figure
fig = gcf;
fig.Position(3) = fig.Position(3) + 250;
% add legend
legend('Cryptos','Classical assets');
Lgnd = legend('show');
Lgnd.Position(1) = 0.01;
Lgnd.Position(2) = 0.4;
%

