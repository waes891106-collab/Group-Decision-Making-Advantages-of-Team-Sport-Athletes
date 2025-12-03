                %  process architecture and stopping rule
% mean interaction contrast of survival function
clear all;
close all;

%% [Subject Session T D1 D2 Left/Right Trigger PICindex PICname FixationTime Response RT ACC ]
% Column 1: Subject
% Column 2: Session
% Column 3: Target (0: target absent, 1: target present)
% Column 4: Dimension 1 (0: no, 1: low, 2: high)
% Column 5: Dimension 2 (0: no, 1: low, 2: high)
% Column 6: Target on the left or right 1 (1: right, 2: left) p.s. not meaningful for the target absent condition
% Column 7: Trigger number (11: HH and right target; 12: HH and left target; 21: HL and right target; 22: HL and left target; 31: LH and right target; 32: LH and left target; 41: LL and right target; 42: LL and left target; 90: XX and no target)
% Column 8: PIC index (for programming)
% Column 9: PIC name (for programming)
% Column 10: Fixation Time (ms unit)
% Column 11: Response (-1: no response, 1: target absent, 2: target present)
% Column 12: Response Time (ms unit)
% Column 13: Accuracy (0: incorrect, 1: correct)

load 'data/VST_sub032_sess1.csv';
y = VST_sub032_sess1;
subs = [032];

%% for processing architecture and stopping rule

n =1000; % number of boot samples 

for i = 1;

% obtain raw data of the redundant-target condiitons
bb = y(find (y(:,4) == 2 & y(:,5) == 2 & y(:,13) == 1 & y(:,12) < 2000 & y(:,12) > 200),12); 
bs = y(find (y(:,4) == 2 & y(:,5) == 1 & y(:,13) == 1 & y(:,12) < 2000 & y(:,12) > 200),12); 
sb = y(find (y(:,4) == 1 & y(:,5) == 2 & y(:,13) == 1 & y(:,12) < 2000 & y(:,12) > 200),12); 
ss = y(find (y(:,4) == 1 & y(:,5) == 1 & y(:,13) == 1 & y(:,12) < 2000 & y(:,12) > 200),12);

%% for SF

% mean RT
bbmean (:,i) = mean (bb)
bsmean (:,i) = mean (bs)
sbmean (:,i) = mean (sb)
ssmean (:,i) = mean (ss)

% interaction contrast of mean RT
ICmean= bbmean -bsmean-sbmean+ssmean


% edf
t = [200:10:2000];
bbedf = EDF (t,bb);
bsedf = EDF (t,bs);
sbedf = EDF (t,sb);
ssedf = EDF (t,ss);

%calculate survival funciton
bbsf =1-bbedf;
bssf =1-bsedf;
sbsf =1-sbedf;
sssf =1-ssedf;

% calculate interaction contrast of survival function
ICSF= bbsf+sssf-bssf-sbsf;
ICSFraw (:,i)= bbsf+sssf-bssf-sbsf;

% plot 
SF = figure (i)

plot (t,bbsf,'LineStyle','-.','LineWidth',1,'Color',[0 0 0],'DisplayName','HH')
hold on
plot (t,bssf,'LineStyle',':','LineWidth',1,'Color',[0 0 0],'DisplayName','HL')
hold on
plot(t,sbsf,'LineStyle','--','LineWidth',1,'Color',[0 0 0],'DisplayName','LH')
hold on
plot (t,sssf,'LineWidth',1,'Color',[0 0 0],'DisplayName','LL')
hold on
%plot(t,ICSF,'LineWidth',2,'Color',[0 0 0],'DisplayName','SIC')
axis([0  2000  0  1])
%set(gca,'XTick',150:100:5000)
set(gca,'YTick',0 :0.25:1)
%legend ('HH','HL','LH','LL','SIC')
legend ('HH','HL','LH','LL')
% Create xlabel
xlabel('RT (ms)','FontSize',16);
% Create ylabel
ylabel('Probability','FontSize',16);
saveas(SF, sprintf('results/bootSFT/SF_%d.tiff', subs(1)));

%% MIC and SIC
% boot strap samples
bbboot = ones(size(bb,1),1)*ones(1,n);
bsboot = ones(size(bs,1),1)*ones(1,n);
sbboot = ones(size(sb,1),1)*ones(1,n);
ssboot = ones(size(ss,1),1)*ones(1,n);

for j = 1:n;
    bbboot(:,j)= bootstrap(bb);
    bsboot(:,j)= bootstrap(bs);
    sbboot(:,j)= bootstrap(sb);
    ssboot(:,j)= bootstrap(ss);
end;

% mean RT
bbmean  = mean (bbboot,1)
bsmean  = mean (bsboot,1)
sbmean  = mean (sbboot,1)
ssmean  = mean (ssboot,1)


% interaction contrast of mean RT
ICmean= bbmean -bsmean-sbmean+ssmean
meanICmean (:,i)= mean (ICmean,2);

%stdICSF= std (ICSF,0,2);
rtci=prctile(ICmean,[2.5 97.5],2);
rtlowciraw(:,i)= prctile(ICmean,[2.5 ],2);
rthighciraw(:,i)= prctile(ICmean,[97.5 ],2);

MIC = figure(i+20)
X=[ICmean]
t=-300:10:300;
hist(X,t)
axis([-300 300 0 100])
a=rtlowciraw(:,i)
b=rthighciraw(:,i)
line([a,a],[0,100],'Color','black','LineWidth',1.5)
line([b,b],[0,100],'Color','black','LineWidth',1.5)
%title ('The interaction contrast at the level of the mean of subj1')
xlabel('Simulated MIC','FontSize',16)
ylabel('Frequency','FontSize',16)
saveas(MIC, sprintf('results/bootSFT/MIC_%d.tiff', subs(1)));



% sort boot samples
bbboot= sort(bbboot,1);
bsboot= sort(bsboot,1);
sbboot= sort(sbboot,1);
ssboot= sort(ssboot,1);

% gausskernel
t = [200:10:2000];
for k = 1:n;
    bbedf(:,k) = EDF (t,bbboot(:,k));
    bsedf(:,k) = EDF (t,bsboot(:,k));
    sbedf(:,k) = EDF (t,sbboot(:,k));
    ssedf(:,k) = EDF (t,ssboot(:,k));
end;

%calculate survival funciton
bbsf =1-bbedf;
bssf =1-bsedf;
sbsf =1-sbedf;
sssf =1-ssedf;

% calculate interaction contrast of survival function
ICSF= bbsf+sssf-bssf-sbsf;
meanICSF= mean (ICSF,2);
meanICSFraw(:,i)= mean (ICSF,2);
%stdICSF= std (ICSF,0,2);
ci=prctile(ICSF,[2.5 97.5],2);
lowciraw(:,i)= prctile(ICSF,[2.5 ],2);
highciraw(:,i)= prctile(ICSF,[97.5 ],2);

% plot 
SIC = figure (i+30)
%plot (t,meanICSF, t,ci(:,1), t,ci(:,2))
plot (t,meanICSF,'LineWidth',2,'Color',[0 0 0],'DisplayName','SIC')
hold on
plot (t,ci(:,1),'LineStyle',':','LineWidth',1,'Color',[0 0 0],'DisplayName','95% CI for SIC')
hold on
plot (t,ci(:,2),'LineStyle',':','LineWidth',1,'Color',[0 0 0],'DisplayName','95% CI for SIC')
hold on
axis([200  2000  -0.50  0.50])
set(gca,'XTick',200:100:2000)
%set(gca,'YTick',-0.30:0.25:0.40)
legend ('SIC','95% CI for SIC')
%legend ('SIC','95% CI for SIC','95% CI for SIC')
%title ('survival interaction contrast of subj1')
% Create xlabel
xlabel('RT (ms)','FontSize',16);
% Create ylabel
ylabel('Probability','FontSize',16);
saveas(SIC, sprintf('results/bootSFT/SIC_%d.tiff', subs(1)));

end;
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           