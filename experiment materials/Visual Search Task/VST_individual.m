                                                                                          clear;clc;
try

AssertOpenGL;
PsychDefaultSetup(2);
%% ask for subject name and test number-----------------------------------
wp='D:/Visual Search Task';
user_ans = inputdlg({'Enter subject name:','Enter session number:','Practice'},'Inputs');
subj_name  = user_ans{1};
subj_session = str2double(user_ans{2});
practice = str2double(user_ans{3});

%% port declaration: (This is for 64-bit computer system)
% config_io;
% address = hex2dec('3FF8');
% outp(address,0);

%% 
ListenChar(2);
HideCursor;

%% prepare screen parameters
Screen('Preference','SkipSyncTests', 1); 
screenNumber = max(Screen('Screens'));
[windowPtr,rect] = Screen('OpenWindow',screenNumber);
Screen('FillRect',windowPtr,0,rect);
Screen('TextFont',windowPtr,'Arial');
Screen('TextSize',windowPtr,36);
Screen('TextStyle',windowPtr,1);
rscreen_length = 36.8; %[cm]
sit_dist = 58; %[cm]
theta_view = atand(rscreen_length/2/sit_dist)*2;
view_scale = theta_view/rect(3); %[degree/pixel]
[xCenter, yCenter] = RectCenter(rect);
[screenXpixels, screenYpixels] = Screen('WindowSize', windowPtr);
Screen('BlendFunction', windowPtr, 'GL_SRC_ALPHA', 'GL_ONE_MINUS_SRC_ALPHA');
ifi = Screen('GetFlipInterval', windowPtr);
% % --------------------------------------------------------------------end
% 
% % prepare shown symbols---- ----------------------------------------------
fixation = '+';
ques_resp = '?';
correct = 'Correct';
wrong = 'Wrong';
noanswer = 'No response';
resting_subrun = 'Take a rest';
resting_end = 'Experiment over!';
% % --------------------------------------------------------------------end

%read image----------------------------------------------------------------
setnum=99;%how many img for each set
QAsheet=[];imgtank=[];text_num=[];
%QAsheet [T D1 D2 Left/Right PICindex PICname FixationTime Response RT ACC ]
condition = [1 2 3 4 9];
% Condition: 1=HH, 2=HL, 3=LH, 4=LL, 5=XX
% D1&D2: 0=no target, 1=low, 2=high
for difficult = condition
    for setindex=00:setnum
        imgindex=difficult*100+setindex;
        imglocation=[wp,'/PIC/',num2str(imgindex,'%03.0f'),'.png'];
        img=imread(imglocation);
        imgtank{end+1}=img;
        switch difficult
            case 1
                text=[1 2 2];
            case 2
                text=[1 2 1];
            case 3
                text=[1 1 2];
            case 4
                text=[1 1 1];
            case 9
                text=[0 0 0];
        end
        
        if (setindex<=49)
                text_num=[1];
        elseif (setindex>=50)
                text_num=[2];
        end
        
        % trigger number: 11: HH and right target; 12: HH and left target;
        % 21: HL and right target; 22: HL and left target;
        % 31: LH and right target; 32: LH and left target
        % 41: LL and right target; 42: LL and left target; 90: XX and no target
        if (imgindex<=149)      
                trigger_num=[11];
        elseif (imgindex<=199)
                trigger_num=[12];
        elseif (imgindex<=249)
                trigger_num=[21];
        elseif (imgindex<=299)
                trigger_num=[22];
        elseif (imgindex<=349)
                trigger_num=[31];
        elseif (imgindex<=399)
                trigger_num=[32];        
        elseif (imgindex<=449)
                trigger_num=[41];        
        elseif (imgindex<=499)
                trigger_num=[42];        
        elseif (imgindex<=999)
                trigger_num=[90];
        end
        
       
        QAsheet=[QAsheet;text text_num trigger_num length(imgtank) imgindex];
        
    end
end
fix_time = (randi([1500 1800],1,500)/1000)';
QAsheet(:,8) = fix_time;
QAsheet(:,9) = -1; %Response
QAsheet(:,10) = 0; %RT
QAsheet(:,11) = 0; %Correct

% ----------------------------------------------------------------------end

%% make question
examsort=randperm(length(QAsheet));
question=QAsheet(examsort,:);
trysort=randperm(length(QAsheet));
tryquestion=QAsheet(trysort,:);
%-----------------------------------------------------------------------end

%% kb name ------------------------------------------------------------------
kb_no= KbName('f');
kb_yes = KbName('j');
space = KbName('space');
%-----------------------------------------------------------------------end

%% experiment start
title_msg = 'Welcome to our experiment\n\n press space bar when you are ready';
prepare=0;
while prepare==0
    DrawFormattedText(windowPtr,title_msg,'center','center',255);
    Screen('Flip',windowPtr);
    [keyisdown,secs,kcode] = KbCheck;
         if keyisdown==1
            key_num = find(kcode);
         if (key_num==space)
                prepare = 1;
         end
         end
end

% Practice
while practice==1
prac_acc=[];

for q=1:10
    % fixation
    fixation_start_time = GetSecs;
    while (GetSecs-fixation_start_time <= fix_time(q))
        DrawFormattedText(windowPtr,fixation,'center','center',255);
        Screen('Flip', windowPtr);
    end
        
    has_respond = 0;
    resp = -1;
    response_start_time=GetSecs;
    while (GetSecs-response_start_time <= 3 && has_respond==0)
        imageTexture = Screen('MakeTexture', windowPtr, imgtank{tryquestion(q,6)});
        Screen('DrawTexture', windowPtr, imageTexture);
        Screen('Flip', windowPtr);
    
        [keyisdown,secs,kcode] = KbCheck;
        if keyisdown==1
            key_num = find(kcode);
            if (key_num==kb_no)
                has_respond = 1;
                resp=0;
                
            elseif (key_num==kb_yes)
                has_respond = 1;
                resp=1;
            end
            
            while KbCheck;
            end
        end
    end
    
    if resp==tryquestion(q,1);
        iscorrect=1;
        tryquestion(q,11)=1;
    else
        iscorrect=0;        
        tryquestion(q,11)=0;
    end
    
    prac_acc(q,1)=tryquestion(q,11);

    % show answer
    showans_start_time = GetSecs;
    if has_respond == 0
        text_show_ans = noanswer;
    elseif iscorrect==1
        text_show_ans = correct;
    elseif iscorrect==0
        text_show_ans = wrong;    
    end
    while (GetSecs-showans_start_time <= 0.5)
        DrawFormattedText(windowPtr,text_show_ans,'center','center',255);
        Screen('Flip',windowPtr);
    end
    Screen('Close')   
end
    
    if mean(prac_acc(:,1)) < 0.8
        practice = 1;
        pracacc_report = ['Accuracy: ',num2str(mean(prac_acc(:,1))), '\n\n\n Practice again \n press space bar when you are ready'];
        prepare=0;
        while prepare==0
            DrawFormattedText(windowPtr,pracacc_report,'center','center',255);
            Screen('Flip',windowPtr);
            [keyisdown,secs,kcode] = KbCheck;
         if keyisdown==1
            key_num = find(kcode);
         if (key_num==space)
                prepare = 1;
         end
         end
        end
    else
        practice = 0;
        pracacc_report = ['Accuracy: ',num2str(mean(prac_acc(:,1))), '\n\n\n Press space bar for formal test'];
        prepare=0;
        while prepare==0
            DrawFormattedText(windowPtr,pracacc_report,'center','center',255);
            Screen('Flip',windowPtr);
            [keyisdown,secs,kcode] = KbCheck;
         if keyisdown==1
            key_num = find(kcode);
         if (key_num==space)
                prepare = 1;
         end
         end
        end
    end
end
%%
title_msg = 'For formal test\n\n press space bar when ready';
prepare=0;
while prepare==0
    DrawFormattedText(windowPtr,title_msg,'center','center',255);
    Screen('Flip',windowPtr);
    [keyisdown,secs,kcode] = KbCheck;
         if keyisdown==1
            key_num = find(kcode);
         if (key_num==space)
                prepare = 1;
         end
         end
end

%%
resp_ans=zeros(length(question),1)-1;
resp_time=zeros(length(question),1);
ACC=[];

%% test
for q=1:length(question)    
    % fixation
    fixation_start_time = GetSecs;
    while (GetSecs-fixation_start_time <= fix_time(q))
        DrawFormattedText(windowPtr,fixation,'center','center',255);
        Screen('Flip', windowPtr);
    end
    
    has_respond = 0;
    resp = -1;

    response_start_time=GetSecs;
    while (GetSecs-response_start_time <= 3 && has_respond==0)
        imageTexture = Screen('MakeTexture', windowPtr, imgtank{question(q,6)});
        Screen('DrawTexture', windowPtr, imageTexture);
        Screen('Flip', windowPtr);
        triggerNum = question(q,5);        
%         outp(address,triggerNum);
    
        [keyisdown,secs,kcode] = KbCheck;
        if keyisdown==1
            key_num = find(kcode);
            if (key_num==kb_no)
                has_respond = 1;
                resp=0;                
                question(q,9) = 0;
                question(q,10)=GetSecs-response_start_time;
                
            elseif (key_num==kb_yes)
                has_respond = 1;
                resp=1;                
                question(q,9) = 1;
                question(q,10)=GetSecs-response_start_time;
            end
            
            while KbCheck;
            end
        end
    end
    
  
  %%  
%     outp(address,0);
    if resp==question(q,1);
        iscorrect=1;
        question(q,11) = 1;
    else
        iscorrect=0;
        question(q,11) = 0;
    end
    
    ACC(q,1)=question(q,11);
    
    % show answer
    showans_start_time = GetSecs;
    if has_respond == 0
        text_show_ans = noanswer;
    elseif iscorrect==1
        text_show_ans = correct;
    elseif iscorrect==0
        text_show_ans = wrong;    
    end
    while (GetSecs-showans_start_time <= 0.5)
        DrawFormattedText(windowPtr,text_show_ans,'center','center',255);
        Screen('Flip',windowPtr);
    end
    
%%
       if rem(q,50)==0 && q~=length(question)
        title_msg = 'saving, please wait  ';
        DrawFormattedText(windowPtr,title_msg,'center','center',255);
        Screen('Flip',windowPtr); 
        %save data in every iter. If it makes laggard, please deleteit.------------------------------------------------------
        filename=['VST_sub',num2str(subj_name),'_sess',num2str(subj_session),'.mat'];
        save(filename);
         %---------------------------------------------------------
        
        acc_report = ['Accuracy: ',num2str(mean(ACC(:,1))), '\n\n For next question press space bar when you are ready'];
        prepare=0;
     while prepare==0
        DrawFormattedText(windowPtr,acc_report,'center','center',255);
        Screen('Flip',windowPtr);
        [keyisdown,secs,kcode] = KbCheck;
         if keyisdown==1
            key_num = find(kcode);
         if (key_num==space)
                prepare = 1;
         end
         end
      end

       elseif rem(q,50)==0 && q==length(question)
        acc_report = ['Accuracy: ',num2str(mean(ACC(:,1))), '\n\n Press space bar'];
         prepare=0;
     while prepare==0
        DrawFormattedText(windowPtr,acc_report,'center','center',255);
        Screen('Flip',windowPtr);
        [keyisdown,secs,kcode] = KbCheck;
         if keyisdown==1
            key_num = find(kcode);
         if (key_num==space)
                prepare = 1;
         end
         end
     end
       end
    Screen('Close')    
       end

clear imgtank img

question(:,8) = question(:,8)*1000;
question(:,10) = question(:,10)*1000; %RT
question(:,12) = str2num(subj_name);
question(:,13) = subj_session;
question = question(:,[12 13 1:11]);

filename=['VST_sub',num2str(subj_name),'_sess',num2str(subj_session),'.mat'];
save(filename);
writematrix(question,['VST_sub',num2str(subj_name),'_sess',num2str(subj_session),'.csv']) 

title_msg = 'Thanks for participating the experiment';
prepare=0;
while prepare==0
    DrawFormattedText(windowPtr,title_msg,'center','center',255);
    Screen('Flip',windowPtr);
    [keyisdown,secs,kcode] = KbCheck;
         if keyisdown==1
            key_num = find(kcode);
         if (key_num==space)
                prepare = 1;
         end
         end
end

Screen('CloseAll');
catch err
   fid = fopen('errorFile','a+');
   fprintf(fid, '%s', err.getReport('extended', 'hyperlinks','off'))
   fclose(fid)
end
