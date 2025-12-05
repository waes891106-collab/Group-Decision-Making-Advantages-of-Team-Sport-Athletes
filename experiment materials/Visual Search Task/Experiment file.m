clear;clc;
setnum=50;%how many img for each set
AssertOpenGL;
PsychDefaultSetup(2);
% % ask for subject name and test number-----------------------------------
wp='C:\Users\User\Desktop\1+1πÍ≈Á\1+1 πÍ≈Á3';
user_ans = inputdlg({'Enter subject1 name:','Enter subject2 name:','Enter session number:'},'Inputs');
subj1_name  = user_ans{1};
subj2_name = user_ans{2};
subj_session = str2double(user_ans{3});

% % --------------------------------------------------------------------end

ListenChar(2);
HideCursor;

% % prepare screen parameters----------------------------------------------
screenNumber = max(Screen('Screens'));
[windowPtr,rect] = Screen('OpenWindow',screenNumber);
Screen('FillRect',windowPtr,128,rect);
Screen('TextFont',windowPtr,'Arial');
Screen('TextSize',windowPtr,24);
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
% % prepare shown symbols--------------------------------------------------
fixation = '+';
ques_resp = '?';
confidence_resp = 'Confidence Rating';
anscorrect = 'Correct';
answrong = 'Wrong';
noanswer = 'No respond';
resting_subrun = 'Take a rest';
resting_end = 'Experiment over!';
% % --------------------------------------------------------------------end

%read image----------------------------------------------------------------
QAsheet=[];imgtank=[];
%QAsheet [T L PICindex rotation]
for difficult=0:5
    for setindex=1:setnum
        imgindex=difficult*100+setindex;
        imglocation=[wp,'\PIC25and60\',num2str(imgindex,'%03.0f'),'.png'];
        img=imread(imglocation);
        imgtank{end+1}=img;
        switch difficult
            case 0
                text=[0 25];
            case 1
                text=[1 25];
            case 2
                text=[2 25];
            case 3
                text=[0 60];
            case 4
                text=[1 60];
            case 5
                text=[2 60];
        end
        
       
        QAsheet=[QAsheet;text length(imgtank) imgindex];
        
    end
end
fix_time = (randi([500 1000],1,300)/1000)';

centerimg=imread([wp '\center.png']);
blankimg=imread([wp '\blank.png']);
correctimg=imread([wp '\correct.png']);
wrongimg=imread([wp '\wrong.png']);                
    
    
    
    

% ----------------------------------------------------------------------end

%makequestion--------------------------------------------------------------
examsort=randperm(length(QAsheet));
question=QAsheet(examsort,:);
trysort=randperm(length(QAsheet));
tryquestion=QAsheet(trysort,:);
%-----------------------------------------------------------------------end

% %kb name ------------------------------------------------------------------
zero= KbName('LeftArrow');
onemore= KbName('DownArrow');
twomore= KbName('RightArrow');
first=KbName('1!');
second=KbName('2@');
third=KbName('3#');
fourth=KbName('4$');
fifth=KbName('5%');
% % experiment start-------------------------------------------------------
title_msg = 'Welcome to our experiment\n\n press when ready';
DrawFormattedText(windowPtr,title_msg,'center','center',0);
Screen('Flip',windowPtr);
KbWait;
while KbCheck; end
% try
for q=1:10
    
   %blank
    blank_start_time = GetSecs;
    while (GetSecs-blank_start_time <= 0.5)
        imageTexture = Screen('MakeTexture', windowPtr,blankimg);
        Screen('DrawTexture', windowPtr, imageTexture);
        Screen('Flip', windowPtr);
    end
    
    % fixation
    fixation_start_time = GetSecs;
    while (GetSecs-fixation_start_time <= fix_time(q))
        imageTexture = Screen('MakeTexture', windowPtr,centerimg);
        Screen('DrawTexture', windowPtr, imageTexture);
        Screen('Flip', windowPtr);
    end
    
    
    
    
    
    imageTexture = Screen('MakeTexture', windowPtr, imgtank{tryquestion(q,3)});
    has_respond = 0;
    starttime=GetSecs;
    while has_respond==0
        Screen('DrawTexture', windowPtr, imageTexture);
        Screen('Flip', windowPtr);
        
        [keyisdown,secs,kcode] = KbCheck;
        if keyisdown==1
            key_num = find(kcode);
            if (key_num==zero)
                has_respond = 1;
                resp=0;
                
            elseif (key_num==onemore)
                has_respond = 1;
                resp=1;
                
            elseif (key_num==twomore)
                has_respond = 1;
                resp=2;
            end
            
            while KbCheck; end
        end
    end
    if resp==tryquestion(q,1);
        iscorrect=1;
    else
        iscorrect=0;
    end
    
    % show answer
    
    showans_start_time = GetSecs;
    if iscorrect
        imageTexture = Screen('MakeTexture', windowPtr,correctimg);
    else
        imageTexture = Screen('MakeTexture', windowPtr,wrongimg);
    end
    while (GetSecs-showans_start_time <= 1)
        Screen('DrawTexture', windowPtr, imageTexture);
        Screen('Flip', windowPtr);
    end
    
    Screen('Close') %190115
end

title_msg = 'For next exercise, changing the keyboard to another one\n\n press when ready';
DrawFormattedText(windowPtr,title_msg,'center','center',0);
Screen('Flip',windowPtr);
KbWait;
while KbCheck; end
for q=11:20
    
    %blank
    blank_start_time = GetSecs;
    while (GetSecs-blank_start_time <= 0.5)
        imageTexture = Screen('MakeTexture', windowPtr,blankimg);
        Screen('DrawTexture', windowPtr, imageTexture);
        Screen('Flip', windowPtr);
    end
    
    % fixation
    fixation_start_time = GetSecs;
    while (GetSecs-fixation_start_time <= fix_time(q))
        imageTexture = Screen('MakeTexture', windowPtr,centerimg);
        Screen('DrawTexture', windowPtr, imageTexture);
        Screen('Flip', windowPtr);
    end
    
    
    
    
    
    imageTexture = Screen('MakeTexture', windowPtr, imgtank{tryquestion(q,3)});
    has_respond = 0;
    starttime=GetSecs;
    while has_respond==0
        Screen('DrawTexture', windowPtr, imageTexture);
        Screen('Flip', windowPtr);
        
        [keyisdown,secs,kcode] = KbCheck;
        if keyisdown==1
            key_num = find(kcode);
            if (key_num==zero)
                has_respond = 1;
                resp=0;
                
            elseif (key_num==onemore)
                has_respond = 1;
                resp=1;
                
            elseif (key_num==twomore)
                has_respond = 1;
                resp=2;
            end
            
            while KbCheck; end
        end
    end
    if resp==tryquestion(q,1);
        iscorrect=1;
    else
        iscorrect=0;
    end
    
    % show answer
    
    showans_start_time = GetSecs;
    if iscorrect
        imageTexture = Screen('MakeTexture', windowPtr,correctimg);
    else
        imageTexture = Screen('MakeTexture', windowPtr,wrongimg);
    end
    while (GetSecs-showans_start_time <= 1)
        Screen('DrawTexture', windowPtr, imageTexture);
        Screen('Flip', windowPtr);
    end
    Screen('Close') %190115
end

title_msg = 'Start experience\n\n  press when ready';
DrawFormattedText(windowPtr,title_msg,'center','center',0);
Screen('Flip',windowPtr);
KbWait;
while KbCheck; end





resp_ans=zeros(length(question),1)-1;
resp_time=zeros(length(question),1);
% test
for q=1:length(question)
    
   %blank
    blank_start_time = GetSecs;
    while (GetSecs-blank_start_time <= 0.5)
        imageTexture = Screen('MakeTexture', windowPtr,blankimg);
        Screen('DrawTexture', windowPtr, imageTexture);
        Screen('Flip', windowPtr);
    end
    
    % fixation
    fixation_start_time = GetSecs;
    while (GetSecs-fixation_start_time <= fix_time(q))
        imageTexture = Screen('MakeTexture', windowPtr,centerimg);
        Screen('DrawTexture', windowPtr, imageTexture);
        Screen('Flip', windowPtr);
    end
    
    
    imageTexture = Screen('MakeTexture', windowPtr, imgtank{question(q,3)});
    has_respond = 0;
    starttime=GetSecs;
    while has_respond==0
        Screen('DrawTexture', windowPtr, imageTexture);
        Screen('Flip', windowPtr);
        
        [keyisdown,secs,kcode] = KbCheck;
        if keyisdown==1
            key_num = find(kcode);
            if (key_num==zero)
                has_respond = 1;
                resp_ans(q) = 0;
                resp_time(q)=GetSecs-starttime;
            elseif (key_num==onemore)
                has_respond = 1;
                resp_ans(q) = 1;
                resp_time(q)=GetSecs-starttime;
                
            elseif (key_num==twomore)
                has_respond = 1;
                resp_ans(q) = 2;
                resp_time(q)=GetSecs-starttime;
            end
            while KbCheck; end
        end
    end
    if rem(q,30)==0 && q~=length(question)
        title_msg = 'saving, please wait  ';
        DrawFormattedText(windowPtr,title_msg,'center','center',0);
        Screen('Flip',windowPtr);
        
        %0109 save data in every iter. If it makes laggard, please delete
        %it.------------------------------------------------------
        filename=['exp3test2_sub',num2str(subj1_name),'_sub',num2str(subj2_name),'_sess',num2str(subj_session),'.mat'];
        save(filename);
        %---------------------------------------------------------        
        
        title_msg = 'Please changing the keyboard, then pressing';
        DrawFormattedText(windowPtr,title_msg,'center','center',0);
        Screen('Flip',windowPtr);
        KbWait;
        while KbCheck; end  
        
        
       title_msg = 'for next quesion, pressing when ready';
        DrawFormattedText(windowPtr,title_msg,'center','center',0);
        Screen('Flip',windowPtr);
        KbWait;
        while KbCheck; end 
    end
    
        
   Screen('Close') %190115 
end
% catch Screen('CloseAll')
% end




% config_msg='what is your confidence rate? 1(min)~5(max)'
% DrawFormattedText(windowPtr,config_msg,'center','center',0);
% Screen('Flip',windowPtr);
% has_respond = 0;
% resp_fid=0;
% while has_respond == 0
%     [keyisdown,secs,kcode] = KbCheck;
%     if keyisdown==1
%         key_num = find(kcode);
%         if (key_num==first)
%             has_respond = 1;
%             resp_fid = 1;
%         elseif (key_num==second)
%             has_respond = 1;
%             resp_fid = 2;
%         elseif (key_num==third)
%             has_respond = 1;
%             resp_fid = 3;
%         elseif (key_num==fourth)
%             has_respond = 1;
%             resp_fid = 4;
%         elseif (key_num==fifth)
%             has_respond = 1;
%             resp_fid = 5;
%         end
%         while KbCheck; end
%     end   
% end  
% 
clear imgtank img
filename=['exp3test2_sub',num2str(subj1_name),'_sub',num2str(subj2_name),'_sess',num2str(subj_session),'.mat'];
save(filename);

title_msg = 'Thanks for participating the experiment, pressing to exit';
DrawFormattedText(windowPtr,title_msg,'center','center',0);
Screen('Flip',windowPtr);
KbWait;
while KbCheck; end

Screen('CloseAll');
