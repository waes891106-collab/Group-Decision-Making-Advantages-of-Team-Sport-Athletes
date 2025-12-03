% 2014/06/10 resting EEG before formal exp.
% eyes closed 3 min & opened 3 min
%% Resting EEG for 6 min total
clear all; close all; clc;
HideCursor;
FlushEvents;
echo off
AssertOpenGL;
KbName('UnifyKeyNames');
%% Subject info.:
Sub = input('type SubID :');
% Condition = input('1 is for control; 2 is for Tennis player.:');
%% Parameters:
TrialNum = 4;
%% port declaration: (This is for 64-bit computer system)
config_io;
address = hex2dec('3FF8');
outp(address,0);
%% trigger names:
% 1: eyes opened.
% 2: eyes closed.
%% Screen parameters:
screens=Screen('Screens');
screenNumber=max(screens);
bgBlack=BlackIndex(screenNumber); 
[theWindow, wRect]=Screen('OpenWindow',screenNumber, bgBlack);
Screen('TextFont',theWindow, 'PMingLiU')
Screen('TextSize', theWindow, 40);
white = [0 0 0];
KbCheck;
WaitSecs(0.1);
priorityLevel=MaxPriority(theWindow);
Priority(priorityLevel);
%% Instruction:
message = '請依照螢幕上指示依序睜眼及閉眼， \n \n 若準備好，請按任意鍵開始。' ;
DrawFormattedText(theWindow, double(message), 'center', 'center', WhiteIndex(theWindow));
% Update the display to show the instruction text:
Screen('Flip', theWindow);
[secs, keyCode, deltaSecs] = KbWait; %% wait until any key is pressed.
% Wait 0.5-s before starting Exp.
WaitSecs(0.5);
%% Exp. begins:
for trial = 1: TrialNum
    if mod(Sub,2) %% if Sub number => odd: open-close-open-close
        if mod(trial,2)
           %disp('odd')
           messageOPEN = '接著，請放鬆並"盯著"凝視點一分半鐘，\n \n 聽到嗶聲後可輕鬆休息。 \n \n 若準備好，請按任意鍵開始。';
           DrawFormattedText(theWindow, double(messageOPEN), 'center', 'center', WhiteIndex(theWindow));
           triggerNum = 1;
        else 
           %disp('even')
           messageCLOSE = '接著，請放鬆並"閉上雙眼"一分半鐘，\n \n 聽到嗶聲後即可睜開眼睛。 \n \n 若準備好，請按任意鍵開始。';
           DrawFormattedText(theWindow, double(messageCLOSE), 'center', 'center', WhiteIndex(theWindow));
           triggerNum = 2;
        end
    else %% if Sub number => even: close-open-close-open
        if mod(trial,2)
           messageCLOSE = '接著，請放鬆並"閉上雙眼"一分半鐘，\n \n 聽到嗶聲後即可睜開眼睛。 \n \n 若準備好，請按任意鍵開始。';
           DrawFormattedText(theWindow, double(messageCLOSE), 'center', 'center', WhiteIndex(theWindow));
           triggerNum = 2;
        else 
           messageOPEN = '接著，請放鬆並"盯著"凝視點一分半鐘，\n \n 聽到嗶聲後可輕鬆休息。 \n \n 若準備好，請按任意鍵開始。';
           DrawFormattedText(theWindow, double(messageOPEN), 'center', 'center', WhiteIndex(theWindow));
           triggerNum = 1;
        end
    end % for if mod(Sub,2) 
    Screen('Flip', theWindow);
    [secs, keyCode, deltaSecs] = KbWait; %% CHECK
    % Wait 0.5-s before starting Exp.
    %WaitSecs(0.5);
    fixation = '+';
    DrawFormattedText(theWindow, fixation, 'center', 'center', WhiteIndex(theWindow));
    %% sending trigger:
    if triggerNum == 1 % open
        outp(address,triggerNum);
    elseif triggerNum == 2 % close
        outp(address,triggerNum);
    end
    Screen('Flip', theWindow);
    outp(address,0);
    WaitSecs(2);%% (90):3-min %% 2-s for testing
    %% beep sound:
    beep = MakeBeep(500,0.5);
    Snd('Open');
    Snd('Play',beep);
    Snd('Close');
    %% take a break:
    rest = '休息一下。 \n \n 若準備好，請按任意鍵開始。 ';
    DrawFormattedText(theWindow, double(rest), 'center', 'center', WhiteIndex(theWindow));
    Screen('Flip', theWindow);
    KbWait; 
    % Wait 0.5-s before starting next session.
    WaitSecs(0.5);
end % for trial = 1:TrialNum
beepEnd = MakeBeep(1000, 0.5);
% beep when experiment end.
    for i =1:5
    Snd('Open');
    Snd('Play', beepEnd);
    Snd('Close');
    end    

Screen('Flip', theWindow);
Screen('CloseAll');
ShowCursor;
Priority(0);
