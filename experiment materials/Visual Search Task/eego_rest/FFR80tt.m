%¼½©ñ¶¶§Çrandperm(7); [6,3,7,5,1,2,4]=MF5,AM.MF8,MF3,F0,carrier,MF1
clear all;
close all;

% don't forget to uncomment eeg triger part
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%     %%%%%%%%%%%%%%%%%%%%%%%%%CAll back routines%%%%%%%%%%%%%%%%%%%%%%%
%initialize all pushbuttons to zero!

dur=0.2;      %1000ms for each harmonics
sr=44100;   %samples 44100 times per second
amp=1;       %amplitude for tone; temporarily set to 1
tranFLAG=2;   %%%transposed FLAG =2 is transposed to higher frequency; 1 = original


t0=0:1/44100:2;
rt=round(sr*0.01); % raising time
break_cue=sin(2*pi*440*t0);
lt=length(t0);
for n=1:rt    %rt=441 %%%% ramp-up
    break_cue(n)=break_cue(n).*(n/rt);
    break_cue(lt-n+1)=break_cue(lt-n+1).*(n/rt);
end;
%for i:rt
%%%%normalize
break_cue = break_cue./max(abs(break_cue));
break_cue = break_cue.*0.9;
% p_cue=audioplayer(break_cue,sr);
% play(p_cue);
% pause(2);

% play(p_cue);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% signal %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
isi=0.07;    %200*0.3=60 
t=0:1/sr:dur;
lt=length(t);    %number of samples in the harmonics
li=isi*sr;     %number of points that defines the silent interstimulus interval

%highCF=1000;
%Wn=(0.2*highCF)/(sr*0.5);                   % Normalized cutoff frequency
% [b,a]=butter(4, Wn,'low');  % Butterworth filter

%%%%play the fundamental tone to check
fundFreq=[80];
MF = 80*[1 2 3 4 5 6 7 8];
carrierFreq=[1000];
MF3_5 = MF(:,3:5);
MF6_8 = MF(:,6:8);



eq_amp=[0.99,0.99,0.0759];
%%
MF3_5_tone = sin(2*pi*MF3_5'*t);
MF6_8_tone = sin(2*pi*MF6_8'*t);

MF3_5_tone = sum(MF3_5_tone)./max(abs(sum(MF3_5_tone)));
MF6_8_tone = sum(MF6_8_tone)./max(abs(sum(MF6_8_tone)));

tones=zeros(lt+li,32);
m=1;

env_tone=sin(2*pi*fundFreq*t+pi/2);
% env_tone=sin(2*pi*fundFreq*t-pi/2);
env_tone = env_tone./max(abs(env_tone));

amp_eq=eq_amp(1);
carrier_tone=amp_eq.*sin(2*pi*carrierFreq*t);
carrier_tone = carrier_tone./max(abs(carrier_tone));

tranAM=carrier_tone.*(1+(m*(env_tone)));
tranAM = tranAM./max(abs(tranAM));

for n=1:rt    %rt=441  %% ramp-up
    tranAM(n)=tranAM(n).*(n/rt);
    tranAM(lt-n+1)=tranAM(lt-n+1).*(n/rt);
end;
%%%%normalize

tones(:,1)=[env_tone zeros(1,3087)];
tones(:,2)=[tranAM zeros(1,3087)];
tones(:,3)=[MF3_5_tone zeros(1,3087)];
tones(:,4)=[MF6_8_tone zeros(1,3087)];
tones(:,5)=[carrier_tone zeros(1,3087)];


%% port declaration: (This is for 64-bit computer system)

config_io;
ioObj = io64;
status = io64(ioObj);                                              
% address=hex2dec('1D00'); %address = hex2dec('D030'); %LPT 2 D050 for synAmp, D030 for NuAMp
address=hex2dec('0378'); %address = hex2dec('D030'); %LPT 2 D050 for synAmp, D030 for NuAMp
% % % outp(address,0)
% trigger_port = hex2dec('0378');

%PsychPortAudio('Close');
% InitializePsychSound(1);
% pahandle = PsychPortAudio('Open', [], [], 2, 44100, 2, 0); % devices id 8 = Focustrite
% pahandle = PsychPortAudio('Open', 2, [], 2, 44100, 2, 0); % devices id 8 = Focustrite
% pahandle = PsychPortAudio('Open', [], [], 2, 44100, 2, 0); % devices id 8 = Focustrite

AA = zeros(9000,1);
for k = 1
    for i = 1:1
        
        for repeat = 1:1800 %%% repeat 1800
            
            tic
%             PsychPortAudio('FillBuffer', pahandle, [10*tones(:,i),10*tones(:,i)]');
            WaitSecs(0.25);
                    ERPtrig = i;
                    outp(address,ERPtrig);
                    WaitSecs(0.001); % maybe 200~300 ms ?
                    outp(address,0);
%             tic
%             PsychPortAudio('Start', pahandle, 1, 0);
            
%             PsychPortAudio('Stop', pahandle, 1);
            AA((i - 1) * 1800 + repeat,1) = toc;
        end
      pause(100);  
    end
    
    
   
end
