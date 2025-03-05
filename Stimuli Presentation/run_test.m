%% UPDATE THIS SECTION BEFORE EACH SUBJECT/TEST
clear all;

SUBJ_NUM = 107;
BLOCK = 4; % 2 is training block, others are test blocks

%% other constants
TIMING_TEST = false;
RTBOX = true; % set to false ONLY if testing code without RTBox
STIMULUS_DIRECTORY = 'words';
FS = 44100; % sampling rate of .wav files
addpath('functions');
addpath('USTCRTBox_003'); % RTBox library

%% set up psychtoolbox and RTBox
init_RTBox(RTBOX);
ptb = init_psychtoolbox(FS);

%% run experiment
training = (BLOCK == 2);

stimuli = get_filepaths(SUBJ_NUM, BLOCK, STIMULUS_DIRECTORY, TIMING_TEST);

if ~TIMING_TEST
    instructions(ptb, BLOCK);
end

start_trial = get_next_trial(SUBJ_NUM, BLOCK);
if start_trial > length(stimuli)
    sca;
    error('You may have already recorded this subject-block.');
end

for s = start_trial:length(stimuli)
    fixation(ptb); % shows fixation cross to start trial
    present_stimulus(stimuli{s}, BLOCK, ptb); % trigger sent here
    if ~TIMING_TEST
        response = ask_for_response(ptb);
        write_output(SUBJ_NUM, BLOCK, s, stimuli{s}, response);
    end
    if training
        give_feedback(stimuli{s}, ptb); % another trigger (7) sent here
        % give subject a break between training blocks
        if ismember(s, [50 100 150 200 250]) && ~TIMING_TEST 
             instructions(ptb, BLOCK + s/50/10);
        end
    end
end


%% end block
instructions(ptb, 0);
sca; % screen clear all
PsychPortAudio('Close'); % clear audio handles



