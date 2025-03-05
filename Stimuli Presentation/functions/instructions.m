function instructions(ptb, block)
    
    if block == 1  
        l = {'a', 'b', 'c', 'd', 'e'}; % overall instructions
    elseif block == 2
        l = {'f'}; % instructions before training
    elseif block == 2.1 % after training block 1
        l = {'g'};
    elseif block == 2.2 % after training block 2
        l = {'h'};
    elseif block == 2.3 % after training block 3
        l = {'i'};
    elseif block == 2.4 % after training block 4
        l = {'j'};
    elseif block == 2.5 % after training block 5
        l = {'k'};
    elseif ismember(block, [3 4 5 6])
        l = {'b', 'c', 'd', 'e'}; %reminder instructions
    elseif block == 0 % to be called at end of every block
        l = {'x'}; % states that subj is done with this block     
    end
    
    ListenChar(-1); % disable typing into matlab window
    for i = 1:length(l)
        fpath = ['instructions/instructions' l{i} '.txt'];
        txt = load_text_from(fpath);
        % make screen background black
        DrawFormattedText(ptb.window, txt, 'center', 'center', 1);
        Screen('Flip', ptb.window);
        KbPressWait();
    end
    ListenChar(0); % reenable typing into matlab
    
end


function [txt] = load_text_from(fpath)
    txt = importdata(fpath);
    txt = strjoin(txt, '\n');
end