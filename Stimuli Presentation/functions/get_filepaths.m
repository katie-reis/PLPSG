function [filepaths] = get_filepaths(subj, block, stim_dir, is_timing_test)

    test = (block ~= 2);
    if test
        % get files
        sd = strcat(stim_dir, '/test/*.wav');
        fnames = dir(sd);
        fnames = {fnames(:).name};
        test_trials = length(fnames)/5; % five tests
        % get the stimulus-block sorting for this subject
        b = ones(1, test_trials);
        blocks = [b 3*b 4*b 5*b 6*b];
        s = RandStream('mt19937ar', 'Seed', subj);
        perm = randperm(s, length(blocks));
        blocks = blocks(perm);
        % select words for this block
        filepaths = fnames(blocks == block);
        % and shuffle those
        perm = randperm(s, length(filepaths));
        filepaths = filepaths(perm);
        % make them filepaths and not names
        for i = 1:length(filepaths)
            filepaths{i} = strcat(stim_dir, '/test/', filepaths{i});
        end
        
    else
        sd = strcat(stim_dir, '/training/*.wav');
        fnames = dir(sd);
        fnames = {fnames(:).name};
        s = RandStream('mt19937ar', 'Seed', subj);
        perm = randperm(s, length(fnames));
        filepaths = fnames(perm);
        for i = 1:length(filepaths)
            filepaths{i} = strcat(stim_dir, '/training/', filepaths{i});
        end
    end
    
    if is_timing_test % make all stimuli the same
        test_stim = filepaths{1};
        for i = 2:length(filepaths)
            filepaths{i} = test_stim;
        end
    end

end