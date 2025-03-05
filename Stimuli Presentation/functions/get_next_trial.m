function trial_num = get_next_trial(subj, block)

    fpath = ['output/subj' num2str(subj) 'block' num2str(block) '.csv'];
    if (exist(fpath, 'file') == 2) % isfile(fpath) in later versions
        f = fopen(fpath);
        txt = textscan(f,'%s');
        txt = txt{1};
        last_row = strsplit(txt{end},',');
        last_trial = str2num(last_row{3});
        trial_num = last_trial + 1;
    else
        trial_num = 1;
    end

end