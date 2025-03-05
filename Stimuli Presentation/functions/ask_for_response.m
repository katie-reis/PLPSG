function resp = ask_for_response(ptb)

    [width, height] = Screen('WindowSize', ptb.window);
    
    ListenChar(-1); % suppresses typing into matlab command window

    resp = Ask(ptb.window, 'Type your response: ', 1, 0, ...
        'GetChar', 'center', 'center');
    
    ListenChar(0); % renables matlab command window

end