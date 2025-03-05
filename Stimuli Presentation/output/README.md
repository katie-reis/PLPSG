in order for the run_test.m code to work, you will need to have this folder "output" for new subject generated data to be generated into. 
The run_test.m code will check for whether a file that already has the given subject ID number and block number has been generated. 
In the case that the file has already been generated, it will throw an error if all trials have been completed, 
or pick up where you left off if the code was interrupted part way through.
