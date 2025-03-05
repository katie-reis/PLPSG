# Generalized Perceptual Learning Task

## Task Description
Eleven blocks (5 test and 6 training) of difficult-to-understand synthetic speech made using RSynth. Test blocks (1, 3-6) have 80 stimuli each, and training blocks (2) has 6 sets of 50 stimuli, which are presented with feedback. All words are randomized across test blocks on a per-subject basis, and stimuli in all blocks are presented in random order (also per-subject). All stimuli are sampled at 44100 Hz, though they were originally generated at a lower sampling rate. No stimulus appears more than once.

## How to Run
Clone this repository onto your machine using git. The main script is `run_test.m` (which should be run by typing `run_test` into the matlab command line with `experiment` as your working directory). Make sure to update subject and block numbers, hard coded in the first code block, each time you run.

You may have to do a bit of initial setup to get it working on your particular machine.
* If you don't have PsychToolBox in your matlab path, you will need to add that.
* You may need to edit the contents of `init_psychtoolbox.m` to reflect the details of your hardware configuration (like if you have more than one soundcard, keyboard, or display, which you'd like to use).
* Right now, the script uses RTBox to send triggers to Brainvision Recorder or pyCorder. If you want to send triggers to a different software, you'll need to rewrite `send_trigger.m`.

When testing, set `RTBOX` to false in the first code block. Otherwise, it should be turned on if you want triggers sent.

If the code is terminated prematurely, all responses up until that point will have already been saved, and the code will pick back up on the trial it left off (though it will repeat the instructions for that block).

## Other Notes
`USTCRTBox_003` is the software library for RTBox. Follow [this link](https://lobes.osu.edu/rt-box.php) if you want an updated version or need to install any RTBox drivers. Also, please cite the kind people who bring us RTBox if you use their device or code:

Li, X., Liang, Z., Kleiner, M., & Lu, Z. L. (2010). RTbox: a device for highly accurate response time measurements. Behavior research methods, 42(1), 212-225.

Cough, cough, and cite our perceptual learning papers if you use our perceptual learning task :-)







