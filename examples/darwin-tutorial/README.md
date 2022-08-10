# Tutorial Instructions

The overall workflow of this tutorial is to run
- `darwin-setup.jl` (which creates the executable and sets up the proper folders on your machine)
- `darwin-run.jl` (where you modify runtime parameters, and then run the model)
- `darwin-plot.jl` (where we look at output)

**Note:** for the plots to show up, darwin-plot.jl must be run from a REPL or notebook. For example, I use VSCode and choose the "Execute active file in REPL" option in the run config. 

## darwin-setup.jl


To run on your local machine, make the following changes
- change `MITgcm_path[1]` to point to the root directory of your local version of the darwin MITgcm
- **note**: BUILD DOES NOT WORK FOR ME
- run darwin-setup
- copy down the `config_id` that is printed out in the terminal 

## darwin-run.jl

Make the following changes
- update `MITgcm_path[1]` to be the root directory of your local version of the darwin MITgcm
- update `config_id` to be the config_id that was output from running darwin-setup
- modifying runtime parameters
    - TODO

## darwin-plot.jl

Update 
- `config_id`
- `data_folder` (TODO: more info)
- `folder`

To see the plots this **MUST** be run from a REPL! 
