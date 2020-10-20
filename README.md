# simulations

Scripts to generate and reconstruct CLAS events. Prepared for the EG2 experiment. Exclusive to run in the JLAB farm.

## Requirements

* A bash session

## Instructions

To download this repository (recommended in home directory)

```
git clone http://github.com/utfsm-eg2-data-analysis/simulations
```

Then,

1. `cd ~/simulations`

2. Make some edits to `run_particleSim.sh`:

   * Replace the content of the environment variables `SIMINDIR` and `TOPOUDIR` **(Lines 134, 136 and 138)** to point the repository and the production directory (avoid your home directory for this one)

   * **Lines 236-242** allow you to set the configuration for each job. I personally recommend, for the default value of 2 hours, to generate 3500 events per job. It's highly recommended to test a few jobs before sending a large batch of jobs.

   * You can uncomment Lines **268-285** to retrieve the intermediate files of the process.

3. To run the simulation type:

   ```
   ./run_particleSim.sh --mode <mode> --Nevts <Nevts> --targ <targ> --pid <pid> --bkg <bkg>--run1 <run1> --run2 <run2>
   ```

where,
*  `<mode>`  = 0 (interactive), 1 (farm)
*  `<Nevts>` = number of events to generate
*  `<targ>`  = 0 (Deuterium), 1 (Carbon), 2 (Iron), 3 (Lead)
*  `<pid>`   = pid of detected particle, eg. 221 (eta), 223 (omega), 2212 (proton)
*  `<bkg>`   = 0 (generate events with at least one of selected particle in the final state), 1 (generate all particles but selected one)
*  `<run1, run2>` = integers to loop over (>= 0)

For the interactive mode, the output files will be produced in `${TOPOUDIR}/ifarm/${run}`. And for the farm mode, they will be produced in `${TOPOUDIR}/farm/${run}`.

For example, to send 10 jobs that generate 3502 events with at least one omega meson (pid = 223) in their final state, for Deuterium target, one should execute:
```
./run_particleSim.sh --mode 1 --Nevts 3502 --targ 0 --pid 223 --bkg 0 --run1 1 --run2 10
```

Another example, to run a quick interactive test.
```
./run_particleSim.sh --mode 0 --Nevts 102 --targ 0 --pid 2212 --bkg 0 --run1 0 --run2 0
```

A more recent example, to run a quick interactive test of background simulations for eta mesons (pid = 221).
```
./run_particleSim.sh --mode 0 --Nevts 102 --targ 0 --pid 221 --bkg 1 --run1 0 --run2 0
```

Lastly, one can type `./run_particleSim.sh` with no arguments to obtain a reminder of the instructions.

## Credits

All these scripts are possible and working thanks to Orlando Soto and Ahmed El Alaoui, and Hayk Hakobyan for implementing the EG2 experiment into GSIM.
