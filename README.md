simulations
===========

Scripts to generate and reconstruct CLAS events. Prepared for the EG2 experiment.

## Requirements

* [env_scripts](https://github.com/utfsm-eg2-data-analysis/env_scripts)

* [clas_software](https://github.com/utfsm-eg2-data-analysis/clas_software)

* [Lepto64Sim](https://github.com/utfsm-eg2-data-analysis/Lepto64Sim)

## Instructions

Download this repository in your `${SOFT_DIR}`.

```
cd ${SOFT_DIR}
git clone http://github.com/utfsm-eg2-data-analysis/simulations
```

Then,

1. `cd ~/simulations`

2. Make some edits to `run_particleSim.sh`:

   * Make sure `SOFTDIR` points to your software directory.

   * Make sure `TOPOUDIR` points to your desired output directory.

   * You can also edit the configuration of the jobs in the respective area. **Recomendation:** set **3 hours per job** as the optimal time to generate **3500 events**.

3. To run the simulation, just execute:

   ```
   ./run_particleSim.sh --mode <mode> --Nevts <Nevts> --targ <targ> --pid <pid> --run1 <run1> --run2 <run2>
   ```

   where,
   *  `<mode>`  = 0 (interactive), 1 (farm)
   *  `<Nevts>` = number of events to generate
   *  `<targ>`  = 0 (Deuterium), 1 (Carbon), 2 (Iron), 3 (Lead)
   *  `<pid>`   = pid of detected particle, eg. 221 (eta), 223 (omega), 2212 (proton)
   *  `<run1>`  = id of initial job to loop over
   *  `<run2>`  = id of final job

For the interactive mode, the output files will be produced in `${TOPOUDIR}/ifarm/${targName}/${run}`. And for the farm mode, they will be produced in `${TOPOUDIR}/farm/${targName}/${run}`.

## Examples

* To send 10 jobs that generate 3500 events with at least one omega meson (pid = 223) in their final state, for Deuterium target, one should execute:
```
./run_particleSim.sh --mode 1 --Nevts 3502 --targ 0 --pid 223 --run1 1 --run2 10
```

* To run a quick interactive test of 10 events, with at least one proton in the final state:
```
./run_particleSim.sh --mode 0 --Nevts 12 --targ 0 --pid 2212 --run1 0 --run2 0
```

## Credits

All these scripts are possible and working thanks to Orlando Soto and Ahmed El Alaoui, and Hayk Hakobyan for implementing the EG2 experiment into GSIM.
