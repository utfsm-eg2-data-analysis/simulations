# LEPTO 6.5.1

MC Event Generator. It depends on `pythia5721.f` and `jetset7410.f`. The original program is called `lepto-6.5.1.f`. And there are currently two modified versions of LEPTO: (located at `src_f/`)

1. `qp1.f`: it will generate events that have **at least one** chosen particle in the final state. The chosen particle is given by an input text file (explained in the next sections).

2. `qp1_bkg.f`: same as previous one, but with the opposite condition. If will generate events where the chosen particle is **not present** at the final state.

For both cases, all LEPTO parameters are written within them.

One can check the difference between both codes by executing: `diff qp1.f qp1_bkg.f`

## Compilation

When using the JLAB cluster, it's necessary to set some environment variables. To do so, during a bash session, execute:

```
source ../set_env.sh
```

Then, compile by running `make`. One can comment **Lines 11 and 17** and uncomment **Lines 12 and 18** in the `Makefile` to swap between versions of `qp1`. The ideal is to compile both of them.

## Usage

After the compilation, two executables will appear within the `bin` directory: `lepto.exe` and `lepto_bkg.exe`

To execute it, there must be a text file called `lepto.txt` in the execution directory with the following content:

```
<Nevts> <tarA> <tarZ> <pid>
```

Where:

* `<Nevts>` : number of events to generate

* `<tarA>` : A (atomic mass) of the nuclear target

* `<tarZ>` : Z (atomic number) of the nuclear target

* `<pid>` : pid

For example:

```
12 2 1 223
```

Then, when running it (`./lepto.exe`), it will generate 12 events with at least one omega meson (pid=223) in the final state, simulating interactions with a Deuterium target (A=2, Z=1).

## References

*work in progress...*