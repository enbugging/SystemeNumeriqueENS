# Netlist simulator

This repository contains my netlist simulator implemented in the context of the course Digital System at ENS. Specifically, the implementation is provided in folder `\tp1`, with detailed report in `\tp1\summary`. What follows provides an overview of the simulator. The repository is hosted on [github](https://github.com/enbugging/SystemeNumeriqueENS).

## Compilation and usage

Compilation requires build tool `ocamlbuild` of version 0.14.0 or higher. The simulator is implemented in OCaml version 5.1.0. The implementation was compiled and tested in Linux Ubuntu 20.04.6 LTS through Window Subsystem for Linux.To compile, use command `ocamlbuild netlist_simulator.byte`.

The execution has basic interface, of the form `netlist_simulator.byte [-print] [-n <nr>] netfile.net`, with
- `-print`: flag to print only the netlist after scheduling;
- `-n <nr>`: flag to specify \texttt{nr}, the number of cycles to be simulated;
- `netfile.net`, the `.net` file containing the description of circuit in netlist. For further information, consult the documentation provided with the package, title `minijazz.pdf`.

## Functionalities

As required, it can simulate circuits described in netlist language. I tried to implement the interface as close as possible to the provided prebuilt netlist simulator. Registers, RAM, and ROM are maintained using dictionary, so in particular, there is no hard limit for memory except that allowed for the simulator, at the expense of an additional log factor to the complexity. By default, all variables and memory are initialised to `0` or `False`. 
	
This convention does lead to the following behavior. When the scheduler is evoke, for the equation `RAM addr_size word_size read_addr write_enable write_addr write_data`, we only required that `read_addr`, `write_enable`, `write_addr`, i.e. read address, flag to switch from read to write, and write address, be calculated and well-defined, and leave `write_data`, if not calculated, by default to \texttt{False}.