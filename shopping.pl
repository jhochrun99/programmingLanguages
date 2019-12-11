:- use_module(library(clpfd)).

shopping :-
    N = 5,
    Component = [_Motherboard, Screen, Keyboard, GPU,Wireless],
    Component ins 1..N,

    Store = [NerdsRUs, BigHouseOfComputer, ChipFactory, SoftwareVillage,BobsElectronics],
    Store ins 1..N,

    all_different(Component),
    all_different(Store),

    % 1. Your second stop was the Software Village.
    SoftwareVillage #= 2,

    % 2. You bought a GPU at NerdsRUs.
    NerdsRUs #= GPU,

    % 3. The store you went to after you bought the Keyboard was the BigHouseOfComputer.
    BigHouseOfComputer #= Keyboard+1,

    % 4. Two stops after leaving the Chip Factory, you bought your computer's LCD Screen.
    Screen #= ChipFactory+2,
    
    % 5. Your last stop was BobsElecontrics, where you bought your Wireless network interface.
    BobsElectronics #= N,
    Wireless #= N,

    flatten([Component,Store],Vars),

    label(Vars),
    pairs_keys_values(ComponentResult, [motherboard, screen, keyboard, gpu,wireless], Component),
    pairs_keys_values(StoreResult, [nerdsRUs, bigHouseOfComputer, chipFactory, softwareVillage,bobsElectronics], Store),
    writeln(shoes=ComponentResult),
    writeln(store=StoreResult).
