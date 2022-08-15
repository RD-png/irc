{ sources ? import ./nix/sources.nix }:
let
  pkgs = import sources.nixpkgs {};
  
  erlangPin = import sources.erlangPin {};
  rebar3Pin = import sources.rebar3Pin {};
  
  rebar3-10-erlangR18 = rebar3Pin.rebar3.override {
    erlang = erlangPin.erlangR18;
  };
in
pkgs.mkShell {
  buildInputs = [
    erlangPin.erlangR18
    rebar3-10-erlangR18
  ];
}
