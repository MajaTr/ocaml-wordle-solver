open! Core

type state = { sampled : string list; allowed : string list }

include Player.S with type state := state 
