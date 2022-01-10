open! Core

type state = unit

include Player.S with type state := state
