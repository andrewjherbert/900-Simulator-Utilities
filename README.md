# 900-Simulator-Utilities

Utility programs associated with Elliott 900 Series computer simulator

A set of useful F# programs to accompany my Elliott 900 Series computer simulator:

SIRPrint -- a pretty printer for Elliott 900 Symbolic Input Routine source code.
(SIR is a 900 series assembler).

ScanTapes -- a utility to convert images of Elliott 900 series computer paper tapes
into verious UTF-8 file formats used by the simulator. (Elliott used their own paper
tape codes and it is convenient to translate them to UTF-8 using Unicode characters
corresponding to Elliott characters, or to numerial form as a sequence of integers
representing the numerical value of each row of tape.

StripHeader -- a utility to remove an initial (generally legible) header from a paper
tape file. (Normally used after header is verified using VisualizeTape.)

VisualizeTape -- A program to show the initial portion of a paper tape file in a
visual form, useful for detetcting legible headers.
