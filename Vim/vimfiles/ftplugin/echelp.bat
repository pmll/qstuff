:: Wrapper to allow ecstract3 help to be run from vim - for some reason, the
:: "start" command cannot be run directly from a vim script. We need start, or
:: something similar to allow the help to run in the background - otherwise
:: vim waits for the command to complete which means no editing and reading
:: help at the same time (not very useful).
@echo off
start hh mk:@MSITStore:C:\Texton\E3LG.chm::/%1.html
