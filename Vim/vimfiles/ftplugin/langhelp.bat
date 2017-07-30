:: Wrapper to allow language help to be run from vim - for some reason, the
:: "start" command cannot be run directly from a vim script. We need start, or
:: something similar to allow the help to run in the background - otherwise
:: vim waits for the command to complete which means no editing and reading
:: help at the same time (not very useful).
:: Parameters:
:: 1 - Help version (currently "release" or "dev")
:: 2 - Help file (currently "E3LG" or "ecstract")
:: 3 - Help topic (usually taken from language keyword under cursor)
@echo off
start hh mk:@MSITStore:C:\QLangHelp\%1\%2.chm::/%3.html
