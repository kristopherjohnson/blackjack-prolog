% Starts PlDoc server and launches browser.

:- doc_server(4000).
:- portray_text(true).

:- use_module(library(pldoc/doc_library)).
:- doc_load_library.

:- doc_browser.

