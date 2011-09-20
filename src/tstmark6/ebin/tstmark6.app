%% Client .app file template
%% This template is filled out by rebar, 
%% or make (Makefile made to fill modules in)
%% and then cp src/client.app.src ebin/client.app

%% Settings (defaults in include/main.hrl):
%% default_timeout (TIMEOUT = 8000)
%% lock_timeout (LOCK_TIMEOUT = 5000)
%% {env, [{default_timeout, 5000}]}

{application, tstmark6, [
    {description, "tstMark6 - Mark6 CLI"},
    {vsn, "0.1"},
    {{modules, [tstmark6]}}, 
    {mod, {client_app, ["Mon Sep 19 16:55:07 EDT 2011"]}},
    {registered, []},
    {applications, [kernel, stdlib, crypto]},
    {env, [{}]}
]}.
