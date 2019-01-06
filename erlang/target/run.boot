%% Load protos files %%
code:load_abs('protos').
rr("./../src/protos.hrl").

%% Load zmq files %%
cd("./../deps/erlzmq2/ebin/").
code:load_abs('erlzmq').
code:load_abs('erlzmq_nif').
cd("./../../../target/").
rr("./../src/erlzmq.hrl").

%%  Load erlang files %%

code:load_abs('client').
code:load_abs('main').
code:load_abs('translater').
code:load_abs('accounts').
code:load_abs('exchanges').

%%  RUN %%

main:main().
