-module(gen_bm).
-author("Giacomo").

-callback receive_command(From :: term(), Command :: term()) -> C :: term().
-callback add_many_resources(Resources :: term()) -> B :: term().
-callback get_local_resources() -> B :: term().
-callback drop_many_resources(From :: term()) -> B :: term().
