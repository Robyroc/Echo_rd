-module(gen_bm).
-author("Giacomo").

-callback receive_command(A :: term()) -> B :: term().
-callback add_many_resources(A :: term()) -> B :: term().
-callback get_local_resources() -> B :: term().
-callback drop_many_resources(A :: term()) -> B :: term().
