-module(logging_policies).
-author("mrbo9").

%% API
-export([check_policy/1]).

check_policy(Module) ->
  Policy = application:get_env(echo_rd, log),
  case Policy of
    undefined -> unable;
    {ok, Profile} -> check_profile(Module, Profile)
  end.


check_profile(communication_manager, all) -> able;
check_profile(communication_manager, lager_only) -> able_lager;
check_profile(communication_manager, comm_only) -> able;

check_profile(checker, all) -> able;
check_profile(checker, logic) -> able;
check_profile(checker, chord_only) -> able;
check_profile(checker, lager_only) -> able_lager;

check_profile(fixer, Policy) -> check_profile(checker, Policy);
check_profile(router, Policy) -> check_profile(checker, Policy);

check_profile(join_handler, all) -> able;
check_profile(join_handler, logic) -> able;
check_profile(join_handler, joiner_only) -> able;
check_profile(join_handler, lager_only) -> able_lager;

check_profile(naming_handler, all) -> able;
check_profile(naming_handler, naming_only) -> able;
check_profile(naming_handler, lager_only) -> able_lager;

check_profile(naming_manager, Policy) -> check_profile(naming_handler, Policy);

check_profile(_, _) -> unable.