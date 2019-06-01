-module(logging_policies).
-author("mrbo9").

%% API
-export([check_policy/1, check_lager_policy/1]).

check_policy(Module) ->
  Policy = application:get_env(echo_rd, log),
  case Policy of
    undefined -> unable;
    {ok, Profile} -> check_profile(Module, Profile)
  end.

check_lager_policy(Module) ->
  LagerPolicy = application:get_env(echo_rd, lager_log),
  case LagerPolicy of
    undefined -> {unable, unable};
    {ok, lager_on} ->
      case naming_handler:get_maybe_identity(lager_sinks_handler) of
        no_name_registered ->
          {lager_off, check_policy(Module)};
        _ ->
          lager_sinks_handler:start_if_not_started(),
          {lager_on, check_policy(Module)}
      end;
    {ok, lager_only} ->
      case naming_handler:get_maybe_identity(lager_sinks_handler) of
        no_name_registered ->
          {undefined, check_policy(Module)};
        _ ->
          lager_sinks_handler:start_if_not_started(),
          {lager_only, check_policy(Module)}
      end;
    {ok, lager_off} ->
      case naming_handler:get_maybe_identity(lager_sinks_handler) of
        no_name_registered -> ok;
        _ -> lager_sinks_handler:terminate_if_not_terminated()
      end,
      {lager_off, check_policy(Module)};
    _ ->
      case naming_handler:get_maybe_identity(lager_sinks_handler) of
        no_name_registered -> ok;
        _ -> lager_sinks_handler:terminate_if_not_terminated()
      end,
      {unable, unable}
  end.

check_profile(communication_manager, all) -> able;
check_profile(communication_manager, comm_only) -> able;

check_profile(checker, all) -> able;
check_profile(checker, logic) -> able;
check_profile(checker, chord_only) -> able;
check_profile(fixer, Policy) -> check_profile(checker, Policy);
check_profile(router, Policy) -> check_profile(checker, Policy);

check_profile(join_handler, all) -> able;
check_profile(join_handler, logic) -> able;
check_profile(join_handler, joiner_only) -> able;

check_profile(naming_handler, all) -> able;
check_profile(naming_handler, naming_only) -> able;

check_profile(naming_manager, Policy) -> check_profile(naming_handler, Policy);

check_profile(_, _) -> unable.