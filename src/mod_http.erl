%%%-------------------------------------------------------------------
%%% @author 10892
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. 4月 2023 17:06
%%%-------------------------------------------------------------------
-module(mod_http).
-author("10892").
-include("../include/command.hrl").


%% API
-export([get_request_info/1]).


%% @doc 获取请求url 和 header
get_request_info(Action) ->
	{ok, Ip} = application:get_env(mmo_server, ejabberd_server_ip),
	{ok, Port} = application:get_env(mmo_server, mod_http_api_port),
	{ok, RootUsername} = application:get_env(mmo_server, ejabberd_admin),
	{ok, RootPassword} = application:get_env(mmo_server, ejabberd_admin_password),
	Command = get_command(Action),
	Url = "http://" ++ Ip ++ ":" ++ Port ++ "/api/" ++ utils:to_list(Command),
	AuthHeaders = [{"Authorization", "Basic " ++ base64:encode_to_string(RootUsername ++ ":" ++ RootPassword)}],
	Headers = [{"Content-Type", "application/json"}],
	FullHeaders = Headers ++ AuthHeaders,
	{Url, FullHeaders}.

%% @doc ejabberd 命令映射
get_command(?EJ_CREATE_MUC) ->
	create_room_with_opts;

get_command(?EJ_REMOVE_MUC) ->
	destroy_room;

get_command(?EJ_CREATE_USER) ->
	register.