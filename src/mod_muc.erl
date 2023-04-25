%%%-------------------------------------------------------------------
%%% @author 10892
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. 4月 2023 14:24
%%%-------------------------------------------------------------------
-module(mod_muc).
-include("../include/command.hrl").
-author("10892").

%% API
-export([sync_action/2, action/2]).
-export([action_result/3]).

%% @doc  同步  执行action
-spec sync_action(Type::atom(), Args :: tuple()) -> ok | error.
sync_action(Action, Args) ->
	{Url, Headers}  = mod_http:get_request_info(Action),
	Body  = get_request_body(Action, Args),
	case catch httpc:request(post, {Url, Headers, "application/json", Body}, [], []) of
		{ok, {{_, 200, _}, _Header,  _BodyRes}} ->
			ok;
		{ok, {{_, ErrCode, _}, _Header,  BodyRes}} ->
			io:format("sync action  ~p args ~p fail Errcode ~p, Messsage ~p ~n",
				[Action, Args, ErrCode, utils:decode(BodyRes)]),
			error;
		_ ->
			error
	end.

%% @doc  异步  执行action
-spec action(Type::atom(), Args :: tuple()) -> ok | error.
action(Action, Args) ->
	{Url, Headers}  = mod_http:get_request_info(Action),
	Body  = get_request_body(Action, Args),
	httpc:request(post, {Url, Headers, "application/json", Body}, [], [{sync,false}, {receiver,
		{?MODULE, action_result, [Action, Args]}}]).

%% @doc  异步  执行action 结果
action_result({_Ref, ReplyInfo}, Action, Args) ->
	case ReplyInfo of
		{{_, 200, _}, _, _} -> %% 创建用户 成功
			io:format("action  ~p args ~p success  ~n", [Action, Args]),
			ok;
		{{_, ErrCode, _}, _Header, Body} ->
			io:format("action  ~p args ~p fail Errcode ~p, Messsage ~p ~n",
				[Action, Args, ErrCode, utils:decode(Body)]),
			error
	end.

%% @doc 按照action 和 args  生成 body 参照ejabberd command 命令参数填充
%% @todo 重启ejabberd服务 之后 聊天记录不见了
get_request_body(?EJ_CREATE_MUC, {Type, Key}) ->
	RoomName = utils:to_bin(utils:to_list(Type) ++ "_" ++ integer_to_list(Key)),
	utils:encode([
		{name, RoomName},
		%% @todo 是否需要按照功能类型区分 eg. guild_muc, world_muc, dungeon_muc, team_muc
		{service, <<"muc">>},
		{host, <<"localhost">>},
		%% options
		{options, [
			{persistent, <<"true">>},
			{allow_subscription, <<"true">>}
		]}
	]);

get_request_body(?EJ_REMOVE_MUC, {Type, Key}) ->
	RoomName = utils:to_bin(utils:to_list(Type) ++ "_" ++ integer_to_list(Key)),
	utils:encode([
		{name, RoomName},
		%% @todo 是否需要按照功能类型区分 eg. guild_muc, world_muc, dungeon_muc, team_muc
		{service, <<"muc">>}
	]).

