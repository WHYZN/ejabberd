%%%-------------------------------------------------------------------
%%% @author 10892
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. 4月 2023 9:30
%%%-------------------------------------------------------------------
-module(mod_register).
-author("10892").

-include("../include/command.hrl").
%% API


-export([admin_register/2, register/1, register_result/2]).
-export([sync_action/2, action/2, action_result/3]).


-spec sync_action(Type::atom(), Args :: tuple()) -> ok | error.
%% @doc 同步执行 action
sync_action(Action, Args) ->
	{Url, Headers}  = mod_http:get_request_info(Action),
	Body  = get_request_body(Action, Args),
	case catch httpc:request(post, {Url, Headers, "application/json", Body}, [], []) of
		{ok, {{_, 200, _}, _Header,  _BodyRes}} ->
			ok;
		{ok, {{_, ErrCode, _}, _Header,  BodyRes}} ->
			io:format("action  ~p args ~p fail Errcode ~p, Messsage ~p ~n",
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
			ok;
		{{_, ErrCode, _}, _Header, Body} ->
			io:format("action  ~p args ~p fail Errcode ~p, Messsage ~p ~n",
				[Action, Args, ErrCode, utils:decode(Body)]),
			error
	end.

%% @doc 通过 web_admin 注册用户
admin_register(Username, Password) ->
	Url = "http://192.168.13.233:5280/admin/server/localhost/users/",
	Method = post,
	RootUsername = "root@localhost",
	RootPassword = "root",
	AuthHeaders = [{"Authorization", "Basic " ++ base64:encode_to_string(RootUsername ++ ":" ++ RootPassword)}],
	Headers1 = [{"Content-Type", "application/json"}],
	FullHeaders1 = Headers1 ++ AuthHeaders,
	Body = mochiweb_util:urlencode([
		{addnewuser, 1},
		{newusername, Username},
		{newuserpassword, Password}
	]),
	io:format("body ~ts ~n", [Body]),
	{ok, _Res1} = httpc:request(Method, {Url, FullHeaders1, "application/x-www-form-urlencoded", Body}, [], []),
	_Res1.

%% @doc 异步 通过 mod_http_api 注册用户
register({PKey, _Password} = Args) ->
	{Url, Headers}  = mod_http:get_request_info(?EJ_CREATE_USER),
	%% @todo Host是否要按照游戏服务器区分
	Body = get_request_body(?EJ_CREATE_USER, Args),
	io:format("body ~ts ~n", [Body]),
	Options = [{sync, false}, {receiver, {?MODULE, register_result, [PKey]}}],
	httpc:request(post, {Url, Headers, "application/json", Body}, [], Options).

%% @doc 收到注册的结果
register_result({_Ref, ReplyInfo}, PKey) ->
	io:format("ReplyInfo ~p ~n", [ReplyInfo]),
	case ReplyInfo of
		{{_, 200, _}, _, _} -> %% 创建用户 成功
			io:format("register Pkey ~p success ~n", [PKey]),
			ok;
		{{_, ErrCode, _}, _Header, Body} ->
			{BodyL} = utils:decode(Body),
			io:format("register Pkey ~p fail Errcode ~p, Messsage ~p ~n",
				[PKey, ErrCode, proplists:get_value(<<"message">>, BodyL)]),
			error
	end.
%% @doc 按照action 和 args  生成 body 参照ejabberd command 命令参数填充
get_request_body(?EJ_CREATE_USER, {PKey, Password}) ->
	utils:encode([
		{host, <<"localhost">>},
		{user, integer_to_binary(PKey)},
		{password, Password}
	]).

