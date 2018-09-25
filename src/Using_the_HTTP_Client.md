## 8. 使用HTTP客户端

有时您不需要创建HTTP服务，而是使用一个 - 也许是使用RESTful服务或其他API，或者通过一次性使用大量请求来加载测试服务器。 Erlang提供了一个HTTP客户端API，可以让您这样做。您可以在http://www.erlang.org/doc/man/httpc.html找到该模块的手册页。 httpc模块是标准Erlang发行版的一部分，不必单独安装。

在使用httpc模块之前要做的第一件事是通过调用inets来启动服务 inets:start() 或application:start(inets)。如果您正在运行应用程序，可以从.erlang文件或测试中的命令行完成。如果你没有启动inets，httpc将无法正常工作。

如果必须为所有调用设置一些选项，则可以使用函数httpc:set_options/1或httpc：set_options/2。这里可以设置许多选项，包括您期望的所有标准选项。特别值得注意的是max_sessions选项，默认为2.此外，如果需要设置代理服务器，可以使用Proxy选项在此处执行此操作。调用set_option时，它将返回ok或{error，Reason}。

> Note: Erlang还有其他几个HTTP客户端软件包，它们提供了更多功能，包括ibrowse和lhttpc。 你可以在网上找到。

## 发送请求

有四个函数可用于生成从一到五个参数运行的HTTP请求（没有三个函数）。 这些提供了对HTTP请求的逐步控制。

最简单的HTTP请求版本是httpc:request/1，它将URL作为参数。 request/1函数只是对提供的URL执行HTTP GET操作，如例8-1所示。 在这种情况下，对http://www.google.com的请求会返回{ok，Response}或{error，Reason}。 响应将是HTTP请求的标头以及请求的主体（在例8-1中截断）。 如果您希望从成功请求中提取值，可以使用此行提取变量：{ok，{{Version，200，ReasonPhrase}，Headers，Body}}。

```
Example 8-1. A simple HTTP request

14> httpc:request("http://www.google.com").
{ok,{{"HTTP/1.1",200,"OK"},
     [{"cache-control","private, max-age=0"},
      {"date","Tue, 24 Apr 2012 17:59:10 GMT"},
      {"server","gws"},
      {"content-length","40887"},
      {"content-type","text/html; charset=windows-1255"},
      {"expires","-1"},
      {"x-xss-protection","1; mode=block"},
      {"x-frame-options","SAMEORIGIN"}],
     "<!doctype html>..."}}
```

有时简单的GET控制不够 - 例如，您希望访问REST服务，您可能需要使用POST，PUT或DELETE发送数据，或者测试是否存在具有HEAD请求的资源。

要更好地控制请求，请使用request/4或request/5。 这里的第一个参数是设置为原子的HTTP动词。 接下来将是请求的内容，其次是HTTP选项，常规选项，最后是配置文件参数。 （有关选项的完整列表，请参见手册页。）

要将数据发布到服务，请使用函数的request/4版本，如例8-2所示。 在这种情况下，我们将一个简单的数据有效负载发送到服务器，可以是URL编码或JSON。 在此示例中，有效负载是要发送到服务器的数据，URL是要将其发送到的资源的地址。

```
Example 8-2. HTTP post

-module(post).
-export([post/3]).

post(url_encoding, URL, Payload) ->
    httpc:request(post, {URL,
			 [],
			 "application/x-www-form-urlencoded",
			 Payload},
		  [],
		  []);
post(json, URL, Payload) ->
    httpc:request(post, {URL,
			 [],
			 "application/json",
			 Payload},
		  [],
		  []).
```

如果您不希望由于某种原因让您的进程等待HTTP请求，您可以将请求包装好并使用spawn/1在其自己的进程中运行它。 但是，如果作为选项传递[{sync，false}]，http:request/4函数将为您执行此操作。 在这种情况下，请求将立即返回，您将获得接收块中的内容。 该过程将发送消息{http，{RequestId，Result}}。 在程序必须轮询多个服务器以获取某些信息并整理结果的情况下，这将特别有用。 如果您习惯在JavaScript中使用Ajax，那将会很熟悉。

```
-module('async_request').
-export([async_request/1]).
async_request(URL) ->
    {ok, RequestId} =
	httpc:request(get, {URL, []}, [], [{sync, false}]),
    receive 
	{http, {RequestId, Result}} -> 
	    Result 
    after 500 -> 
	    error 
    end.
```

最后，如果HTTP请求将返回大量数据，则将其写入磁盘以进行进一步处理可能很有用。 为此，您可以使用选项{stream，Filename}，如例8-3所示。 在这种情况下，request/4函数将返回{ok，saved_to_file}或{error，Reason}，具体取决于发生的情况。 也可以通过传递self或{self，once}而不是文件名来将数据流式传输到进程。 有关其工作原理的更多详细信息，请查看http://erlang.org上的httpc手册页。

```
Example 8-3. Save to a file (stream_to.erl)

-module('stream_to').
-export([stream_to/2]).


stream_to(URL, Filename) ->
        httpc:request(get, 
		      {URL,[]},
		      [],
		      [{stream, Filename}]
		      ).

```

## 使用OAuth

许多网站现在使用OAuth提供身份服务。 OAuth是一种允许用户从外部资源（例如Google或Facebook）进行身份验证的协议。 要使用OAuth，程序只需要知道来自服务器和httpc或其他Web客户端的令牌。

OAuth的工作方式是您的网站将用户重定向到OAuth提供商提供的网页，然后该网站会提示用户批准您的网站使用OAuth。 假设用户授权访问，将使用令牌将用户重定向回您的站点。 如果您随后使用该令牌向提供站点发出HTTP请求，它将使用提供用户信息（包括其名称）的JSON进行响应。 参见例8-4。

```
Example 8-4. Using OAuth (oauth.erl)

-module(oauth).

-export([auth/2]).


auth(Site, OAuthToken) ->
    URL = lists:flatten(io_lib:format("~s~s", [Site, OAuthToken])),
    io:format("~n~p:~p (~p)~n OAuth URL ~p~n", [?MODULE, ?LINE, self(), URL]), 
    {ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} = httpc:request(URL),
    {ok, JSON,_} = rfc4627:decode(Body),
    io:format("~n~p:~p (~p)~n JSON: ~p~n", [?MODULE, ?LINE, self(), JSON]), 
    JSON.
```
对于Facebook Canvas（参见下一节），一旦用户获得授权，那么当Facebook加载您的Canvas页面时，它将向页面发送一个POST，其中包含一个包含OAuth令牌的JSON。 （完整的详细信息在Facebook的开发者网站上。）获得该令牌后，您可以向Facebook服务器发送https请求，该服务器将返回如示例8-9中的JSON。 （请注意，此JSON已使用JSON Lint重新格式化，并且已删除个人信息。）

### Facebook Canvas

如果您正在构建Facebook应用程序，可以通过Canvas与Facebook进行交互的一种方式。当Facebook将其作为Canvas打开应用程序时，它将在Facebook的页面内的iframe中打开。在该页面内，您的应用程序可以与您的服务器通信，并执行您希望它执行的任何其他操作。您还可以通过其界面与Facebook通信。

当Facebook打开Canvas页面时，它会向您发送一个POST，其中包含一个签名请求，该请求是base64编码的JSON，允许您对用户进行身份验证。

要使用此数据，请从Facebook获取发布数据的signed_request字段，并将其拆分为句点。第一部分是使用密钥验证的签名，第二部分是允许您验证用户的数据。

如果用户未获得您的应用程序授权，您将获得类似于例8-5的JSON。在这种情况下，您应该将用户重定向到Facebook身份验证对话框。 （有关详细信息，请参阅Facebook的文档：https：//developers.facebook.com/docs/authentication/canvas/。）

此时，您需要将用户重定向到Facebook授权页面。

```
Example 8-5. Initial JSON

{
    "algorithm": "HMAC-SHA256",
    "issued_at": 1335672795,
    "user": {
        "country": "il",
        "locale": "en_US",
        "age": {
            "min": 21
        }
    }
}
```

要实现这一点，请使用示例8-6之类的页面，该页面生成基本HTML页面，然后调用示例8-7中的代码来解压缩请求并将JSON发送到任何包含的JavaScript。 （您可能还希望将数据保存在会话cookie中。）

```
Example 8-6. Facebook interface Yaws file (facebook.yaws)

<!DOCTYPE html>
<html>
  <head>
    <meta   http-equiv	="Content-Type" 
	    content	="text/html; charset=UTF-8">  
    <title>Canvas</title>
  </head>
  <body>
    <pre>
      <erl>
out(Arg) ->    
    {ok, SignedRequest} = postvar(Arg, "signed_request"),    
    ParsedRequest	= facebook:parse_signed_request(SignedRequest),
    facebook:response(facebook:user_has_authorized(ParsedRequest)).
      </erl>
    </pre>
  </body>
</html>
```

例8-7中的代码将实现Facebook界面的基础知识。 它可以解码来自Facebook的请求并与OAuth服务器进行交互。

```
Example 8-7. Facebook interface (facebook.erl)

-module(facebook).


-export([parse_signed_request/1,
         user_has_authorized/1, 
         make_redirect_script/0, 
         get_user_id/1,
         get_user_info/1,
         response/1]).

-define(SECRET,            "********************************").
-define(APP_ID,            "***************").
-define(APP_NAMESPACE,     "*************").


parse_signed_request(SignedRequest) ->
    [_EncodingSig, Payload]     = string:tokens(SignedRequest, "."),    
    PayloadJson                 = tt:fb_decode_base64(Payload),
    {ok, JSON, _}               = rfc4627:decode(PayloadJson),
    JSON.

user_has_authorized(ParsedRequest) ->
    rfc4627:get_field(ParsedRequest, "oauth_token", undefined).

get_user_id(ParsedRequest) ->
    rfc4627:get_field(ParsedRequest, "user_id", undefined).

make_user_redirect_url()->
    URLPatern           = 
	"https://www.facebook.com/dialog/oauth/?client_id=~s&redirect_uri=~s&scope=~s",
    RedirectURL         = lists:flatten(io_lib:format( "https://apps.facebook.com/~s",
						       [?APP_NAMESPACE])),
    Permission_Names    = string:join(["user_interests", 
				       "user_location", 
				       "user_photos", 
				       "user_hometown", 
				       "email"], 
				      ","),
    URL                 = io_lib:format(URLPatern, 
                                        [?APP_ID,
                                         yaws_api:url_encode(RedirectURL),
                                         Permission_Names]),
    lists:flatten(URL).
                         
    
make_redirect_script() ->
    Url		= make_user_redirect_url(),
    Tag		= "<a href=~p>~p</a>",
    Script	= io_lib:format(Tag, [Url,Url]),
    lists:flatten(Script).

get_user_info(OAuthToken) ->
    URL = lists:flatten("https://graph.facebook.com/me?access_token=" 
			++ binary:bin_to_list(OAuthToken)),
    io:format("~n~p:~p (~p)~n OAuth URL ~p~n", [?MODULE, ?LINE, self(), URL]), 
    {ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} = httpc:request(URL),
    {ok, JSON,_} = rfc4627:decode(Body),
    io:format("~n~p:~p (~p)~n JSON: ~p~n", [?MODULE, ?LINE, self(), Body]), 
    JSON.


response(undefined)->
    {html, facebook:make_redirect_script()};
response(OAuthToken) ->
    UserInfo    = get_user_info(OAuthToken),
    io:format("~n~p:~p (~p)~n JSON: ~p~n", [?MODULE, ?LINE, self(), UserInfo]), 
    JSON        = rfc4627:encode(UserInfo),
    [
     {ehtml,  {script,[], "user_info_data = " ++ JSON}}].
```
一旦用户告诉Facebook他希望允许您的应用知道他是谁，Facebook将使用类似于例8-8的JSON打开您的页面。 在这里，您将注意到有两个新字段。 第一个是oauth_token，它使您能够从Facebook请求用户的详细信息; 第二个是user_id，可用于跟踪会话和本地缓存用户信息。

```
Example 8-8. Authorized JSON

{
    "algorithm": "HMAC-SHA256",
    "expires": 1335679200,
    "issued_at": 1335673105,
    "oauth_token": "AAAB9elehJ9...",
    "user": {
        "country": "il",
        "locale": "en_US",
        "age": {
            "min": 21
        }
    },
    "user_id": "100************"
}
```

如果用户允许应用程序进行身份验证，请参阅示例8-9，了解Facebook从OAuth请求发回的数据。 （并不是说这里的JSON已被重新格式化以使其更具可读性。）

```
Example 8-9. Using OAuth (oauth.json)

{
    "id": "***************",
    "name": "Joshua Levi",
    "first_name": "Joshua",
    "last_name": "Levi",
    "link": "http:\\/\\/www.facebook.com\\/profile.php?id=***************",
    "gender": "male",
    "email": "zkessin\\u0040**********.***",
    "timezone": 3,
    "locale": "en_US",
    "updated_time": "2010-10-17T10:49:04+0000"
}
```