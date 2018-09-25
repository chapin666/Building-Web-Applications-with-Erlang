### 3. Appmods：Yaws的动态内容

在开发Web服务时，开发人员有时不希望将Web浏览器发送的URL映射到实际文件。这可能是因为所有数据都存储在数据库中或动态生成。在这种情况下，您希望浏览器转到某个URL并让服务器创建内容并将其返回。

Appmod是雅虎的Apache模块 —— 一种为Web服务器中URL创建自定义处理程序的方法。当使用appmod时，浏览器将发送一个标准URL，但不会将静态文件发送回用户，Yaws将执行一些Erlang代码并将响应发送回用户。这允许Yaws Web服务器中的完全动态内容。

不同之处在于，在appmod的情况下，out/1将需要创建响应的完整内容，而在.yaws文件中，它可能只是其中的一小部分。在许多情况下，响应不是HTML，而是其他格式，例如XML(XML)，JSON(JSON)，甚至是音频文件，CSV文本文件或PDF。

此外，通过使用appmod，您可以中断节点的本地磁盘上的文件与显示的URL表示之间的关联。因此，可以提供像/blog-posts/2011-Dec-02/why-you-should-use-erlang.html 这样的URL，而不会在该路径上实际存在文件（甚至是目录结构） - 只是一些知道如何将该URL映射到可以构造的某个逻辑资源的代码。使用Apache中的mod_rewrite或其他Apache模块可以实现类似的效果。

在二进制格式的情况下，然后，Erlang应该创建数据并使用正确的标头返回它。如何创建二进制文件的细节取决于数据的格式，超出了本书的范围。但是，在许多情况下会有Erlang模块来创建数据;在某些情况下，数据将流式传输到客户端。

如果在Web服务器的根目录下设置了appmod，那么请求http://example-server.com/chatroom/example-room.json 将 Arg#arg.appmoddata字段设置为/chatroom/example-room.js，这是appmod要处理的路径的一部分。

## Appmod配置

要设置appmod，请将appmods字段添加到yaws.conf中的<server>处。 其示例的一般格式如例3-1所示。

appmods配置的基本形式是<path, module>。 路径可以是Web服务器上的任何路径。 如果您想要appmod，从Web服务器提供所有内容，请将路径设置为“/”，这将把所有请求路由到appmod。 但是，即使在这种情况下，您可能希望将某些目录作为静态文件（例如，images目录），因此可以使用exclude_paths指令添加要排除的路径列表，如示例中所示3-1。
```
Example 3-1. Appmod config options

appmods = <path, module exclude_paths icons css js>
```

在这种情况下，任何名称不在icons，css或js目录中的路径都将路由到功能模块：out/1。 这里的out/1功能与Yaws中的Dynamic Content相同。

## 当URI与文件不对应时

对于.yaws文件，发送到服务器的HTTP路径将直接映射到文件。用户将发出类似“/get-stock-price.yaws”的请求，Yaws将调用get-stock-price.yaws文件中的代码。

但是，在appmod中，程序员必须直接将请求URI转换为某些操作。这会将一些工作负载从Web服务器传输到开发人员，但这并不是很难。

为此，我们需要在处理函数中找出用户请求的URI。这可以在#arg记录中的几个位置之一中设置。它将在pathinfo或fullpath中（实际上都是）。

通常，路径信息将是由斜杠分隔的一组字符串，因此对/record.yaws/show/3141/5926的请求将pathinfo设置为显示/3141/5926。应使用re:split/3拆分此字符串，然后用于显示正确的数据。

可以使用re:split/2或string:tokens/2将此路径拆分为单个令牌。如例3-2所示，这两个文件都将使用字符串和标记或正则表达式来拆分字符串并返回列表。但是，有一点点差异。 re:split / 2函数将返回二进制文件列表，并将空字符串保留在列表的开头。另一方面，string:tokens / 2将返回一个字符串列表，并且不包含初始的空白元素。
```
Example 3-2. Splitting a string

1> T = "/show/3141/5926".
"/show/3141/5926"
2> re:split(T, "/").
[<<>>,<<"show">>,<<"3141">>,<<"5926">>]
3> string:tokens(T, "/").
["show","3141","5926"]
```
在例3-3中，path/1函数从目录分隔符（“/”）上的arg记录中拆分appmoddata路径，然后使用case语句将路径与各种选项相匹配，这些选项将提供正确的处理程序，具体取决于详细信息 什么通过。 模式可以匹配特定字符串或具有分配给变量的数组元素。 模式将从上到下匹配，直到一个匹配或没有模式匹配，这将使进程崩溃。 模式匹配的完整描述超出了本书的范围，但这个概念对于Erlang编程非常重要。

通过将术语[{return，list}，trim]添加到re:split/3函数，它将删除任何空元素，并将结果作为字符串列表返回，而不是以二进制格式。
```
Example 3-3. Path

path(Path) ->
    Elements = re:split(Path,"/", [{return, list}, trim]),
    case Elements of 
        ["ChatRooms", Room] ->
            act_on_room(Room);
        [Directory, File] ->
            show_file(Directory, File);
        [Directory] ->
            show_directory(Directory)
    end.
```

### Cookies

当万维网在20世纪90年代首次创建时，每个HTTP请求都是独立的，Web请求无法维护任何形式的状态。因此，例如，Web服务器没有简单的方法来跟踪用户购物车中的项目。

Netscape在早期版本的Navigator中引入了cookie，现在它们已成为所有现代浏览器的标准配置。可以由浏览器或服务器设置cookie，并且一旦设置将在所有HTTP请求的标头中发送到该服务器，直到它过期或被删除。雅司当然可以设置cookie并访问它们。

通常，cookie用于跟踪用户在网站上的活动。这可以包括身份验证和状态。例如，如果网站实现了购物车，则可以通过cookie跟踪用户的当前项目。通常最好不要将项目本身放在cookie中，而是放置一个可以引用包含购物车信息的数据库记录的散列。这将大大减少应用程序使用的带宽。

HTTP cookie是一种持久性数据形式，可以由浏览器或服务器设置，并伴随发送到服务器的每个请求。要设置cookie，请使用yaws_api:setcookie/2，它将获取cookie名称和值。如果你想设置cookie的选项，请查看yaws_api手册页，其中包含yaws_api:setcookie/n的版本，它们采用额外的参数来指定一堆其他选项。

> Note: 不要将此处讨论的HTTP cookie与Erlang用于在连接节点时提供安全性的cookie混淆。

你也可以通过out/1返回{set_cookie，Cookie}来设置cookie，因为cookie是HTTP头的一部分。

要获取现有cookie，请查看Arg记录的标题记录。 函数yaws_api:find_cookie_val/2可以从cookie列表中提取cookie的值，如例3-4所示。 如果未设置cookie，则此函数将返回空HTML。
```
Example 3-4. Cookies

<erl>
out(Arg) ->
    Headers = Arg#arg.headers,
    Cookie  = Headers#headers.cookie,
    Prefs =  yaws_api:find_cookie_val("Prefs", Cookie),
    {html, Prefs}.
</erl>
```

Yaws还包括一组用于使用cookie创建会话跟踪的接口（请参阅session处理）。

## Session

Yaws为使用yaws_api:new_cookie_session/1-3函数的cookie处理会话提供了一个很好的API。基本函数new_cookie_session/1采用可由应用程序指定的状态记录。该记录可以通过函数yaws_api:cookieval_to_opaque/1来检索。

要更新会话数据，请使用函数yaws_api:replace_cookie_session/2以及cookie的名称和新的状态值。

除了new_cookie_session/1函数之外，还有一个new_cookie_session/2，它接受超时（TTL）值，之后cookie会话将被清除。在new_cookie_session/1中，会话将在默认时间段后超时。

如果需要在会话结束后进行某种形式的清理，请使用new_cookie_session/3函数。除了状态变量和TTL之外，该函数还为清理过程采用PID。当会话结束时，它将向该进程发送{yaws_session_end，Reason，Cookie，Opaque}形式的消息。原因可能是超时或正常。

要删除会话，请使用delete_cookie_session/1函数，该函数将删除cookie并在需要时发送清除消息。

在示例3-5中，取自Yaws源，存在会话处理的示例。与PHP处理$ _SESSION结构的方式类似，Yaws实际上并不将记录保存到HTTP cookie，而是存储nonode@nohost-5560960749617266689形式的密钥，并将cookie存储在服务器上。通常情况下，cookie和数据将存储在Yaws进程中;但是，您可以将其设置为存储在Mnesia或ETS数据存储中。 http://yaws.hyber.org/pcookie.yaws上有这方面的例子。

```
Example 3-5. Cookie session handling (session.erl)

-record(myopaque, {udata,
                   times = 0,
                   foobar}).

out(A) ->
    H = A#arg.headers,
    C = H#headers.cookie,
    case yaws_api:find_cookie_val("baz", C) of
        [] ->
            M = #myopaque{},
            Cookie = yaws_api:new_cookie_session(M),
            Data = {ehtml,
                    {html,[],
                     ["I just set your cookie to ", Cookie, "Click ",
                      {a, [{href,"session1.yaws"}], " here "},
                      "to revisit"]}},
            CO = yaws_api:setcookie("baz",Cookie,"/"),
            [Data, CO];
        Cookie ->
            case yaws_api:cookieval_to_opaque(Cookie) of
                {ok, OP} ->
                    OP2 = OP#myopaque{times = OP#myopaque.times + 1},
                    yaws_api:replace_cookie_session(Cookie, OP2),
                    Data = {ehtml,
                            {html,[],
                             [
                              "Click ",
                              {a, [{href,"session1.yaws"}], " here "},
                              "to revisit",
                              {p, [], f("You have been here ~p times", 
                                        [OP2#myopaque.times])},
                              {p, [], f("Your cookie is ~s", [Cookie])}]}},
                    Data;
                {error, no_session} ->
                    new_session()
            end
    end.

new_session() ->
    M = #myopaque{},
    Cookie = yaws_api:new_cookie_session(M),
    
    Data = {ehtml,
            {html,[],
             ["I just set your cookie to ", Cookie, "Click ",
              {a, [{href,"session1.yaws"}], " here "},
              
              "to revisit"]}},
    CO = yaws_api:setcookie("baz",Cookie,"/"),
    [Data, CO].
```

## 访问控制

有时您可能希望限制对资源的访问，例如对输入密码或可以进行身份​​验证的用户（如在Facebook应用程序中）。在许多情况下，您可能希望执行诸如检查Mnesia数据库或其他数据存储的用户名和密码或会话令牌之类的操作。

理想情况下，您将针对某些数据源（例如Mnesia表）验证用户名和密码。在例3-6中，我使用了validate_username_password/1函数，它从请求中提取用户名和密码，并根据Mnesia表进行检查。如果用户验证正确，此函数将返回{true，Uuid}，或{false，Reason}。在这种情况下，在没有该名称的用户或bad_password的情况下，Reason可以是no_user。清楚地分享登录被拒绝的原因是个坏主意。

out/2函数获取validate_username_password/1的结果，如果用户未进行身份验证或HTML页面，则返回{status，401}。它还记录登录日志。

```
Example 3-6. Custom access control (access-control.erl)

-module('access-control').
-include("../roulette/yaws_api.hrl").
-export([out/1]).
-record(user,
	{
	  uuid,
	  username,
	  passwordMD5
	}).

validate_username_password(Arg) ->
    Username = yaws_api:queryvar(Arg, "Username"),
    Password = yaws_api:queryvar(Arg, "Password"),
    PasswordMD5 = crypto:md5(Password),
    Query = fun () ->
		    mnesia:read({username, Username})
	    end,
    Value = mnesia:transaction(Query),
    case Value of
	{atomic, []} ->
	    {false, no_user};
	{atomic, [UserRecord]}
	  when UserRecord#user.passwordMD5 =:= PasswordMD5 ->
	    {true, UserRecord#user.uuid};
	 {atomic, [_UserRecord]} ->
	    {false, bad_password}
    end.

out({false, Reason}, _Arg) ->
    io:format("~p:~p Unable to login user: ~p", [?MODULE, ?LINE, Reason]),
    {status, 401};
out({true, Uuid}, _Arg) ->
    io:format("~p:~p Welcome: ~p", [?MODULE, ?LINE, Uuid]),
    {html, "Hello World"}.
    
out(Arg) ->    
    out(validate_username_password(Arg), Arg).
```
## 与Erlang服务和业务逻辑层交互

在许多情况下，Web服务将是围绕由OTP服务器组成的更复杂的中间层的简单包装，可以通过Erlang的标准消息传递方法进行通信。

要做到这一点，Yaws必须与其他Erlang节点集群，并且必须知道要与之通信的进程的PID。 例3-7显示了如何使这项工作。

```
Example 3-7. Interacting with middleware

out(Arg) ->
	BackendPid ! {webrequest, node(), Arg}
	receive
		{response, Data} ->
			Data;
		{error, ErrorMsg} ->
			ErrorMsg
	after 500 ->
		[
		 {status, 404},
		 {html, "<h2>System timed out</h2>"}]
	end.
```

这里有几点需要注意。首先，发送消息永远不会失败，但不保证交付。因此，如果进程由于某种原因而消失，则发送不会返回任何类型的错误。因此，示例3-7中的代码必须超时，以便让用户知道出现了问题。在这种情况下，它是after子句，它将等待500ms然后返回超时错误。

处理此问题的更好方法是将中间件包装在gen_server中，并使用OTP框架创建许多自定义服务器来运行应用程序。这在第9章中完成。在这种情况下，每个模块将导出一组可以调用的访问函数，并将使用OTP gen_server:call/2或gen_server:cast/2函数来访问服务器基础结构。 gen_server的实现负责所有的消息传递！从不明确使用运算符。

比较例3-7和例3-8。在前者中，所有非功能部件都被gen_server隐藏：call/2，经过充分测试并且可以假设正常工作。在后者中，我们的out/1函数不需要知道它正在调用的层的操作;它只调用get_data/1函数，该函数用作某种形式的后端服务的接口

```
Example 3-8. Interacting with a service via OTP

get_data(Req) ->
    {response, Data} = gen_server:call(?MODULE, Req),
    Data.
```