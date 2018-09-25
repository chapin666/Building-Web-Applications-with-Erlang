## 5. 文件上传

虽然能够通过Ajax提交表单或其他帖子变量是有用的，但大多数应用程序迟早会希望让用户上传文件，例如头像图像或视频。

Yaws允许应用程序以标准上载格式从用户接收文件，这是PHP等其他服务器端技术的用户所期望的。

在PHP中上传文件时，PHP将输入缓冲到/tmp目录中，然后将完成的文件发送到您的程序。在Yaws中，服务器将文件以块的形式发送到您的代码中。因此，程序员必须准备好获取大量数据流，而不是获得完整的文件。这确实给程序员带来了更多的工作，但它也允许程序员在需要时使用不完整的上传。

本章中的示例取自Yaws文档网站上的示例，并进行了一些小的更改。完整的工作代码在例5-4中，而前面的例子显示了部分代码以供解释。

> Note: 要测试上传，您当然可以使用浏览器，但是从命令行使用curl程序可以使一切更容易测试。 使用这样的命令行会将文件从本地磁盘上传到Yaws服务器。 在此示例中，您指定的文件应复制到上传目录/tmp/YawsUploads - 如果该目录不存在，则将创建该目录。 上传完成后，Web服务器将返回带有文件“File Upload Done”的HTML片段。
curl -F radio=@large_audio_file.mp3 http://localhost:8081/upload.yaws

## 文件上传请求

在文件上传的情况下，out/1将被调用不仅一次而是多次，每次调用都有一个新的数据切片（参见例5-1）。 为了维持上传的状态（以及可能与之一起发生的任何其他事情），Yaws提供了一种从out/1返回状态并在下次调用时将其返回给您的方法。 在此示例中，上载状态在#upload{}记录中编码，并存储在Arg#arg.state中的调用之间。

此函数中的第一个子句使用guard来检查是否未设置Arg#arg.state字段。 如果尚未设置，则会创建一个空白的上传对象并将其传递给multipart/2。 函数的第二个子句只是从Arg＃arg.state中获取现有的状态对象，并将其传递给multipart/2。

```

Example 5-1. Repeated upload requests

<erl>
multipart(Arg, State) ->
    Parse = yaws_api:parse_multipart_post(Arg),
    case Parse of
        [] -> ok;
        {cont, Content, Res} ->
            case nextChunk(Arg, Res, State) of
                {done, Result} ->
                    Result;
                {cont, NewState} ->
                    {get_more, Content, NewState}
            end;
        {result, Res} ->
            case nextChunk(Arg, Res, State#upload{last=true}) of
                {done, Result} ->
                    Result;
                {cont, _} ->
                    err()
            end
    end.

out(A) when A#arg.state == undefined ->
    State = #upload{},
    multipart(A, State);
out(A) ->
    multipart(A, A#arg.state).
</erl>

```

如果这是来自浏览器的最后一块数据，函数yaws_api:parse_multipart_post/1将返回{result，Res}。 但是，如果函数返回{cont，Contents，Res}，则会有更多数据来自浏览器。 此时out/1函数应该返回{get_more，Contents，State}。 下一次调用out / 1时，该元组的State部分将被传回以供使用，如例5-2所示。

上传完成后，multipart/2将返回将显示给用户的{html，“Upload Finished”}等结果。 如果上传没有完成，它将返回如上所述的元组，让Yaws知道为它提供更多数据。 请注意，此示例不会保存将保存到磁盘中显示的数据。

```
Example 5-2. Multipart

multipart(Arg, State) ->
    Parse = yaws_api:parse_multipart_post(Arg),
    case Parse of
        [] -> ok;
        {cont, Content, Res} ->
            case nextChunk(Arg, Res, State) of
                {done, Result} ->
                    Result;
                {cont, NewState} ->
                    {get_more, Content, NewState}
            end;
        {result, Res} ->
            case nextChunk(Arg, Res, State#upload{last=true}) of
                {done, Result} ->
                    Result;
                {cont, _} ->
                    err()
            end
    end.
```

### 保存到磁盘

文件上传最明显的事情是将其保存到文件系统中。 这可能是文件的最终位置，或者是一种缓冲大型上载的方式，直到它们完成，并且可以推送到其他存储机制中，以免耗尽大量内存。

要将文件写入磁盘，请调用BIF的file:open/2，如例5-3所示。 如果没有错误，则返回{ok，FD}，其中FD是可用于写入文件的文件句柄。 有关在Erlang中处理文件的完整详细信息，请参阅Erlang手册页。

打开文件后，可以使用file:write/2将每个后续数据块添加到文件中，直到文件结束，然后可以调用file:close/2来关闭文件句柄。

> Note: 如果进程在写入文件的过程中死亡，Erlang将自动关闭文件句柄。 让监控机制删除文件也许是值得的。

例5-3中的writeToDisk/3函数有几个子句，但它们都采用相同的三个参数。第一个是Yaws发送到 out/1 的标准Arg记录，在此处传递。第二个是要保存的文件的部分列表，第三个是当前状态记录。

部件缓冲区是可以保存到磁盘的上载文件的块列表。如果列表为空且State#upload.last为false，则已处理已缓冲的所有数据。在这种情况下，writeToDisk/3将返回{cont，State}，这将让Yaws知道在它到达时发送下一个数据块并等到发生这种情况。

当缓冲区不为空时，它将包含{Atom，Data}形式的元组列表。有几种可能的原子可以被发送。

要发送的第一个元素将以{head，{Name，Options}}的形式发送。要处理这个问题，writeToDisk/3应该打开文件句柄，设置状态记录，然后使用新的状态记录和缓冲区列表的尾部递归调用writeToDisk/3。

在文件中间有一大块数据的情况下，缓冲区的头部看起来像{body，Data}。在这种情况下，应该将数据写入磁盘，然后再次使用列表的尾部递归调用writeToDisk/3。

如果缓冲区列表为空且State＃upload.last为true，则文件完成上载。此时我们可以调用file:close / 1来关闭文件句柄。之后我们可以调用upload_callback/1来处理上传完成后我们可能希望处理的任何操作（例如同步到其他节点或上传到CouchDB）并返回完成状态。

```
Example 5-3. Save file upload

writeToDisk(A, [{part_body, Data}|Res], State) ->
    writeToDisk(A, [{body, Data}|Res], State);

writeToDisk(_A, [], State) when State#upload.last==true,
                                 State#upload.filename /= undefined,
                                 State#upload.fd /= undefined ->
    file:close(State#upload.fd),
    upload_callback(State),
    Res= {html, "Done"},
    {done, Res};

writeToDisk(A, [], State) when State#upload.last==true ->
    {done, err()};

writeToDisk(_A, [], State) ->
    {cont, State};


writeToDisk(A, [{head, {_Name, Opts}}|Res], State ) ->
    case lists:keysearch(filename, 1, Opts) of
        {value, {_, Fname0}} ->
            Fname = yaws_api:sanitize_file_name(basename(Fname0)),
	    TargetDir = "/tmp",
	    file:make_dir(TargetDir),
	    case file:open([TargetDir, Fname] ,[write]) of
		{ok, Fd} ->
		    S2 = State#upload{filename = Fname,
				      fd = Fd},
		    writeToDisk(A, Res, S2);
		Err ->
		    {done, err()}
	    end;
	false ->
            writeToDisk(A,Res,State)
    end;


writeToDisk(A, [{body, Data}|Res], State)
  when State#upload.filename /= undefined ->
    case file:write(State#upload.fd, Data) of
        ok ->
            writeToDisk(A, Res, State);
        Err ->
            {done, err()}
    end.
```
如果上传文件是应用程序的很大一部分，那么磁盘可能成为应用程序性能的瓶颈。 虽然服务器可能有20或40个内核，但磁盘是非常顺序的，也是系统中最慢的部分。 这必须根据阿姆达尔定律来考虑（参见Amdahl定律）。 使用像亚马逊S3这样的东西可能是更好的解决方案（请参阅保存到Amazon S3）。

## 把它们（代码）放在一起

例5-4汇集了各种代码，向您展示如何在Yaws中上传文件。

```
Example 5-4. Complete upload code (upload.yaws)

<erl>

-record(upload, {
          fd,
          filename,
          last}).

-define(DIR, "/tmp/").

out(Arg) when Arg#arg.state == undefined ->
    State = #upload{},
    multipart(Arg, State);
out(Arg) ->
    multipart(Arg, Arg#arg.state).

err() ->
    {ehtml,
     {p, [], "error"}}.

multipart(Arg, State) ->
    Parse = yaws_api:parse_multipart_post(Arg),
    case Parse of
        [] -> ok;
        {cont, Cont, Res} ->
            case addFileChunk(Arg, Res, State) of
                {done, Result} ->
                    Result;
                {cont, NewState} ->
                    {get_more, Cont, NewState}
            end;
        {result, Res} ->
            case addFileChunk(Arg, Res, State#upload{last=true}) of
                {done, Result} ->
                    Result;
                {cont, _} ->
                    err()
            end
    end.

addFileChunk(Arg, [{part_body, Data}|Res], State) ->
    addFileChunk(Arg, [{body, Data}|Res], State);

addFileChunk(_Arg, [], State) when State#upload.last	== true,
                                 State#upload.filename	/= undefined,
                                 State#upload.fd	/= undefined ->

    file:close(State#upload.fd),
    Res = {ehtml,
           {p,[], "File upload done"}},
    {done, Res};

addFileChunk(Arg, [], State) when State#upload.last==true ->
    {done, err()};

addFileChunk(_Arg, [], State) ->
    {cont, State};

addFileChunk(Arg, [{head, {_Name, Opts}}|Res], State ) ->
    case lists:keysearch(filename, 1, Opts) of
        {value, {_, Fname0}} ->
            Fname = yaws_api:sanitize_file_name(basename(Fname0)),

            %% we must not put the file in the
            %% docroot, it may execute uploade code if the
            %% file is a .yaws file !!!!!
	    file:make_dir(?DIR),
	    case file:open([?DIR, Fname] ,[write]) of
		{ok, Fd} ->
		    S2 = State#upload{filename = Fname,
				      fd = Fd},
		    addFileChunk(Arg, Res, S2);
		Err ->
		    {done, err()}
	    end;
	false ->
            addFileChunk(Arg,Res,State)
    end;

addFileChunk(Arg, [{body, Data}|Res], State)
  when State#upload.filename /= undefined ->
    case file:write(State#upload.fd, Data) of
        ok ->
            addFileChunk(Arg, Res, State);
        Err ->
            {done, err()}
    end.


basename(FilePath) ->
    case string:rchr(FilePath, $\\) of
        0 ->
            %% probably not a DOS name
            filename:basename(FilePath);
        N ->
            %% probably a DOS name, remove everything after last \
            basename(string:substr(FilePath, N+1))
    end.
</erl>
```

## 存储在分布式系统中

另一个复杂因素是将文件写入磁盘可能不是处理数据的正确方法。 Erlang应用程序是运行在大量服务器上的分布式应用程序。因此，如果您从用户上传文件并将其放在一个节点上，则所有其他节点都不会看到该文件。在这种情况下，将文件保存在某种数据存储中是一个更好的想法，这种数据存储可以在网络中复制数据。

一种解决方案是尝试将文件放在共享文件系统上。但是，除非它是像亚马逊S3那样的系统，否则出于某些原因这可能是一个坏主意。首先，拥有该系统的服务器将成为瓶颈和单点故障。如果该系统脱机，整个系统将变得不可用。此外，这样的系统必须非常大才能处理所有客户端的负载。再一次，必须根据应用程序的设计和使用来评估存储的细节。

使用像CouchDB这样的东西（参见CouchDB）在这里是有意义的，因为它将允许文件很好地在应用程序的节点周围传播。在这种情况下，可能会发生的情况是文件将被上传到本地磁盘，然后，当上载完成后，它将被移动到分布式系统，即CouchDB，Riak，HBase或其他东西。这样，如果文件上载被取消或损坏，它将不会传播到网络上。

处理上传数据的另一个选择是根本不写出来，而是将其流式传输给用户。 Yaws完全能够流式传输多媒体 - 请参阅Yaws文档以获取更多详细信息。

## 保存到Amazon S3

通常，我们会想要获取用户上传的文件，并将其提供给全世界下载。例如，想想YouTube上的视频：用户上传文件，可能会对数据本身进行一些操作（转换格式等），然后将其放在其他用户可以查看的位置。

最简单的方法之一是将文件保存到Amazon S3，这是一种高度可靠的云服务，旨在解决此特定问题。

要从Erlang使用Amazon Web Services（AWS），请使用https://github.com/gleber/erlcloud上的erlcloud软件包。该软件包为AWS提供了一个Erlang接口。在这种情况下，我们只对S3服务感兴趣。

在Amazon S3中，文件存在于存储桶中，示例5-5中的代码假定我们已经创建了存储桶。存储桶的名称应由文件中的-define()设置。也可以在存储桶上设置默认选项，以便它们满足您的应用程序所需。此外，必须在此文件中设置两个密钥（将它们设置为您的AWS密钥）。

一旦从用户上传文件，我们需要使用函数erlcloud_s3:put_object/6将其上传到S3（如果你想允许默认选项，还有函数erlcloud_s3:put_object/3-5）。将此函数传递给桶名称，密钥和要上载的值;您还可以传递选项，HTTP标头和配置对象。这会将对象上传到Amazon S3。

设置完所有内容后，我们就可以将文件上传到S3。为此，我们将键和值传递给函数s3：upload()，它将调用erlcloud_s3:put_object/3来上传文件。

如果文件在磁盘上，请使用函数s3:upload_file / 2，它将自动将文件读入内存并将其传递给upload/2。

```
Example 5-5. Uploading to S3 (s3.erl)

-module(s3).

-define('ACCESS_KEY',           "********************").
-define('SECRET_ACCESS_KEY',    "****************************************").
-define('BUCKET',               "*************"). 

-export([upload/2, upload_file/2]).

upload_file(Key, Path) ->    
    {ok, Binary} = file:read_file(Path),
    upload(Key, Binary).

    
upload(Key, Value) ->
    erlcloud_ec2:configure(?ACCESS_KEY, ?SECRET_ACCESS_KEY),
    error_logger:info_msg("~p:~p Settng up AWS to S3 ~n", 
                          [?MODULE, ?LINE]),
    R = erlcloud_s3:put_object(?BUCKET, Key, Value, [], [{"Content-type", "image/jpeg"}]),
    error_logger:info_msg("~p:~p Uploaded File ~p to S3 ~n", 
                          [?MODULE, ?LINE, R]),
    {ok, R}.
```

显然，在使用此示例之前，您需要填写访问密钥和密钥以及存储桶的名称。 此外，在尝试将代码上传到S3之前，您需要在Erlang中启动inets和ssl服务。 为此，在启动Erlang节点时运行以下两行：
```
      inets:start().
      ssl:start().
```
要查看此操作，您可以运行示例5-6，它将从Unix命令行获取密钥和文件并将其上传到S3。 有更好的AWS命令行工具，但这是测试示例5-5中的代码的有用方法。

```
Example 5-6. Uploading to S3 shell wrapper (s3_upload)

#!/usr/bin/env escript
-export([main/1]).

main([Key, File_Name]) ->
    inets:start(),
    ssl:start(),
    s3:upload_file(Key, File_Name).
```