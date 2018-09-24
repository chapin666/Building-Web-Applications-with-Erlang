## <span id="1">前言</span>

Erlang承诺让您比使用Java或C＃更容易构建健壮的容错服务器。这听起来好得令人难以置信，但Erlang已成为程序员的秘密握手。尽管我们很多人都讨厌我们的电话公司，但有一个基本的事实必须得到认可：当你拿起电话拨打电话时，它通常都能正常工作。所以人们已经开始意识到电信人员必须做正确的事！

Erlang是为爱立信的电话交换机编程而设计的，大多数语言设计选择反映了编程电话交换机所需的内容。这意味着，例如，Erlang软件可以一次运行多年而不会中断，因为电话交换机应该这样做。 Erlang应用程序可以在不使系统脱机或甚至丢失状态的情况下进行升级，因为电话公司每次必须修补错误或推出新功能时都不能丢弃城市的呼叫。

当Web服务出现故障时，很多事情都会破裂。它可能不像突然中断的呼叫那么明显，但实际上可能会因故障造成新故障而产生更多问题。 Web服务可以从Erlang的创建者在电话交换环境中做出的语言设计决策中受益。拥有一台可以不间断运行可以让开发团队为其客户提供更好的服务的服务器。

## <span id="2">本书适用于谁</span>

本书向您展示了使用Erlang构建Web服务的详细步骤。它并没有试图教你学习Erlang（还有其他书籍），也没有试图向你展示使用Erlang构建大型应用程序。相反，它向您展示了如何构建简单的Web服务，作为学习构建大型Web服务的一个步骤。

我希望很多读者会像我一样成为长期的网络专业人士，他们将Erlang视为从众多Java和C＃开发人员中脱颖而出的一种方式。毕竟，在几年内，Erlang可能是下一件大事，你想要领先于浪潮。或者您可能对使用其他语言构建Web应用程序的某些方面感到沮丧，并且正在寻找更强大的功能。

您至少需要了解基本的Erlang，但您也应该熟悉Web开发 - 使用PHP，Perl，Ruby，Java或其他东西。这里假设你已经看过HTML并且知道HTTP如何工作的基础知识。

本书中有一些例子使用JavaScript将浏览器与Erlang示例连接起来。 除了[第9章](./Using_the_HTTP_Client.md)之外，这段代码对于理解Erlang代码的作用并不重要，当然如果你正在构建一个大型Web应用程序，它将包含JavaScript。 我也在一些地方使用CoffeeScript。 CoffeeScript是一种可以编译为JavaScript的小语言，通常比直接JavaScript更好的编程体验。

## <span id="3">学习Erlang</span>

这本书不会教你erlang。已经有很多好的资源，包括：

* [Learn You Some Erlang for Great Good](https://learnyousomeerlang.com)。了解你一些Erlang也将于2012年9月由No Starch Press出版。

* Erlang Programming，由Francesco Cesarini和Simon Thompson撰写，由O'Reilly出版。

* Programming Erlang，由Joe Armstrong编写，由The Pragmatic Programmers出版。

阅读其中任何一章的前几章并理解Erlang如何工作的基础应该就足够了。但是，在尝试这里的项目之前，您应该计划真正完成这些章节并编写一些简单的程序。

特别是，您应该阅读顺序代码以及Erlang中并发工作原理的基础知识。在Erlang中构建大型应用程序时，利用开放式电信平台（OTP）将使程序员能够利用大量经过良好测试的功能。虽然OTP功能非常强大，并且在Erlang中的开发变得更加容易，但OTP的细节对于预先学习并不重要，并且在您了解系统的其他部分如何工作之后可以学习。

## <span id="4">在你开始之前</span>

在深入阅读本书之前，您应该在系统上安装Erlang和Yaws。 （如果您需要帮助，请查看附录A.）Erlang和Yaws可以在Windows，Mac和Linux上运行，因此任何类型的系统都可以正常工作。

> Note： 有几个人问我为什么围绕Yaws而不是其他网络软件包写这本书。 有几个原因。 首先，Yaws似乎是最容易实现简单工作的软件包。其次，其他几个软件包不支持Web套接字（或者至少在我开始编写时没有），而且我知道我需要Web 我自己开发的套接字。

我也假设您熟悉Unix命令行。 虽然没有必要成为 Bash Kung-Fu Master（我不是），但你应该能够与bash shell进行交互，而不是吓坏了。

## <span id="5">你会学到什么</span>

构建完整的Erlang应用程序需要大量技能。本书将帮助您实现构建基本Web服务应用程序并使其运行的程度。

首先，您将探索Erlang和REST的一些强大功能和神秘感。你会明白为什么Erlang作为构建可扩展和可靠系统的基础是有意义的，以及为什么REST是一种构建Web服务的流行方法，并探索将这两者结合使用所涉及的一些权衡。第一章还将探讨您的一些数据存储选项。

Yaws Web服务器是我们应用程序的基础，因此您将学习配置Yaws并提供静态内容。是的，静态内容。在许多情况下，具有动态内容的网站将具有静态文件集合作为资源。一旦您知道如何管理静态文件，您就可以继续使用动态内容，将Erlang嵌入到HTML文件或其他类型的文件中（请参阅[Yaws中的动态内容](https://www.safaribooksonline.com/library/view/Building+Web+Applications+with+Erlang/9781449320621/ch02.html#dynamic_content)）。您将学习如何使用HTTP本身以及日志等基本调试工具。

您需要一种方法将呈现为URL的客户端请求路由到服务的内部资源。[第3章](https://www.safaribooksonline.com/library/view/Building+Web+Applications+with+Erlang/9781449320621/ch03.html)中讨论的Appmod将允许您将任意URL映射到相关资源。

接下来我们介绍输出格式。我将展示向用户输出数据的三种常规方法。第一种也是最不实用的方法是使用ehtml将Erlang数据直接转换为HTML或XML。我们还将看到如何使用erlydtl库来使用Django模板语言来创建格式化输出。 （DTL是Python上的常用模板包，对本书的一些读者来说应该很熟悉。）最后，我们将看到如何将Erlang数据结构编码为JSON或XML，然后将其发送给用户。在许多情况下，现代Web应用程序将拥有一个静态（或几乎静态）HTML页面以及许多将通过Ajax通道发送JSON或XML与服务器交互的JavaScript。

现在我们可以生成内容了，是时候构建一个简单的RESTful服务了。您将组装一个可以侦听HTTP请求，处理它们，存储数据和返回有用信息的应用程序。您还将学习如何处理大量传入信息，处理多部分请求和文件上传。

如果您想超越HTTP的请求 - 响应模型，[第6章](https://www.safaribooksonline.com/library/view/Building+Web+Applications+with+Erlang/9781449320621/ch06.html)介绍了客户端和服务器之间的实时双向通信方法。 Yaws支持Web套接字，Erlang的动态，事件驱动特性使其成为将动态数据推送到客户端的理想平台。

最后，[第9章](https://www.safaribooksonline.com/library/view/Building+Web+Applications+with+Erlang/9781449320621/ch09.html)介绍了一个更大的示例，它将大多数或所有前面讨论过的主题整合到一个完整的应用程序中。本章将介绍如何使用Yaws和OTP构建完整的小型应用程序。

## <span id="6">本书的局限</span>

如果您想要使用Erlang构建大型容错站点的完整指南，您会感到失望。大型网站的架构需要一本自己的书。 （像这样的项目最终可能会有90％的后端和逻辑以及10％的网络界面。）

我还特意没有覆盖用于使用Erlang构建Web应用程序的六个左右框架中的任何一个，因为我想专注于使用Yaws和自定义代码在Erlang中构建基本服务的任务。 MochiWeb，Chicago Boss，Nitrogen，Zotonic以及其他人需要他们自己的书籍，但我在[附录B](https://www.safaribooksonline.com/library/view/Building+Web+Applications+with+Erlang/9781449320621/apb.html)中简要总结了它们。

本书并不试图展示如何构建Erlang应用程序超出基础知识：对OTP的完整介绍需要比这一本书更长的书。

它也不是监督树的介绍。第9章简要介绍了它们，但这是对一个非常大的主题的简短介绍。

Erlang具有一整套功能，可以监控应用程序的状态，并在进程或节点脱机时进行响应。这在许多层面都非常强大。例如，在节点在凌晨2:00失败的情况下，Erlang可以生成日志消息并从云创建新节点而无需人工干预 - 这比系统管理员的紧急唤醒呼叫要好得多！

对于自动化测试，Erlang有一个名为EUnit的测试框架（在Erlang编程中有记录）以及Haskell QuickCheck测试套件的版本。这些超出了本书的范围，但对于开发非常有用。

最后，本书未详细介绍如何在Amazon EC2或其他云服务上最佳运行Erlang。在云主机上运行一堆Erlang节点可以很有意义。

## <span id="7">Help! 它不能编译或运行！</span>

使用您可能不太了解的语言使用新框架时，不可避免的是迟早会遇到一些问题。 代码不会编译，否则它会编译然后以各种奇怪的方式崩溃。

如果您和我一样，您可能不会直接从本书中复制/粘贴代码（尽管如果您愿意，欢迎您这样做）; 相反，您可能会尝试将此代码调整为您尝试解决的其他问题。 毕竟，这就是这样的书籍的全部要点 - 为您提供以有趣的新方式解决问题的工具。 那么，如果某些东西不按预期工作，你该怎么办？

### <span id="8">诊断错误</span>

如果对Yaws的请求不起作用，它将显示一个屏幕链接，如图1所示。乍一看这看起来有点神秘，但实际上非常有用。 首先，您将注意到包含带有违规代码的Erlang模块的文件的路径。 然后，您将看到它崩溃的原因（在这种情况下，是对已卸载模块中的函数的调用），然后是所做的请求和堆栈跟踪。 在Erlang R15中，此堆栈跟踪还将包含行号; 此屏幕截图来自R14B02，不包括它们。

![Alt text](../img/httpatomoreillycomsourceoreillyimages1220114-2.png "Figure 1. Error Page")

### <span id="9">你在运行什么版本的Erlang和Yaws?</span>

这本书是围绕Erlang R14B02和R15B建立的。 理想情况下，您应该使用R15B或更高版本。 这是一个主要版本，其他功能包括堆栈跟踪中的行号，这使得查找错误更容易。 您可以通过从命令行运行erl -v来找到您拥有的Erlang版本。

这本书也是用Yaws 1.92版本制作的。 您可以通过命令行运行yaws -v找到您的Yaws版本。 [第6章](https://www.safaribooksonline.com/library/view/Building+Web+Applications+with+Erlang/9781449320621/ch06.html)中描述的Web套接字接口在Yaws版本1.90和1.92之间发生了重大变化。

### <span id="10">一切都正确加载吗？</span>

从PHP或Perl等语言来到Erlang的程序员会发现Erlang还有一个额外的步骤。虽然Yaws将自动编译并加载新的.yaws文件（请参阅Yaws中的[动态内容](https://www.safaribooksonline.com/library/view/Building+Web+Applications+with+Erlang/9781449320621/ch02.html#dynamic_content)），但必须编译任何其他Erlang模块并将其加载到Erlang运行时。可以使用c（Module）在Erlang shell中进行编译。命令，它还将新代码加载到Erlang运行时。这对于代码的交互式测试和开发周期的速度非常有用。从PHP转换到Erlang的人肯定会不时忘记这一步。

也可以使用来自Unix shell的erlc命令从外部命令行编译Erlang代码。Erlang将自动加载代码;但是，正确设置包含路径以便它可以找到.beam文件非常重要。此选项适用于自动构建等操作。通过将加载命令添加到.erlang文件或其他配置选项，可以自动加载外部模块。

此外，Erlang应用程序通常由许多模块组成，所有模块都必须加载到系统中才能工作。因此，如果某些内容失败，请检查模块是否尚未加载或不在路径中。要查看shell的当前路径，请运行code：get_path()。

关于Erlang的一个好处是，如果以合理的方式设置系统，则永远不需要让整个系统脱机以上载新版本的代码。

### <span id="12">你是否正确地调用一切</span>

Erlang命令行是你的朋友！ 这是一个试用代码并查看代码是否按预期工作的好地方。 不要害怕在命令行创建测试数据并为您的函数提供测试输入，以确保它们返回正确的结果。

> Note: 加载模块时，其记录不会加载到shell中。 这必须使用Erlang shell中的rr命令显式完成。 您还可以使用rd定义记录并使用rf删除记录。 要使用它们，请在Erlang命令行上键入help()。

### <span id="13">Mnesia正在运行正确的表吗?</span>

必须启动Erlang的内置数据库Mnesia，并为其创建table。 在启动Mnesia之前，您必须运行命令mnesia:create_schema/1，它为Mnesia创建基本数据库存储; 然后，启动Mnesia使用命令application:start(mnesia)。 如果您在使用Mnesia表时遇到问题，可以在Erlang命令提示符下键入tv:start() 来使用表查看器。

### <span id="14">示例是否简单错误？</span>

显然，我已经尝试确保本书中的所有代码都是第一次运行顺利，但错误可能会随之而来。 您需要查看本书网页上的勘误表（请参阅前言末尾的“如何联系我们”部分），并下载示例代码，该代码将更新以修复发布后发现的任何错误。

## <span id="15">本书中使用的约定</span>

本书使用以下印刷约定：

_斜体_

> 表示新术语，URL，电子邮件地址，文件名和文件扩展名。

恒定宽度

> 用于程序列表，以及段落内部，用于引用程序元素，如变量或函数名称，数据库，数据类型，环境变量，语句和关键字。

**恒定宽度粗体**

> 显示应由用户按字面输入的命令或其他文本。

_等宽斜体_

> 显示应使用用户提供的值替换的文本或由上下文确定的值。

## <span id="16">使用代码示例</span>

这本书是为了帮助你完成工作。通常，您可以在程序和文档中使用本书中的代码。除非您复制了大部分代码，否则您无需与我们联系以获得许可。例如，编写使用本书中几个代码块的程序不需要许可。出售或分发O'Reilly书籍中的示例CD-ROM需要获得许可。通过引用本书并引用示例代码来回答问题不需要许可。将本书中的大量示例代码合并到产品文档中需要获得许可。

我们感谢，但不要求，归属。归属通常包括标题，作者，出版商和ISBN。例如：“使用Zachary Kessin的Erlang构建Web应用程序（O'Reilly）。版权所有2012 Zachary Kessin，978-1-449-30996-1。“

如果您认为您对代码示例的使用不属于合理使用范围或上述许可，请随时通过permissions@oreilly.com与我们联系。

## <span id="17">Safari®联机丛书</span>

> Note: Safari Books Online（www.safaribooksonline.com）是一个按需数字图书馆，提供来自世界领先的技术和商业作者的书籍和视频形式的专家内容。

技术专业人员，软件开发人员，网页设计人员以及商业和创意专业人员使用Safari Books Online作为研究，解决问题，学习和认证培训的主要资源。

Safari Books Online为组织，政府机构和个人提供一系列[产品](https://www.oreilly.com/online-learning/pricing.html)组合和定价计划。 订阅者可以在一个完全可搜索的数据库中访问数千本书籍，培训视频和预发布稿件，这些数据库来自O'Reilly Media，Prentice Hall Professional，Addison-Wesley Professional，Microsoft Press，Sams，Que，Peachpit Press，Focal Press，Cisco等出版商。 Press，John Wiley＆Sons，Syngress，Morgan Kaufmann，IBM Redbooks，Packt，Adobe Press，FT Press，Apress，Manning，New Riders，McGraw-Hill，Jones＆Bartlett，Course Technology等等。 有关Safari Books Online的[更多](https://www.oreilly.com/online-learning/)信息，请访问我们的[网站](https://www.safaribooksonline.com/home/)。

## <span id="18">如何联系我们</span>

请向出版商提出有关本书的评论和问题：
```
O'Reilly Media，Inc。
1005 Gravenstein Highway North
Sebastopol，CA 95472
800-998-9938（在美国或加拿大）
707-829-0515（国际或当地）
707-829-0104（传真）
```

我们有一本本书的网页，其中列出了勘误表，示例和任何其他信息。 您可以访问此页面：

http://oreil.ly/build_webapps_erlang

要对本书发表评论或提出技术问题，请发送电子邮件至：

bookquestions@oreilly.com

有关我们的书籍，课程，会议和新闻的更多信息，请访问我们的网站 http://www.oreilly.com。

在Facebook上找到我们：http://facebook.com/oreilly

在Twitter上关注我们：http://twitter.com/oreillymedia

在YouTube上观看我们：http://www.youtube.com/oreillymedia

### 致谢

一本书是一个团队的努力，如果没有一个伟大的团队支持我，我就不可能写这本书。首先，我要感谢Simon St. Laurent让我有机会写这本书，并通过整理这个过程来支持我。

我还要感谢Reuven Lerner，他帮助我成为一名顾问，并使其变得比以往更有趣。

我还要感谢我的技术评审员：

FredHébert是了解Great Good的背后的人，这是[学习Erlang的好方法](https://learnyousomeerlang.com)。你可以在推特上找到弗雷德@mononcqc。

自2008年以来，Steve Vinoski一直是Yaws项目的贡献者和提交者。他还撰写了IEEE Internet Computing的“Functional Web”专栏，内容涉及函数式编程语言的应用和Web系统开发的技术。在http://steve.vinoski.net/在线查找他的专栏。

Francesco Cesarini是Erlang Programming的合着者和Erlang Solutions的首席执行官。

我还要感谢所有通过电子邮件发送和推特给我这本书的人。希望对你有帮助！请随时在Twitter上联系我@zkessin。

当然，我要感谢Joe Armstrong创建Erlang，以及“klacke”（Claes Wikstrom）创建Yaws以及Erlang生态系统的其他各个部分。没有他们，这本书就不存在了。

最后，我要感谢我的妻子Devora，他忍受了我在电脑前花费的时间比她想要的多了几个小时，然后忍受了几个肮脏的餐具，我花了更长的时间做的比我可能做的更多应该有。