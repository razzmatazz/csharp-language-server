// https://github.com/TheAngryByrd/FsLibLog/blob/64f118ae8df2f2944ef69758052cb3b148b87e79/src/FsLibLog/FsLibLog.fs

namespace CSharpLanguageServer.Logging

open System.Text.RegularExpressions


[<AutoOpen>]
module Types =
    open System

    let nonNull (name: string) (value: 'T when 'T : null) : 'T =
        if Object.ReferenceEquals(value, null) then
            raise (new Exception(sprintf "unexpected null value for %s" name))
        else
            value

    type LogLevel =
        | Trace = 0
        | Debug = 1
        | Info = 2
        | Warn = 3
        | Error = 4
        | Fatal = 5

    /// An optional message thunk.
    ///
    /// - If <see cref="T:Microsoft.FSharp.Core.Option<_>.None">None</see> is provided, this typically signals to the logger to do a isEnabled check.
    /// - If <see cref="T:Microsoft.FSharp.Core.Option<_>.Some">Some</see> is provided, this signals the logger to log.
    type MessageThunk = (unit -> string) option

    /// The signature of a log message function
    type Logger = LogLevel -> MessageThunk -> exn option -> obj array -> bool
    type MappedContext = string -> obj -> bool -> IDisposable

    /// Type representing a Log
    [<NoEquality; NoComparison>]
    type Log =
        {
            LogLevel: LogLevel
            Message: MessageThunk
            Exception: exn option
            Parameters: obj list
            AdditionalNamedParameters: ((string * obj * bool) list)
        }

        static member StartLogLevel(logLevel: LogLevel) = {
            LogLevel = logLevel
            Message = None
            Exception = None
            Parameters = List.empty
            AdditionalNamedParameters = List.empty
        }

    /// An interface wrapper for a<see cref="T:FsLibLog.Types.Logger">Logger</see>. Useful when using depedency injection frameworks.
    type ILog =
        abstract member Log: Logger
        abstract member MappedContext: MappedContext

#if FABLE_COMPILER
    // Fable doesn't support System.Collections.Generic.Stack, so this implementation (from FCS)
    // is used instead.
    type Stack<'a>() =
        let mutable contents = Array.zeroCreate<'a> (2)
        let mutable count = 0

        member buf.Ensure newSize =
            let oldSize = contents.Length

            if newSize > oldSize then
                let old = contents
                contents <- Array.zeroCreate (max newSize (oldSize * 2))
                Array.blit old 0 contents 0 count

        member buf.Count = count

        member buf.Pop() =
            let item = contents.[count - 1]
            count <- count - 1
            item

        member buf.Peep() = contents.[count - 1]

        member buf.Top(n) =
            [ for x in contents.[max 0 (count - n) .. count - 1] -> x ]
            |> List.rev

        member buf.Push(x) =
            buf.Ensure(count + 1)
            contents.[count] <- x
            count <- count + 1

        member buf.IsEmpty = (count = 0)
#endif

    [<AutoOpen>]
    module Inner =
#if !FABLE_COMPILER
        open System.Collections.Generic
#endif

        /// <summary>
        /// DisposableStack on Dispose will call dispose on items appended to its stack in Last In First Out.
        /// </summary>
        type DisposableStack() =
            let stack = Stack<IDisposable>()

            interface IDisposable with
                member __.Dispose() =
                    while stack.Count > 0 do
                        stack.Pop().Dispose()

            member __.Push(item: IDisposable) = stack.Push item

            member __.Push(items: IDisposable list) =
                items
                |> List.iter stack.Push

            static member Create(items: IDisposable list) =
                let ds = new DisposableStack()
                ds.Push items
                ds

        type ILog with

            /// <summary>
            /// Logs a log
            /// </summary>
            /// <param name="log">The type representing a log message to be logged</param>
            /// <returns><see langword="true"/> if the log message was logged</returns>
            member logger.fromLog(log: Log) =
                use __ =
                    log.AdditionalNamedParameters
                    |> List.map (fun (key, value, destructure) -> logger.MappedContext key value destructure)
                    // This stack is important, it causes us to unwind as if you have multiple uses in a row
                    |> DisposableStack.Create

                log.Parameters
                |> List.toArray
                |> logger.Log log.LogLevel log.Message log.Exception

            /// <summary>
            /// Logs a fatal log message given a log configurer.
            /// </summary>
            /// <param name="logConfig">A function to configure a log</param>
            /// <returns><see langword="true"/>  if the log message was logged</returns>
            member logger.fatal'(logConfig: Log -> Log) =
                Log.StartLogLevel LogLevel.Fatal
                |> logConfig
                |> logger.fromLog

            /// <summary>
            /// Logs a fatal log message given a log configurer.
            /// </summary>
            /// <param name="logConfig">A function to configure a log</param>
            member logger.fatal(logConfig: Log -> Log) =
                logger.fatal' logConfig
                |> ignore

            /// <summary>
            /// Logs an error log message given a log configurer.
            /// </summary>
            /// <param name="logConfig">A function to configure a log</param>
            /// <returns><see langword="true"/>  if the log message was logged</returns>
            member logger.error'(logConfig: Log -> Log) =
                Log.StartLogLevel LogLevel.Error
                |> logConfig
                |> logger.fromLog

            /// <summary>
            /// Logs an error log message given a log configurer.
            /// </summary>
            /// <param name="logConfig">A function to configure a log</param>
            member logger.error(logConfig: Log -> Log) =
                logger.error' logConfig
                |> ignore

            /// <summary>
            /// Logs a warn log message given a log configurer.
            /// </summary>
            /// <param name="logConfig">A function to configure a log</param>
            /// <returns><see langword="true"/>  if the log message was logged</returns>
            member logger.warn'(logConfig: Log -> Log) =
                Log.StartLogLevel LogLevel.Warn
                |> logConfig
                |> logger.fromLog

            /// <summary>
            /// Logs a warn log message given a log configurer.
            /// </summary>
            /// <param name="logConfig">A function to configure a log</param>
            member logger.warn(logConfig: Log -> Log) =
                logger.warn' logConfig
                |> ignore

            /// <summary>
            /// Logs an info log message given a log configurer.
            /// </summary>
            /// <param name="logConfig">A function to configure a log</param>
            /// <returns><see langword="true"/>  if the log message was logged</returns>
            member logger.info'(logConfig: Log -> Log) =
                Log.StartLogLevel LogLevel.Info
                |> logConfig
                |> logger.fromLog

            /// <summary>
            /// Logs an info log message given a log configurer.
            /// </summary>
            /// <param name="logConfig">A function to configure a log</param>
            member logger.info(logConfig: Log -> Log) =
                logger.info' logConfig
                |> ignore

            /// <summary>
            /// Logs a debug log message given a log configurer.
            /// </summary>
            /// <param name="logConfig">A function to configure a log</param>
            /// <returns><see langword="true"/>  if the log message was logged</returns>
            member logger.debug'(logConfig: Log -> Log) =
                Log.StartLogLevel LogLevel.Debug
                |> logConfig
                |> logger.fromLog

            /// <summary>
            /// Logs a debug log message given a log configurer.
            /// </summary>
            /// <param name="logConfig">A function to configure a log</param>
            member logger.debug(logConfig: Log -> Log) =
                logger.debug' logConfig
                |> ignore

            /// <summary>
            /// Logs a trace log message given a log configurer.
            /// </summary>
            /// <param name="logConfig">A function to configure a log</param>
            /// <returns><see langword="true"/>  if the log message was logged</returns>
            member logger.trace'(logConfig: Log -> Log) =
                Log.StartLogLevel LogLevel.Trace
                |> logConfig
                |> logger.fromLog

            /// <summary>
            /// Logs a trace log message given a log configurer.
            /// </summary>
            /// <param name="logConfig">A function to configure a log</param>
            member logger.trace(logConfig: Log -> Log) =
                logger.trace' logConfig
                |> ignore


    /// An interface for retrieving a concrete logger such as Serilog, Nlog, etc.
    type ILogProvider =
        abstract member GetLogger: string -> Logger
        abstract member OpenNestedContext: string -> IDisposable
        abstract member OpenMappedContext: string -> obj -> bool -> IDisposable

    module Log =

        /// <summary>
        /// Amends a <see cref="T:FsLibLog.Types.Log">Log</see> with a message.
        /// </summary>
        /// <param name="message">The message to set for the log.</param>
        /// <param name="log">The log to amend.</param>
        /// <returns>The amended log.</returns>
        let setMessage (message: string) (log: Log) =
            { log with
                Message = Some(fun () -> message)
            }

        /// <summary>
        /// Amends a <see cref="T:FsLibLog.Types.Log">Log</see> with a message thunk.  Useful for "expensive" string construction scenarios.
        /// </summary>
        /// <param name="messageThunk">The function that generates a message to add to a Log.</param>
        /// <param name="log">The log to amend.</param>
        /// <returns>The amended log.</returns>
        let setMessageThunk (messageThunk: unit -> string) (log: Log) =
            { log with Message = Some messageThunk }

        /// <summary>
        /// Amends a <see cref="T:FsLibLog.Types.Log">Log</see> with a parameter.
        /// </summary>
        /// <param name="param">The value to add to the log</param>
        /// <param name="log">The log to amend.</param>
        /// <returns>The amended log.</returns>
        let addParameter (param: 'a) (log: Log) =
            { log with
                Parameters = List.append log.Parameters [ (box param) ]
            }

        /// <summary>
        /// Amends a <see cref="T:FsLibLog.Types.Log">Log</see> with a list of parameters.
        /// </summary>
        /// <param name="params">The values to add to the log, in the form of an `obj list`.</param>
        /// <param name="log">The log to amend.</param>
        /// <returns>The amended log.</returns>
        let addParameters (``params``: obj list) (log: Log) =
            let ``params`` =
                ``params``
                |> List.map box

            { log with
                Parameters =
                    log.Parameters
                    @ ``params``
            }


        /// <summary>
        /// Amends a <see cref="T:FsLibLog.Types.Log">Log</see> with additional named parameters for context. This helper adds more context to a log.
        /// This DOES NOT affect the parameters set for a message template.
        /// This is the same calling OpenMappedContext right before logging.
        /// </summary>
        /// <param name="key">The key of the parameter to add to the log.</param>
        /// <param name="value">The value of the parameter to add to the log.</param>
        /// <param name="log">The log to amend.</param>
        /// <returns>The amended log.</returns>
        let addContext (key: string) (value: obj) (log: Log) =
            { log with
                AdditionalNamedParameters = List.append log.AdditionalNamedParameters [ key, (box value), false ]
            }


        /// <summary>
        /// Amends a <see cref="T:FsLibLog.Types.Log">Log</see> with additional named parameters for context. This helper adds more context to a log.
        /// This DOES NOT affect the parameters set for a message template.
        /// This is the same calling OpenMappedContext right before logging.
        /// This destructures an object rather than calling `ToString()` on it.
        /// WARNING: Destructring can be expensive.
        /// </summary>
        /// <param name="key">The key of the parameter to add to the log.</param>
        /// <param name="value">The value of the parameter to add to the log.</param>
        /// <param name="log">The log to amend.</param>
        /// <returns>The amended log.</returns>
        let addContextDestructured (key: string) (value: obj) (log: Log) =
            { log with
                AdditionalNamedParameters = List.append log.AdditionalNamedParameters [ key, (box value), true ]
            }


        /// <summary>
        /// Amends a <see cref="T:FsLibLog.Types.Log">Log</see> with an <see cref="T:System.Exception">exn</see>. Handles nulls.
        /// </summary>
        /// <param name="exception">The exception to add to the log.</param>
        /// <param name="log">The log to amend.</param>
        /// <returns>The amended log.</returns>
        let addException (``exception``: exn) (log: Log) =
            { log with
                Exception = Option.ofObj ``exception``
            }

        /// <summary>
        /// Amends a <see cref="T:FsLibLog.Types.Log">Log</see> with an <see cref="T:System.Exception">exn</see>. Handles nulls.
        /// </summary>
        /// <param name="exception">The exception to add to the log.</param>
        /// <param name="log">The log to amend.</param>
        /// <returns>The amended log.</returns>
        let addExn (``exception``: exn) (log: Log) = addException ``exception`` log

        /// <summary>
        /// Amends a <see cref="T:FsLibLog.Types.Log">Log</see> with a given <see cref="T:FsLibLog.Types.LogLevel">LogLevel</see>
        /// </summary>
        /// <param name="logLevel">The level to set for the log.</param>
        /// <param name="log">The log to amend.</param>
        /// <returns>The amended log.</returns>
        let setLogLevel (logLevel: LogLevel) (log: Log) = { log with LogLevel = logLevel }

#if !FABLE_COMPILER

        let private formatterRegex =
            Regex(@"(?<!{){(?<number>\d+)(?<columnFormat>:(?<format>[^}]+))?}(?!})", RegexOptions.Compiled)

        let private isAnObject value =
            Convert.GetTypeCode(value) = TypeCode.Object

        /// <summary>
        /// Amends a <see cref="T:FsLibLog.Types.Log">Log</see> with a given interpolated string. This will generate a message template from a special syntax within the interpolation. The syntax for the interplated string is <code> $"I want to log {myVariable:MyLogVariableName}". </code>
        ///
        /// This would be equivalent of calling <code>(setMessage "I want to log {MyLogVariableName}" >> addContextDestructured "MyLogVariable" myVariable)</code>
        /// </summary>
        /// <param name="message">An interpolated string</param>
        /// <param name="log">The log to amend.</param>
        /// <returns>The amended log.</returns>
        let setMessageInterpolated (message: FormattableString) (log: Log) =
            let mutable messageFormat = message.Format

            let args =
                formatterRegex.Matches(messageFormat)
                |> Seq.cast<Match>
                |> Seq.map (fun m ->
                    let number = Int32.Parse(m.Groups.["number"].Value)
                    let formatGroup = m.Groups.["format"]
                    let propertyValue = message.GetArgument(number)
                    let propertyName = formatGroup.Value
                    let columnFormatGroup = m.Groups.["columnFormat"]
                    propertyName, propertyValue, columnFormatGroup.Index, columnFormatGroup.Length
                )
            // Reverse the args so we won't change the indexes earlier in the string
            args
            |> Seq.rev
            |> Seq.iter (fun (_, _, removeStart, removeLength) ->
                if removeLength > 0 then
                    messageFormat <- messageFormat.Remove(removeStart, removeLength)
            )

            let namedArgs =
                args
                |> Seq.map (fun (name, _, _, _) -> box $"{{{name}}}")
                |> Seq.toArray

            messageFormat <- messageFormat.Replace("{{", "{{{{").Replace("}}", "}}}}")
            // Replace numbered args with named args from regex match
            messageFormat <- String.Format(messageFormat, args = namedArgs)

            let addContexts args (log: Log) =
                let addArgsToContext =
                    (id, args)
                    ||> Seq.fold (fun state (name, value, _, _) ->
                        let contextAdder =
                            if value |> isAnObject then
                                addContextDestructured
                            else
                                addContext

                        state
                        >> contextAdder name value
                    )

                addArgsToContext log

            log
            |> setMessage messageFormat
            |> addContexts args

        /// <summary>
        /// Amends a <see cref="T:FsLibLog.Types.Log">Log</see> with a given interpolated string. This will generate a message template from a special syntax within the interpolation. The syntax for the interplated string is <code> $"I want to log {myVariable:MyLogVariableName}". </code>
        ///
        /// This would be equivalent of calling <code>(setMessage "I want to log {MyLogVariableName}" >> addContextDestructured "MyLogVariable" myVariable)</code>
        /// </summary>
        /// <param name="message">An interpolated string</param>
        /// <param name="log">The log to amend.</param>
        /// <returns>The amended log.</returns>
        let setMessageI (message: FormattableString) (log: Log) = setMessageInterpolated message log
#endif

/// Provides operators to make writing logs more streamlined.
module Operators =

    /// <summary>
    /// Amend a log with a message. Wrapper for <see cref="M:FsLibLog.Types.LogModule.setMessage">Log.setMessage</see>.
    /// </summary>
    /// <param name="message">The string of the base message.</param>
    /// <returns>A new Log instance with the specified message.</returns>
    let (!!!) message = Log.setMessage message

    /// <summary>
    /// Amends a log with a parameter. Wrapper for <see cref="M:FsLibLog.Types.LogModule.addParameter">Log.addParameter</see>.
    /// </summary>
    /// <param name="log">The Log to add the parameter to.</param>
    /// <param name="value">The value for the parameter.</param>
    /// <returns>The Log with the added parameter.</returns>
    let (>>!) log value =
        log
        >> Log.addParameter value

    /// <summary>
    /// Amends a Log with additional named parameters for context. This helper adds more context to a log.
    /// This DOES NOT affect the parameters set for a message template. This is the same calling OpenMappedContext right before logging.
    ///
    /// Wrapper for <see cref="M:FsLibLog.Types.LogModule.addContext">Log.addContext</see>.
    /// </summary>
    /// <param name="log">The log to add the parameter to.</param>
    /// <param name="key">The name for the parameter.</param>
    /// <param name="value">The value for the parameter.</param>
    /// <returns>The amended log with the parameter added.</returns>
    let (>>!-) log (key, value) =
        log
        >> Log.addContext key value

    /// <summary>
    /// Amends a Log with additional named parameters for context. This helper adds more context to a log. This DOES NOT affect the parameters set for a message template.
    /// This is the same calling OpenMappedContext right before logging. This destructures an object rather than calling ToString() on it. WARNING: Destructring can be expensive.
    ///
    /// Wrapper for <see cref="M:FsLibLog.Types.LogModule.addContextDestructured">Log.addContextDestructured</see>.
    /// </summary>
    /// <param name="log">The log to add the parameter to.</param>
    /// <param name="key">The name for the parameter.</param>
    /// <param name="value">The value for the parameter.</param>
    /// <returns>The amended log with the parameter added.</returns>
    let (>>!+) log (key, value) =
        log
        >> Log.addContextDestructured key value

    /// <summary>
    /// Amends a Log with an exn. Handles nulls.
    ///
    /// Wrapper for <see cref="M:FsLibLog.Types.LogModule.addException">Log.addException</see>.
    /// </summary>
    /// <param name="log">The log to add the parameter to.</param>
    /// <param name="e">The exception to add to the log.</param>
    /// <returns>The amended log with the parameter added.</returns>
    let (>>!!) log e =
        log
        >> Log.addException e


#if !FABLE_COMPILER
module Providers =
    module SerilogProvider =
        open System
        open System.Linq.Expressions

        let getLogManagerType () = Type.GetType("Serilog.Log, Serilog")
                                   |> nonNull "Type.GetType('Serilog.Log, Serilog')"

        let isAvailable () =
            getLogManagerType ()
            |> isNull
            |> not

        let getPushProperty () =

            let ndcContextType =
                Type.GetType("Serilog.Context.LogContext, Serilog")
                |> Option.ofObj
                |> Option.defaultWith (fun () -> Type.GetType("Serilog.Context.LogContext, Serilog.FullNetFx"))

            ()

            let pushPropertyMethod =
                ndcContextType.GetMethod(
                    "PushProperty",
                    [|
                        typedefof<string>
                        typedefof<obj>
                        typedefof<bool>
                    |]
                )

            let nameParam = Expression.Parameter(typedefof<string>, "name")

            let valueParam = Expression.Parameter(typedefof<obj>, "value")

            let destructureObjectParam =
                Expression.Parameter(typedefof<bool>, "destructureObjects")

            let pushPropertyMethodCall =
                Expression.Call(null, pushPropertyMethod, nameParam, valueParam, destructureObjectParam)

            let pushProperty =
                Expression
                    .Lambda<Func<string, obj, bool, IDisposable>>(
                        pushPropertyMethodCall,
                        nameParam,
                        valueParam,
                        destructureObjectParam
                    )
                    .Compile()

            fun key value destructure -> pushProperty.Invoke(key, value, destructure)


        let getForContextMethodCall () =
            let logManagerType = getLogManagerType ()

            let method =
                logManagerType.GetMethod(
                    "ForContext",
                    [|
                        typedefof<string>
                        typedefof<obj>
                        typedefof<bool>
                    |]
                )

            let propertyNameParam = Expression.Parameter(typedefof<string>, "propertyName")

            let valueParam = Expression.Parameter(typedefof<obj>, "value")

            let destructureObjectsParam =
                Expression.Parameter(typedefof<bool>, "destructureObjects")

            let exrs: Expression[] = [|
                propertyNameParam
                valueParam
                destructureObjectsParam
            |]

            let methodCall = Expression.Call(null, method, exrs)

            let func =
                Expression
                    .Lambda<Func<string, obj, bool, obj>>(
                        methodCall,
                        propertyNameParam,
                        valueParam,
                        destructureObjectsParam
                    )
                    .Compile()

            fun name -> func.Invoke("SourceContext", name, false)

        [<NoEquality; NoComparison>]
        type SerilogGateway =
            {
                Write: obj -> obj -> string -> obj[] -> unit
                WriteException: obj -> obj -> exn -> string -> obj[] -> unit
                IsEnabled: obj -> obj -> bool
                TranslateLevel: LogLevel -> obj
            }

            static member Create() =
                let logEventLevelType = Type.GetType("Serilog.Events.LogEventLevel, Serilog")
                                        |> nonNull "Type.GetType('Serilog.Events.LogEventLevel, Serilog')"

                let debugLevel = Enum.Parse(logEventLevelType, "Debug", false)

                let errorLevel = Enum.Parse(logEventLevelType, "Error", false)

                let fatalLevel = Enum.Parse(logEventLevelType, "Fatal", false)

                let informationLevel = Enum.Parse(logEventLevelType, "Information", false)

                let verboseLevel = Enum.Parse(logEventLevelType, "Verbose", false)

                let warningLevel = Enum.Parse(logEventLevelType, "Warning", false)

                let translateLevel (level: LogLevel) =
                    match level with
                    | LogLevel.Fatal -> fatalLevel
                    | LogLevel.Error -> errorLevel
                    | LogLevel.Warn -> warningLevel
                    | LogLevel.Info -> informationLevel
                    | LogLevel.Debug -> debugLevel
                    | LogLevel.Trace -> verboseLevel
                    | _ -> debugLevel

                let loggerType = Type.GetType("Serilog.ILogger, Serilog")
                                 |> nonNull "Type.GetType('Serilog.ILogger, Serilog')"

                let isEnabledMethodInfo = loggerType.GetMethod("IsEnabled", [| logEventLevelType |])

                let instanceParam = Expression.Parameter(typedefof<obj>)

                let instanceCast = Expression.Convert(instanceParam, loggerType)

                let levelParam = Expression.Parameter(typedefof<obj>)

                let levelCast = Expression.Convert(levelParam, logEventLevelType)

                let isEnabledMethodCall =
                    Expression.Call(instanceCast, isEnabledMethodInfo, levelCast)

                let isEnabled =
                    Expression
                        .Lambda<Func<obj, obj, bool>>(isEnabledMethodCall, instanceParam, levelParam)
                        .Compile()

                let writeMethodInfo =
                    loggerType.GetMethod(
                        "Write",
                        [|
                            logEventLevelType
                            typedefof<string>
                            typedefof<obj[]>
                        |]
                    )

                let messageParam = Expression.Parameter(typedefof<string>)
                let propertyValuesParam = Expression.Parameter(typedefof<obj[]>)

                let writeMethodExp =
                    Expression.Call(instanceCast, writeMethodInfo, levelCast, messageParam, propertyValuesParam)

                let expression =
                    Expression.Lambda<Action<obj, obj, string, obj[]>>(
                        writeMethodExp,
                        instanceParam,
                        levelParam,
                        messageParam,
                        propertyValuesParam
                    )

                let write = expression.Compile()

                let writeExceptionMethodInfo =
                    loggerType.GetMethod(
                        "Write",
                        [|
                            logEventLevelType
                            typedefof<exn>
                            typedefof<string>
                            typedefof<obj[]>
                        |]
                    )

                let exceptionParam = Expression.Parameter(typedefof<exn>)

                let writeMethodExp =
                    Expression.Call(
                        instanceCast,
                        writeExceptionMethodInfo,
                        levelCast,
                        exceptionParam,
                        messageParam,
                        propertyValuesParam
                    )

                let writeException =
                    Expression
                        .Lambda<Action<obj, obj, exn, string, obj[]>>(
                            writeMethodExp,
                            instanceParam,
                            levelParam,
                            exceptionParam,
                            messageParam,
                            propertyValuesParam
                        )
                        .Compile()

                {
                    Write =
                        (fun logger level message formattedParmeters ->
                            write.Invoke(logger, level, message, formattedParmeters)
                        )
                    WriteException =
                        fun logger level ex message formattedParmeters ->
                            writeException.Invoke(logger, level, ex, message, formattedParmeters)
                    IsEnabled = fun logger level -> isEnabled.Invoke(logger, level)
                    TranslateLevel = translateLevel
                }

        type private SeriLogProvider() =
            let getLoggerByName = getForContextMethodCall ()
            let pushProperty = getPushProperty ()
            let serilogGatewayInit = lazy (SerilogGateway.Create())

            let writeMessage logger logLevel (messageFunc: MessageThunk) ``exception`` formatParams =
                let serilogGateway = serilogGatewayInit.Value
                let translatedValue = serilogGateway.TranslateLevel logLevel

                match messageFunc with
                | None -> serilogGateway.IsEnabled logger translatedValue
                | Some _ when
                    serilogGateway.IsEnabled logger translatedValue
                    |> not
                    ->
                    false
                | Some m ->
                    match ``exception`` with
                    | Some ex -> serilogGateway.WriteException logger translatedValue ex (m ()) formatParams
                    | None -> serilogGateway.Write logger translatedValue (m ()) formatParams

                    true

            interface ILogProvider with
                member this.GetLogger(name: string) : Logger =
                    getLoggerByName name
                    |> writeMessage

                member this.OpenMappedContext (key: string) (value: obj) (destructure: bool) : IDisposable =
                    pushProperty key value destructure

                member this.OpenNestedContext(message: string) : IDisposable = pushProperty "NDC" message false

        let create () = SeriLogProvider() :> ILogProvider


    module MicrosoftExtensionsLoggingProvider =
        open System
        open System.Linq.Expressions
        open System.Reflection
        open System.Collections.Generic

        type ILoggerFactory = obj
        // This has to be set from usercode for this to light up
        let mutable private microsoftLoggerFactory: ILoggerFactory option = None

        let setMicrosoftLoggerFactory (factory: ILoggerFactory) =
            microsoftLoggerFactory <- Option.ofObj factory

        let getLogFactoryType =
            lazy
                (Type.GetType("Microsoft.Extensions.Logging.ILoggerFactory, Microsoft.Extensions.Logging.Abstractions"))

        let isAvailable () =
            getLogFactoryType.Value
            |> isNull
            |> not
            && microsoftLoggerFactory
               |> Option.isSome


        type ILogger = obj
        type LoggerName = string
        type MicrosoftLogLevel = obj
        type MessageFormat = string
        type MessageArgs = obj array

        [<NoEquality; NoComparison>]
        type LoggerFactoryGateway =
            {
                CreateLogger: ILoggerFactory -> LoggerName -> ILogger
            }

            static member Create() =
                let createLogger =
                    let factoryType = getLogFactoryType.Value

                    let createLoggerMethodInfo =
                        factoryType.GetMethod("CreateLogger", [| typedefof<string> |])

                    let instanceParam = Expression.Parameter(typedefof<ILoggerFactory>)
                    let nameParam = Expression.Parameter(typedefof<string>)
                    let instanceCast = Expression.Convert(instanceParam, factoryType)

                    let createLoggerMethodExp =
                        Expression.Call(instanceCast, createLoggerMethodInfo, nameParam)

                    let createLogger =
                        Expression
                            .Lambda<Func<ILoggerFactory, string, ILogger>>(
                                createLoggerMethodExp,
                                instanceParam,
                                nameParam
                            )
                            .Compile()

                    createLogger
                    |> FuncConvert.FromFunc

                { CreateLogger = createLogger }

        type LoggerGateway =
            {
                Write: ILogger -> MicrosoftLogLevel -> MessageFormat -> MessageArgs -> unit
                WriteError: ILogger -> MicrosoftLogLevel -> exn -> MessageFormat -> MessageArgs -> unit
                IsEnabled: ILogger -> MicrosoftLogLevel -> bool
                TranslateLevel: LogLevel -> MicrosoftLogLevel
                BeginScope: ILogger -> obj -> IDisposable
            }

            static member Create() =
                let loggerExtensions =
                    Type.GetType(
                        "Microsoft.Extensions.Logging.LoggerExtensions, Microsoft.Extensions.Logging.Abstractions"
                    )
                    |> nonNull "Type.GetType('Microsoft.Extensions.Logging.LoggerExtensions')"

                let loggerType =
                    Type.GetType("Microsoft.Extensions.Logging.ILogger, Microsoft.Extensions.Logging.Abstractions")
                    |> nonNull "Type.GetType('Microsoft.Extensions.Logging.ILogger')"

                let logEventLevelType =
                    Type.GetType("Microsoft.Extensions.Logging.LogLevel, Microsoft.Extensions.Logging.Abstractions")
                    |> nonNull "Type.GetType('Microsoft.Extensions.Logging.LogLevel')"

                let instanceParam = Expression.Parameter(typedefof<ILogger>)

                let instanceCast = Expression.Convert(instanceParam, loggerType)
                let levelParam = Expression.Parameter(typedefof<MicrosoftLogLevel>)

                let levelCast = Expression.Convert(levelParam, logEventLevelType)

                let isEnabled =
                    let isEnabledMethodInfo = loggerType.GetMethod("IsEnabled", [| logEventLevelType |])

                    let isEnabledMethodCall =
                        Expression.Call(instanceCast, isEnabledMethodInfo, levelCast)


                    Expression
                        .Lambda<Func<ILogger, MicrosoftLogLevel, bool>>(isEnabledMethodCall, instanceParam, levelParam)
                        .Compile()
                    |> FuncConvert.FromFunc

                let write, writeError =
                    let messageParam = Expression.Parameter(typedefof<MessageFormat>)
                    let propertyValuesParam = Expression.Parameter(typedefof<MessageArgs>)

                    let write =
                        let writeMethodInfo =
                            loggerExtensions.GetMethod(
                                "Log",
                                BindingFlags.Static
                                ||| BindingFlags.Public,
                                null,
                                [|
                                    loggerType
                                    logEventLevelType
                                    typedefof<MessageFormat>
                                    typedefof<MessageArgs>
                                |],
                                null
                            )

                        let writeMethodExp =
                            Expression.Call(
                                null,
                                writeMethodInfo,
                                instanceCast,
                                levelCast,
                                messageParam,
                                propertyValuesParam
                            )

                        let expression =
                            Expression.Lambda<Action<ILogger, MicrosoftLogLevel, MessageFormat, MessageArgs>>(
                                writeMethodExp,
                                instanceParam,
                                levelParam,
                                messageParam,
                                propertyValuesParam
                            )

                        expression.Compile()
                        |> FuncConvert.FromAction


                    let writeError =
                        let writeMethodInfo =
                            loggerExtensions.GetMethod(
                                "Log",
                                BindingFlags.Static
                                ||| BindingFlags.Public,
                                null,
                                [|
                                    loggerType
                                    logEventLevelType
                                    typedefof<exn>
                                    typedefof<MessageFormat>
                                    typedefof<MessageArgs>
                                |],
                                null

                            )

                        let exnParam = Expression.Parameter(typedefof<exn>)

                        let writeMethodExp =
                            Expression.Call(
                                null,
                                writeMethodInfo,
                                instanceCast,
                                levelCast,
                                exnParam,
                                messageParam,
                                propertyValuesParam
                            )

                        let expression =
                            Expression.Lambda<Action<ILogger, MicrosoftLogLevel, exn, MessageFormat, MessageArgs>>(
                                writeMethodExp,
                                instanceParam,
                                levelParam,
                                exnParam,
                                messageParam,
                                propertyValuesParam
                            )

                        expression.Compile()
                        |> FuncConvert.FromAction

                    write, writeError

                let translateLevel =

                    let debugLevel = Enum.Parse(logEventLevelType, "Debug", false)

                    let errorLevel = Enum.Parse(logEventLevelType, "Error", false)

                    let criticalLevel = Enum.Parse(logEventLevelType, "Critical", false)

                    let informationLevel = Enum.Parse(logEventLevelType, "Information", false)

                    let traceLevel = Enum.Parse(logEventLevelType, "Trace", false)

                    let warningLevel = Enum.Parse(logEventLevelType, "Warning", false)

                    fun (level: LogLevel) ->
                        match level with
                        | LogLevel.Fatal -> criticalLevel
                        | LogLevel.Error -> errorLevel
                        | LogLevel.Warn -> warningLevel
                        | LogLevel.Info -> informationLevel
                        | LogLevel.Debug -> debugLevel
                        | LogLevel.Trace -> traceLevel
                        | _ -> debugLevel

                let beginScope =
                    let beginScopeMethodInfo =
                        loggerType.GetMethod("BeginScope")
                        |> nonNull "loggerType.GetMethod('BeginScope')"
                        |> _.MakeGenericMethod(typedefof<obj>)

                    let stateParam = Expression.Parameter(typedefof<obj>)

                    let beginScopeMethodCall =
                        Expression.Call(instanceCast, beginScopeMethodInfo, stateParam)

                    Expression
                        .Lambda<Func<ILogger, obj, IDisposable>>(beginScopeMethodCall, instanceParam, stateParam)
                        .Compile()
                    |> FuncConvert.FromFunc

                {
                    Write = write
                    WriteError = writeError
                    IsEnabled = isEnabled
                    TranslateLevel = translateLevel
                    BeginScope = beginScope
                }


        type private MicrosoftProvider() =
            let factoryGateway = lazy (LoggerFactoryGateway.Create())
            let loggerGateway = lazy (LoggerGateway.Create())

            interface ILogProvider with
                member this.GetLogger(name: string) : Logger =
                    match microsoftLoggerFactory with
                    | None -> fun _ _ _ _ -> false
                    | Some factory ->
                        let logger = factoryGateway.Value.CreateLogger factory name

                        fun logLevel message exn args ->
                            let microsoftLevel = loggerGateway.Value.TranslateLevel logLevel

                            match message with
                            | Some message ->
                                let message = message ()

                                match exn with
                                | Some ex -> loggerGateway.Value.WriteError logger microsoftLevel ex message args
                                | None -> loggerGateway.Value.Write logger microsoftLevel message args

                                true
                            | None -> loggerGateway.Value.IsEnabled logger microsoftLevel

                member this.OpenMappedContext (key: string) (value: obj) (destructure: bool) : IDisposable =
                    match microsoftLoggerFactory with
                    | None ->
                        { new IDisposable with
                            member x.Dispose() = ()
                        }
                    | Some factory ->
                        // Create bogus logger that will propagate to a real logger later
                        let logger = factoryGateway.Value.CreateLogger factory (Guid.NewGuid().ToString())
                        // Requires a IEnumerable<KeyValuePair> to make sense
                        // https://nblumhardt.com/2016/11/ilogger-beginscope/
                        [ KeyValuePair(key, value) ]
                        |> box
                        |> loggerGateway.Value.BeginScope logger


                member this.OpenNestedContext(message: string) : IDisposable =
                    match microsoftLoggerFactory with
                    | None ->
                        { new IDisposable with
                            member x.Dispose() = ()
                        }
                    | Some factory ->
                        // Create bogus logger that will propagate to a real logger later
                        let logger = factoryGateway.Value.CreateLogger factory (Guid.NewGuid().ToString())

                        loggerGateway.Value.BeginScope logger (box message)

        let create () = MicrosoftProvider() :> ILogProvider

#endif


module LogProvider =
    open System
    open Types
#if !FABLE_COMPILER
    open Providers
#endif
    open System.Diagnostics
    open Microsoft.FSharp.Quotations.Patterns

    let mutable private currentLogProvider = None

    let private knownProviders = [
#if !FABLE_COMPILER
        (SerilogProvider.isAvailable, SerilogProvider.create)
        (MicrosoftExtensionsLoggingProvider.isAvailable, MicrosoftExtensionsLoggingProvider.create)
#endif
    ]

    /// Greedy search for first available LogProvider. Order of known providers matters.
    let private resolvedLogger =
        lazy
            (knownProviders
             |> Seq.tryFind (fun (isAvailable, _) -> isAvailable ())
             |> Option.map (fun (_, create) -> create ()))

    let private noopLogger _ _ _ _ = false

    let private noopDisposable =
        { new IDisposable with
            member __.Dispose() = ()
        }

    /// <summary>
    /// Allows custom override when a <c>getLogger</c> function searches for a LogProvider.
    /// </summary>
    /// <param name="logProvider">The <see cref="M:FsLibLog.Types.ILogProvider"/> to set</param>
    /// <returns></returns>
    let setLoggerProvider (logProvider: ILogProvider) = currentLogProvider <- Some logProvider

    /// <summary>
    /// Gets the currently set LogProvider or attempts to find known built in providers
    /// </summary>
    /// <returns></returns>
    let getCurrentLogProvider () =
        match currentLogProvider with
        | None -> resolvedLogger.Value
        | Some p -> Some p

    /// <summary>
    /// Opens a mapped diagnostic context.  This will allow you to set additional parameters to a log given a scope.
    /// </summary>
    /// <param name="key">The name of the property.</param>
    /// <param name="value">The value of the property.</param>
    /// <param name="destructureObjects">If true, and the value is a non-primitive, non-array type, then the value will be converted to a structure; otherwise, unknown types will be converted to scalars, which are generally stored as strings. WARNING: Destructring can be expensive.</param>
    /// <returns>An IDisposable upon disposing will remove this value from a loggers scope</returns>
    let openMappedContextDestucturable (key: string) (value: obj) (destructureObjects: bool) =
        let provider = getCurrentLogProvider ()

        match provider with
        | Some p -> p.OpenMappedContext key value destructureObjects
        | None -> noopDisposable


    /// <summary>
    /// Opens a mapped diagnostic context.  This will allow you to set additional parameters to a log given a scope. Sets destructureObjects to false.
    /// </summary>
    /// <param name="key">The name of the property.</param>
    /// <param name="value">The value of the property.</param>
    /// <returns>An IDisposable upon disposing will remove this value from a loggers scope</returns>
    let openMappedContext (key: string) (value: obj) =
        //TODO: We should try to find out if the value is a primitive
        openMappedContextDestucturable key value false


    /// <summary>
    /// Opens a nested diagnostic context.  This will allow you to set additional parameters to a log given a scope.
    /// </summary>
    /// <param name="value">The value of the property</param>
    /// <returns>An IDisposable upon disposing will remove this value from a loggers scope</returns>
    let openNestedContext (value: string) =
        let provider = getCurrentLogProvider ()

        match provider with
        | Some p -> p.OpenNestedContext value
        | None -> noopDisposable

    /// <summary>
    /// Creates a logger given a <see cref="T:System.String">string</see>. This will attempt to retrieve any loggers set with <see cref="M:FsLibLog.LogProviderModule.setLoggerProvider">Log.setLoggerProvider</see>.  It will fallback to a known list of providers.
    /// </summary>
    /// <param name="name">A name to give a logger. This can help you identify the location of where the log occurred upon reviewing the logs.</param>
    /// <returns></returns>
    let getLoggerByName (name: string) =
        let loggerProvider = getCurrentLogProvider ()

        let logFunc =
            match loggerProvider with
            | Some loggerProvider -> loggerProvider.GetLogger(name)
            | None -> noopLogger

        { new ILog with
            member x.Log = logFunc
            member x.MappedContext = openMappedContextDestucturable
        }

    /// <summary>
    /// Creates a logger given a <see cref="T:System.Type">Type</see>.  This will attempt to retrieve any loggers set with <see cref="M:FsLibLog.LogProviderModule.setLoggerProvider">Log.setLoggerProvider</see>.  It will fallback to a known list of providers.
    /// </summary>
    /// <param name="objectType">The type to generate a logger name from. </param>
    /// <returns></returns>
    let getLoggerByType (objectType: Type) =
        objectType
        |> string
        |> getLoggerByName

    /// <summary>
    /// Creates a logger given a <c>'a</c> type. This will attempt to retrieve any loggers set with <see cref="M:FsLibLog.LogProviderModule.setLoggerProvider">Log.setLoggerProvider</see>.  It will fallback to a known list of providers.
    /// </summary>
    /// <typeparam name="'a">The type to generate a name from.</typeparam>
    /// <returns></returns>
    let inline getLoggerFor<'a> () = getLoggerByType (typeof<'a>)

#if !FABLE_COMPILER
    let rec private getModuleType =
        function
        | PropertyGet (_, propertyInfo, _) -> propertyInfo.DeclaringType
        // | Call (_, methInfo, _) -> sprintf "%s.%s" methInfo.DeclaringType.FullName methInfo.Name
        // | Lambda(_, expr) -> getModuleType expr
        // | ValueWithName(_,_,instance) -> instance
        | x -> failwithf "Expression is not a property. %A" x


    /// <summary>
    /// Creates a logger given a Quotations.Expr type. This is only useful for module level declarations. It uses the DeclaringType on the PropertyInfo of the PropertyGet.
    ///
    /// It can be utilized like:
    ///
    /// <code>
    /// let rec logger = LogProvider.getLoggerByQuotation &lt;@ logger @&gt;
    /// </code>
    ///
    /// inside a module to get the modules full qualitfied name.
    /// </summary>
    /// <param name="quotation">The quotation to generate a logger name from.</param>
    /// <returns></returns>
    let getLoggerByQuotation (quotation: Quotations.Expr) =
        getModuleType quotation
        |> getLoggerByType


type LogProvider =
    /// <summary>
    /// Creates a logger based on `Reflection.MethodBase.GetCurrentMethod().FullName` and `CallerMemberName`. This is only useful for calls within functions. Results may vary on lambda and inlined functions.
    /// </summary>
    /// <param name="memberName">Do not pass anything to this parameter to get `CallerMemberName` to work.</param>
    /// <returns></returns>
    static member inline getLoggerByFunc([<System.Runtime.CompilerServices.CallerMemberName>] ?memberName: string) =
        let mi = System.Reflection.MethodBase.GetCurrentMethod()
                 |> nonNull "System.Reflection.MethodBase.GetCurrentMethod()"
        // When we're in a CE we get something like `WebBackend.App+thingsToCall2@130`.
        // CallerMemberName gets us the function that actually called it
        // Splitting off + seems like the best option to get the Fully Qualified Path
        let location =
            mi.DeclaringType
            |> nonNull "mi.DeclaringType"
            |> _.FullName.Split('+')
            |> Seq.tryHead
            |> Option.defaultValue ""

        sprintf "%s.%s" location memberName.Value
        |> LogProvider.getLoggerByName

#endif
