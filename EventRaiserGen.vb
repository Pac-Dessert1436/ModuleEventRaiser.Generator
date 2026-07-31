Imports Microsoft.CodeAnalysis
Imports Microsoft.CodeAnalysis.Text
Imports Microsoft.CodeAnalysis.VisualBasic
Imports Microsoft.CodeAnalysis.VisualBasic.Syntax
Imports IGIC = Microsoft.CodeAnalysis.IncrementalGeneratorInitializationContext

<Generator(LanguageNames.VisualBasic)>
Public NotInheritable Class EventRaiserGen
    Implements IIncrementalGenerator

    Private Class ModuleInfo
        Public Property ModuleName As String
        Public Property Events As List(Of EventInfo)
        Public Property RequiredNamespaces As List(Of String)
        Public Property ContainingNamespace As String
        Public Property IsPublicModule As Boolean
    End Class

    Private Class EventInfo
        Public Property EventName As String
        Public Property ModuleName As String
        Public Property EventType As String
        Public Property Parameters As List(Of ParameterInfo)
        Public Property RequiredNamespaces As HashSet(Of String)
        Public Property Location As Location
        Public Property IsDelegatePattern As Boolean
        Public Property DelegateTypeName As String
        Public Property ContainingNamespace As String
        Public Property Accessibility As Accessibility

        ' Fixed in 1.2.4: Generated methods for `Friend Event`s are no longer `Internal Sub`.
        Public ReadOnly Property AccessibilityString As String
            Get
                Return If(Accessibility = Accessibility.Internal, "Friend", Accessibility.ToString())
            End Get
        End Property
    End Class

    Private Class ParameterInfo
        Public Property ParamName As String
        Public Property ParamType As String
        Public Property ContainingNamespace As String
    End Class

    Public Sub Initialize(context As IGIC) Implements IIncrementalGenerator.Initialize
        Dim moduleAccessLevel As Accessibility

        ' Create a pipeline for VB.NET syntax nodes
        Dim syntaxProvider = context.SyntaxProvider.CreateSyntaxProvider(
            Function(syntaxNode, token)
                ' Check if the node is an EventStatementSyntax (VB.NET event declaration)
                Dim eventDecl = TryCast(syntaxNode, EventStatementSyntax)
                If eventDecl Is Nothing Then Return False

                ' Check if the event is declared inside a module by looking at ancestors
                Dim moduleDecl = eventDecl.FirstAncestorOrSelf(Of ModuleBlockSyntax)()
                If moduleDecl Is Nothing Then Return False

                Return True
            End Function,
            Function(gsc, token)
                Dim eventDecl = DirectCast(gsc.Node, EventStatementSyntax)
                Dim semanticModel = gsc.SemanticModel

                ' Find the containing module using FirstAncestorOrSelf
                Dim moduleBlock = eventDecl.FirstAncestorOrSelf(Of ModuleBlockSyntax)()
                Dim moduleStatement = moduleBlock?.BlockStatement

                ' NEW in 1.1.8: Check accessibility level for each module (fixed in 1.2.4)
                moduleAccessLevel = gsc.SemanticModel.GetDeclaredSymbol(moduleStatement).DeclaredAccessibility
                ' Return nothing if not in a module (should be filtered out by predicate)
                If moduleStatement Is Nothing Then Return Nothing

                ' Initialize collections
                Dim parameters As New List(Of ParameterInfo)
                Dim requiredNamespaces As New HashSet(Of String)
                Dim isDelegateEvent = False
                Dim delegateTypeName = ""

                ' Check if this is a delegate-based event (As SomeDelegate)
                If eventDecl.AsClause IsNot Nothing Then
                    ' This is an "As EventHandler" style event
                    isDelegateEvent = True
                    delegateTypeName = eventDecl.AsClause.Type.ToString()
                    ' NOTE: Use the GetParametersFromDelegate function here!
                    parameters = GetParametersFromDelegate(eventDecl.AsClause.Type, semanticModel)

                    ' Collect namespaces from the extracted parameters
                    For Each pInfo As ParameterInfo In parameters
                        If Not String.IsNullOrEmpty(pInfo.ContainingNamespace) Then
                            requiredNamespaces.Add(pInfo.ContainingNamespace)
                        End If
                    Next pInfo
                    ' Also add the delegate's namespace if needed
                    Dim typeInfo = semanticModel.GetTypeInfo(eventDecl.AsClause.Type)
                    If typeInfo.Type IsNot Nothing Then
                        Dim delegateNamespace = typeInfo.Type.ContainingNamespace.ToDisplayString()
                        If Not String.IsNullOrEmpty(delegateNamespace) AndAlso
                           Not delegateNamespace = "System" Then
                            requiredNamespaces.Add(delegateNamespace)
                        End If
                    End If
                Else
                    ' This is a traditional event with parameter list
                    parameters = GetEventParameters(eventDecl, semanticModel)

                    ' Collect namespaces from parameters
                    For Each pInfo As ParameterInfo In parameters
                        If Not String.IsNullOrEmpty(pInfo.ContainingNamespace) Then
                            requiredNamespaces.Add(pInfo.ContainingNamespace)
                        End If
                    Next
                End If

                ' Get the containing namespace from the module
                Dim containingNamespace As String = String.Empty
                Dim namespaceDecl = moduleBlock.FirstAncestorOrSelf(Of NamespaceBlockSyntax)()
                If namespaceDecl IsNot Nothing Then
                    containingNamespace = namespaceDecl.NamespaceStatement.Name.ToString()
                End If

                ' Get the accessibility level of the event
                Dim eventSymbol = semanticModel.GetDeclaredSymbol(eventDecl)
                Dim eventAccessibility = If(eventSymbol IsNot Nothing, eventSymbol.DeclaredAccessibility, Accessibility.Public)

                Return New EventInfo With {
                    .EventName = eventDecl.Identifier.ValueText,
                    .ModuleName = moduleStatement.Identifier.ValueText,
                    .EventType = If(eventDecl.AsClause IsNot Nothing,
                        eventDecl.AsClause.Type.ToString(), "EventHandler"),
                    .Parameters = parameters,
                    .RequiredNamespaces = requiredNamespaces,
                    .Location = moduleStatement.GetLocation(),
                    .IsDelegatePattern = isDelegateEvent,
                    .DelegateTypeName = delegateTypeName,
                    .ContainingNamespace = containingNamespace,
                    .Accessibility = eventAccessibility
                }
            End Function
        )

        ' Filter out any null values from the provider
        Dim filteredEvents = syntaxProvider.Where(Function(e) e IsNot Nothing)

        ' Group events by module name and merge namespaces
        Dim groupedByModule = filteredEvents.Collect().
            Select(Function(events, token)
                       ' Group by module name
                       Dim moduleGroups = events.GroupBy(Function(e) e.ModuleName)

                       ' For each module, merge the required namespaces from all events
                       Return moduleGroups.Select(
                           Function(group)
                               Dim moduleName = group.Key
                               Dim eventsInModule = group.ToList()

                               ' Merge all namespaces from all events in this module
                               Dim allNamespaces As New HashSet(Of String)
                               For Each evt In eventsInModule
                                   If evt.RequiredNamespaces IsNot Nothing Then
                                       allNamespaces.UnionWith(evt.RequiredNamespaces)
                                   End If
                               Next evt

                               ' Extract the containing namespace from the first event (should be
                               ' consistent for all events in same module)
                               Dim containingNamespace As String = String.Empty
                               Dim firstEventNsp = eventsInModule(0).ContainingNamespace
                               If eventsInModule.Count > 0 AndAlso Not String.
                                   IsNullOrEmpty(firstEventNsp) Then containingNamespace = firstEventNsp

                               Return New ModuleInfo With {
                                    .ModuleName = moduleName,
                                    .Events = eventsInModule,
                                    .RequiredNamespaces = allNamespaces.ToList(),
                                    .ContainingNamespace = containingNamespace,
                                    .IsPublicModule = moduleAccessLevel = Accessibility.Public
                                }
                           End Function).ToList()
                   End Function)

        ' New in version 1.2.0: Unified module event scheduler as a breaking change
        context.RegisterPostInitializationOutput(
            Sub(ctx) ctx.AddSource("ModuleEventScheduler.g.vb", EVENT_SCHEDULER_CLASSDEF))

        ' Improved in version 1.2.3+: Weak multicast event support
        context.RegisterPostInitializationOutput(
            Sub(ctx) ctx.AddSource("WeakMulticastEvent.g.vb", WEAK_EVENT_CLASSDEF))

        ' Register the source output
        context.RegisterSourceOutput(groupedByModule,
            Sub(sourceContext, moduleInfos)
                For Each modInfo As ModuleInfo In moduleInfos
                    ' Generate a single file for this module with all event raisers
                    Dim sourceCode = GenerateModuleRaiseMethods(modInfo)
                    Dim fileName = $"{modInfo.ModuleName}_EventRaisers.g.vb"

                    sourceContext.AddSource(
                        fileName, SourceText.From(sourceCode, System.Text.Encoding.UTF8))
                Next modInfo
            End Sub)
    End Sub

    Private Function GetParametersFromDelegate(
        delegateTypeSyntax As TypeSyntax, semanticModel As SemanticModel) As List(Of ParameterInfo)

        Dim parameters As New List(Of ParameterInfo)
        Dim typeInfo = semanticModel.GetTypeInfo(delegateTypeSyntax)

        If typeInfo.Type IsNot Nothing Then
            Dim delegateSymbol = TryCast(typeInfo.Type, INamedTypeSymbol)
            If delegateSymbol IsNot Nothing AndAlso delegateSymbol.DelegateInvokeMethod IsNot Nothing Then
                Dim invokeMethod = delegateSymbol.DelegateInvokeMethod

                For Each param In invokeMethod.Parameters
                    Dim paramType = param.Type.ToDisplayString()
                    Dim containingNamespace = param.Type.ContainingNamespace.ToDisplayString()

                    ' Generate a meaningful parameter name
                    Dim paramName = param.Name
                    If String.IsNullOrEmpty(paramName) Then
                        ' Descriptive naming doesn't work at all - fall back to "arg*" naming
                        paramName = String.Format("arg{0}", parameters.Count)
                    End If

                    parameters.Add(New ParameterInfo With {
                        .ParamName = paramName,
                        .ParamType = paramType,
                        .ContainingNamespace = containingNamespace
                    })
                Next param
            End If
        End If

        Return parameters
    End Function

    Private Shared Function GetEventParameters _
        (eventDecl As EventStatementSyntax, semanticModel As SemanticModel) As List(Of ParameterInfo)

        Dim parameters As New List(Of ParameterInfo)
        If eventDecl.ParameterList IsNot Nothing Then
            For Each paramSyntax In eventDecl.ParameterList.Parameters
                Dim paramName = paramSyntax.Identifier.Identifier.ValueText
                Dim paramTypeName = "Object"
                Dim containingNamespace = String.Empty

                ' Try to get type information from the semantic model
                If paramSyntax.AsClause IsNot Nothing Then
                    paramTypeName = paramSyntax.AsClause.Type.ToString()

                    ' Get the symbol for the type to find its namespace
                    Dim typeInfo = semanticModel.GetTypeInfo(paramSyntax.AsClause.Type)
                    If typeInfo.Type IsNot Nothing Then
                        Dim typeSymbol = typeInfo.Type

                        ' Get the containing namespace
                        Dim namespaceSymbol = typeSymbol.ContainingNamespace
                        If namespaceSymbol IsNot Nothing AndAlso
                            Not namespaceSymbol.IsGlobalNamespace Then
                            containingNamespace = namespaceSymbol.ToDisplayString()
                        End If

                        ' Use the fully qualified type name to ensure correct imports
                        paramTypeName = typeSymbol.ToDisplayString()
                    End If
                End If

                parameters.Add(New ParameterInfo With {
                    .ParamName = paramName,
                    .ParamType = paramTypeName,
                    .ContainingNamespace = containingNamespace
                })
            Next paramSyntax
        End If

        Return parameters
    End Function

    Private Shared Function GenerateModuleRaiseMethods(modInfo As ModuleInfo) As String
        Dim code As New System.Text.StringBuilder

        ' Prepare the parameter description function
        Dim ParameterDescription =
            Function(pInfo As ParameterInfo) As String
                Select Case True
                    Case pInfo.ParamName = "sender" AndAlso pInfo.ParamType = "Object"
                        Return "The source of the event."
                    Case pInfo.ParamName = "e" AndAlso pInfo.ParamType = "System.EventArgs"
                        Return "An object that contains the event data."
                    Case pInfo.ParamName.StartsWith("arg")
                        ' Extract number from ParamName of the parameter info
                        Dim substr = pInfo.ParamName.Substring(3), id = 0
                        If Not Integer.TryParse(substr, id) Then GoTo DefaultCase
                        Dim suffix As String
                        If Math.Abs(id) Mod 100 >= 10 AndAlso Math.Abs(id) Mod 100 <= 20 Then
                            suffix = "th"
                        Else
                            Select Case Math.Abs(id) Mod 10
                                Case 1
                                    suffix = "st"
                                Case 2
                                    suffix = "nd"
                                Case 3
                                    suffix = "rd"
                                Case Else
                                    suffix = "th"
                            End Select
                        End If
                        Return $"The {id}{suffix} argument to raise the event with."
                    Case Else
DefaultCase:            Dim desc As String = pInfo.ParamName
                        ' Add spaces between camelCase or PascalCase words
                        For i As Integer = 1 To desc.Length - 1
                            If Char.IsUpper(desc(i)) Then
                                desc = desc.Insert(i, " ")
                                i += 1 ' Skip the space we just added
                            End If
                        Next i
                        Return $"The {desc.ToLower()} value to raise the event with."
                End Select
            End Function

        ' Add file header
        code.AppendLine("' <auto-generated>")
        code.AppendLine("'     This code was generated by `ModuleEventRaiser.Generator`.")
        code.AppendLine("'     Changes to this file may cause incorrect behavior and will be lost if")
        code.AppendLine("'     the code is regenerated.")
        code.AppendLine("' </auto-generated>")
        code.AppendLine()
        code.AppendLine("Option Explicit On")
        code.AppendLine("Option Strict On")
        code.AppendLine()

        Dim namespaces = If(modInfo.RequiredNamespaces, New List(Of String))
        If Not namespaces.Contains("System.Threading.Tasks") Then namespaces.Add("System.Threading.Tasks")

        ' Add collected namespaces (sorted for consistency)
        If namespaces.Count > 0 Then
            For Each ns As String In From x In namespaces Order By x Distinct
                code.AppendLine($"Imports {ns}")
            Next ns
        End If
        code.AppendLine()

        ' Add namespace if the module is inside a namespace
        Dim hasNamespace As Boolean = Not String.IsNullOrEmpty(modInfo.ContainingNamespace)
        If hasNamespace Then
            code.AppendLine($"Namespace {modInfo.ContainingNamespace}")
            code.AppendLine()
        End If
        ' Begin module with proper module accessibility
        Dim accessModifier = If(modInfo.IsPublicModule, "Public", "Friend")
        
        code.AppendLine($"Partial {accessModifier} Module {modInfo.ModuleName}")
        code.AppendLine()
        code.AppendLine("    ''' <summary>")
        code.AppendLine("    ''' Provides access to the unified event scheduler for this module.")
        code.AppendLine("    ''' </summary>")
        code.AppendLine("    ''' <value>A shared instance of <see cref=""ModuleEventScheduler""/> for scheduling and raising events.</value>")
        code.AppendLine("    ''' <remarks>")
        code.AppendLine("    ''' <para>")
        code.AppendLine("    ''' This property provides a thread-safe mechanism to schedule events to be raised later,")
        code.AppendLine("    ''' which is particularly useful in game development frameworks (MonoGame, FNA, Unity, etc.)")
        code.AppendLine("    ''' where raising events during the update phase can cause performance issues.")
        code.AppendLine("    ''' </para>")
        code.AppendLine("    ''' <para>")
        code.AppendLine("    ''' <b>Usage Example:</b>")
        code.AppendLine("    ''' <code lang=""vb"">")
        code.AppendLine("    ''' ' Schedule an event to be raised later; even a simple subroutine is valid")
        code.AppendLine("    ''' EventScheduler.ScheduleEventAction(Sub() Console.WriteLine(""Hello world!""))")
        code.AppendLine("    '''")
        code.AppendLine("    ''' ' Later, typically in the Draw phase, raise all scheduled events:")
        code.AppendLine("    ''' EventScheduler.RaiseScheduledEvents()")
        code.AppendLine("    ''' </code>")
        code.AppendLine("    ''' </para>")
        code.AppendLine("    ''' <para>")
        code.AppendLine("    ''' For more information about event scheduling, see the <see cref=""ModuleEventScheduler""/> class.")
        code.AppendLine("    ''' </para>")
        code.AppendLine("    ''' </remarks>")
        code.AppendLine("    Public ReadOnly Property EventScheduler As New ModuleEventScheduler")
        code.AppendLine()
        ' Generate raise methods for each event in this module
        For Each evtInfo As EventInfo In modInfo.Events
            ' Skip if event name is empty
            If String.IsNullOrWhiteSpace(evtInfo.EventName) Then Continue For

            ' Build parameter list for the raise method
            Dim paramList As New List(Of String)
            For Each pInfo As ParameterInfo In evtInfo.Parameters
                Dim pTypeName = pInfo.ParamType
                For Each ns As String In namespaces
                    If pInfo.ParamType.StartsWith(ns) Then
                        pTypeName = pInfo.ParamType.Substring(ns.Length + 1)
                        Exit For
                    End If
                Next ns
                paramList.Add($"{pInfo.ParamName} As {pTypeName}")
            Next pInfo
            Dim params = String.Join(", ", paramList)
            ' Fixed: Add comma only if `params` is not empty
            Dim comma = If(String.IsNullOrWhiteSpace(params), "", ", ")

            ' Build argument list for RaiseEvent
            Dim argList As New List(Of String)
            For Each pInfo As ParameterInfo In evtInfo.Parameters
                argList.Add(pInfo.ParamName)
            Next pInfo
            Dim args = String.Join(", ", argList)

            ' Generate the raise method
            code.AppendLine($"    ''' <summary>")
            code.AppendLine($"    ''' Raises the <see cref=""{evtInfo.EventName}""/> event (direct invocation).")
            code.AppendLine($"    ''' </summary>")
            ' Add parameter documentation
            For Each pInfo As ParameterInfo In evtInfo.Parameters
                code.AppendLine($"    ''' <param name=""{pInfo.ParamName}"">{ParameterDescription(pInfo)}</param>")
            Next pInfo
            code.AppendLine($"    {evtInfo.AccessibilityString} Sub RaiseEvent_{evtInfo.EventName}({params})")
            code.AppendLine($"        RaiseEvent {evtInfo.EventName}({args})")
            code.AppendLine($"    End Sub")
            code.AppendLine()

            ' Generate async raise method (same as sync raise method)
            code.AppendLine($"    ''' <summary>")
            code.AppendLine($"    ''' Asynchronously raises the <see cref=""{evtInfo.EventName}""/> event. Use this method only in desktop apps, networking, etc.")
            code.AppendLine($"    ''' DO NOT USE THIS METHOD WHEN WRITING GAME LOGIC IN GAME FRAMEWORKS (MonoGame, FNA, etc.).")
            code.AppendLine($"    ''' </summary>")
            For Each pInfo As ParameterInfo In evtInfo.Parameters
                code.AppendLine($"    ''' <param name=""{pInfo.ParamName}"">{ParameterDescription(pInfo)}</param>")
            Next pInfo
            code.AppendLine($"    ''' <param name=""withDelaySec"">The delay in seconds before raising the event. Default is 0.</param>")
            code.AppendLine($"    ''' <returns>A task representing the asynchronous operation.</returns>")
            code.AppendLine($"    ''' <remarks>")
            code.AppendLine($"    ''' For game logic execution in game frameworks (MonoGame, FNA, etc.), use the <see cref=""ScheduleEvent_{evtInfo.EventName}""/> method instead.")
            code.AppendLine($"    ''' </remarks>")
            code.AppendLine($"    {evtInfo.AccessibilityString} Async Function RaiseEventAsync_{evtInfo.EventName}({params}{comma}Optional withDelaySec As Double = 0) As Task")
            code.AppendLine($"        If withDelaySec < 0 Then Throw New ArgumentOutOfRangeException(NameOf(withDelaySec), ""Delay seconds must be non-negative."")")
            code.AppendLine($"        If withDelaySec > 0 Then Await Task.Delay(TimeSpan.FromSeconds(withDelaySec))")
            code.AppendLine($"        Await Task.Run(Sub() RaiseEvent {evtInfo.EventName}({args}))")
            code.AppendLine($"    End Function")
            code.AppendLine()

            ' New in version 1.1.0+: Add "ScheduleEvent_xxx" methods for each event
            code.AppendLine($"    ''' <summary>")
            code.AppendLine($"    ''' Schedules the <see cref=""{evtInfo.EventName}""/> event to be raised later. Useful for game frameworks (MonoGame, FNA, etc.).")
            code.AppendLine($"    ''' </summary>")
            For Each pInfo As ParameterInfo In evtInfo.Parameters
                code.AppendLine($"    ''' <param name=""{pInfo.ParamName}"">{ParameterDescription(pInfo)}</param>")
            Next pInfo
            code.AppendLine($"    ''' <param name=""withPriority"">The priority value to raise the event with (default is 0).")
            code.AppendLine($"    ''' Events with higher priority values are raised first.</param>")
            code.AppendLine($"    {evtInfo.AccessibilityString} Sub ScheduleEvent_{evtInfo.EventName}({params}{comma}Optional withPriority As Integer = 0)")
            code.AppendLine($"        {modInfo.ModuleName}.EventScheduler.ScheduleEventAction(Sub() RaiseEvent {evtInfo.EventName}({args}), withPriority)")
            code.AppendLine($"    End Sub")
            code.AppendLine()
        Next evtInfo
        ' End module with proper newline (POSIX standard)
        code.AppendLine("End Module")

        ' Finally, close namespace if we opened one at the beginning
        If hasNamespace Then
            code.AppendLine()
            code.AppendLine("End Namespace")
        End If

        Return code.ToString()
    End Function
End Class