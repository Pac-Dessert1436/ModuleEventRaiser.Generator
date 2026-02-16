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
    End Class

    Private Class ParameterInfo
        Public Property ParamName As String
        Public Property ParamType As String
        Public Property ContainingNamespace As String
    End Class

    Public Sub Initialize(context As IGIC) Implements IIncrementalGenerator.Initialize
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

                    ' USE the GetParametersFromDelegate function here!
                    parameters = GetParametersFromDelegate(eventDecl.AsClause.Type, semanticModel)

                    ' Collect namespaces from the extracted parameters
                    For Each pInfo As ParameterInfo In parameters
                        If Not String.IsNullOrEmpty(pInfo.ContainingNamespace) Then
                            requiredNamespaces.Add(pInfo.ContainingNamespace)
                        End If
                    Next

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

                Return New EventInfo With {
                    .EventName = eventDecl.Identifier.ValueText,
                    .ModuleName = moduleStatement.Identifier.ValueText,
                    .EventType = If(eventDecl.AsClause IsNot Nothing,
                        eventDecl.AsClause.Type.ToString(), "EventHandler"),
                    .Parameters = parameters,
                    .RequiredNamespaces = requiredNamespaces,
                    .Location = moduleStatement.GetLocation(),
                    .IsDelegatePattern = isDelegateEvent,
                    .DelegateTypeName = delegateTypeName
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
                       Return moduleGroups.Select(Function(group)
                                                      Dim moduleName = group.Key
                                                      Dim eventsInModule = group.ToList()

                                                      ' Merge all namespaces from all events in this module
                                                      Dim allNamespaces = New HashSet(Of String)()
                                                      For Each evt In eventsInModule
                                                          If evt.RequiredNamespaces IsNot Nothing Then
                                                              allNamespaces.UnionWith(evt.RequiredNamespaces)
                                                          End If
                                                      Next evt

                                                      Return New ModuleInfo With {
                                                          .ModuleName = moduleName,
                                                          .Events = eventsInModule,
                                                          .RequiredNamespaces = allNamespaces.ToList()
                                                      }
                                                  End Function).ToList()
                   End Function)

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
    delegateTypeSyntax As TypeSyntax,
    semanticModel As SemanticModel) As List(Of ParameterInfo)

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
                        ' Use common naming conventions based on type
                        paramName = GetDefaultParameterName(param.Type, parameters.Count + 1)
                    End If

                    parameters.Add(New ParameterInfo With {
                    .ParamName = paramName,
                    .ParamType = paramType,
                    .ContainingNamespace = containingNamespace
                })
                Next
            End If
        End If

        Return parameters
    End Function

    Private Function GetDefaultParameterName(typeSymbol As ITypeSymbol, index As Integer) As String
        ' Common naming conventions
        Select Case typeSymbol.Name
            Case "Object", "Object?" : Return "sender"
            Case "EventArgs", "EventArgs?" : Return "e"
            Case "String", "String?" : Return "value"
            Case "Integer", "Integer?" : Return "count"
            Case "Boolean", "Boolean?" : Return "flag"
            Case Else
                ' Use type name with first letter lowercase
                Dim typeName = typeSymbol.Name
                If typeName.Length > 0 Then
                    Return Char.ToLowerInvariant(typeName(0)) & typeName.Substring(1)
                End If
                Return $"arg{index}"
        End Select
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

    Private Shared Function GenerateModuleRaiseMethods(moduleInfo As ModuleInfo) As String
        Dim code As New System.Text.StringBuilder

        ' Prepare the parameter description function
        Dim ParameterDescription =
            Function(pInfo As ParameterInfo) As String
                Select Case True
                    Case pInfo.ParamName = "sender" AndAlso pInfo.ParamType = "Object"
                        Return "The source of the event."
                    Case pInfo.ParamName = "e" AndAlso pInfo.ParamType = "System.EventArgs"
                        Return "An object that contains the event data."
                    Case Else
                        Dim desc As String = pInfo.ParamName
                        ' Add spaces between camelCase words
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

        Dim namespaces = moduleInfo.RequiredNamespaces
        ' Add collected namespaces (sorted for consistency)
        If namespaces IsNot Nothing AndAlso namespaces.Count > 0 Then
            For Each ns As String In namespaces.OrderBy(Function(x) x)
                code.AppendLine($"Imports {ns}")
            Next ns
        End If
        code.AppendLine()

        ' Begin module
        code.AppendLine($"Partial Public Module {moduleInfo.ModuleName}")
        code.AppendLine()

        ' Generate raise methods for each event in this module
        For Each evtInfo As EventInfo In moduleInfo.Events
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

            ' Build argument list for RaiseEvent
            Dim argList As New List(Of String)
            For Each pInfo As ParameterInfo In evtInfo.Parameters
                argList.Add(pInfo.ParamName)
            Next pInfo
            Dim args = String.Join(", ", argList)

            ' Generate the raise method
            code.AppendLine($"    ''' <summary>")
            code.AppendLine($"    ''' Raises the {evtInfo.EventName} event.")
            code.AppendLine($"    ''' </summary>")

            ' Add parameter documentation
            For Each pInfo As ParameterInfo In evtInfo.Parameters
                code.AppendLine($"    ''' <param name=""{pInfo.ParamName}"">{ParameterDescription(pInfo)}</param>")
            Next pInfo

            code.AppendLine($"    Public Sub RaiseEvent_{evtInfo.EventName}({params})")
            code.AppendLine($"        RaiseEvent {evtInfo.EventName}({args})")
            code.AppendLine($"    End Sub")
            code.AppendLine()

            ' Generate async raise method (same as sync raise method)
            code.AppendLine($"    ''' <summary>")
            code.AppendLine($"    ''' Asynchronously raises the {evtInfo.EventName} event. DO NOT USE THIS METHOD IN GAME FRAMEWORKS (MonoGame, FNA, etc.)")
            code.AppendLine($"    ''' </summary>")
            For Each pInfo As ParameterInfo In evtInfo.Parameters
                code.AppendLine($"    ''' <param name=""{pInfo.ParamName}"">{ParameterDescription(pInfo)}</param>")
            Next pInfo
            code.AppendLine($"    ''' <returns>A task representing the asynchronous operation.</returns>")
            code.AppendLine($"    ''' <remarks>")
            code.AppendLine($"    ''' For game frameworks (MonoGame, FNA, etc.), use the 'ScheduleEvent_{evtInfo.EventName}' method instead.")
            code.AppendLine($"    ''' </remarks>")
            code.AppendLine($"    Public Async Function RaiseEventAsync_{evtInfo.EventName}({params}) As Task")
            code.AppendLine($"        Await Task.Run(Sub() RaiseEvent {evtInfo.EventName}({args}))")
            code.AppendLine($"    End Function")
            code.AppendLine()

            ' New in version 1.1.0: Add "ScheduleEvent_xxx" methods for each event
            code.AppendLine($"    ''' <summary>")
            code.AppendLine($"    ''' Schedules the {evtInfo.EventName} event to be raised later. Useful for game frameworks (MonoGame, FNA, etc.).")
            code.AppendLine($"    ''' </summary>")
            For Each pInfo As ParameterInfo In evtInfo.Parameters
                code.AppendLine($"    ''' <param name=""{pInfo.ParamName}"">{ParameterDescription(pInfo)}</param>")
            Next pInfo
            code.AppendLine($"    Public Sub ScheduleEvent_{evtInfo.EventName}({params})")
            code.AppendLine($"        ScheduleEventAction(Sub() RaiseEvent {evtInfo.EventName}({args}))")
            code.AppendLine($"    End Sub")
            code.AppendLine()
        Next evtInfo

        ' End module with proper newline (POSIX standard)
        code.AppendLine("End Module")

        ' New in version 1.1.0+: Event scheduler module with comprehensive documentation
        code.AppendLine($"
''' <summary>
''' Schedules event actions from the {moduleInfo.ModuleName} module to be raised later. 
''' Useful for game frameworks (MonoGame, FNA, etc.) where you want to avoid raising events 
''' during the update phase.
''' </summary>
''' <remarks>
''' This module provides a thread-safe way to queue events and raise them at a later time,
''' particularly important in game development to maintain consistent frame rates and avoid
''' race conditions.
''' </remarks>
Public Module {moduleInfo.ModuleName}EventScheduler
    Private ReadOnly _pendingEvents As New List(Of Action)
    Private ReadOnly _lock As New Object()

    ''' <summary>
    ''' Schedules an event action to be raised later.
    ''' </summary>
    ''' <param name=""eventAction"">The event action to schedule.</param>
    ''' <remarks>
    ''' This method is thread-safe and can be called from any thread.
    ''' </remarks>
    Public Sub ScheduleEventAction(eventAction As Action)
        SyncLock _lock
            _pendingEvents.Add(eventAction)
        End SyncLock
    End Sub

    ''' <summary>
    ''' Raises all scheduled event actions defined in this module.
    ''' </summary>
    ''' <remarks>
    ''' This method is thread-safe and should be called during a phase where
    ''' event handling is safe (e.g., during the 'Draw' phase in game frameworks).
    ''' All scheduled events are raised in the order they were scheduled.
    ''' </remarks>
    Public Sub RaiseScheduledEvents()
        Dim actionsToRaise = Array.Empty(Of Action)()
        SyncLock _lock
            If _pendingEvents.Count = 0 Then Exit Sub
            actionsToRaise = _pendingEvents.ToArray()
            _pendingEvents.Clear()
        End SyncLock

        ' Raise all events outside the lock to avoid deadlocks
        Array.ForEach(actionsToRaise, Sub(atn) atn.Invoke())
    End Sub

    ''' <summary>
    ''' Gets the number of pending events scheduled to be raised.
    ''' </summary>
    ''' <returns>The number of pending events.</returns>
    ''' <remarks>
    ''' This method is thread-safe and can be called from any thread.
    ''' </remarks>
    Public ReadOnly Property PendingEventCount As Integer
        Get
            SyncLock _lock
                Return _pendingEvents.Count
            End SyncLock
        End Get
    End Property

    ''' <summary>
    ''' Clears all scheduled events without raising them.
    ''' </summary>
    ''' <remarks>
    ''' This method is thread-safe and can be called from any thread.
    ''' </remarks>
    Public Sub ClearScheduledEvents()
        SyncLock _lock
            _pendingEvents.Clear()
        End SyncLock
    End Sub
End Module")

        Return code.ToString()
    End Function
End Class