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
    End Class

    Private Class ParameterInfo
        Public Property Name As String
        Public Property Type As String
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

                ' Get event parameters with namespace information
                Dim parameters = GetEventParameters(eventDecl, semanticModel)

                ' Collect all required namespaces from parameter types
                Dim requiredNamespaces As New HashSet(Of String)
                For Each param As ParameterInfo In parameters
                    If Not String.IsNullOrEmpty(param.ContainingNamespace) Then
                        requiredNamespaces.Add(param.ContainingNamespace)
                    End If
                Next param

                Return New EventInfo With {
                    .EventName = eventDecl.Identifier.ValueText,
                    .ModuleName = moduleStatement.Identifier.ValueText,
                    .EventType = If(eventDecl.AsClause IsNot Nothing,
                        eventDecl.AsClause.Type.ToString(), "EventHandler"),
                    .Parameters = parameters,
                    .RequiredNamespaces = requiredNamespaces,
                    .Location = moduleStatement.GetLocation()
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

    Private Shared Function GetEventParameters _
        (eventDecl As EventStatementSyntax, semanticModel As SemanticModel) As List(Of ParameterInfo)

        Dim parameters As New List(Of ParameterInfo)
        If eventDecl.ParameterList IsNot Nothing Then
            For Each paramSyntax In eventDecl.ParameterList.Parameters
                Dim paramName = paramSyntax.Identifier.Identifier.ValueText
                Dim paramTypeName = "Object"
                Dim containingNamespace = ""

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
                    .Name = paramName,
                    .Type = paramTypeName,
                    .ContainingNamespace = containingNamespace
                })
            Next paramSyntax
        End If

        Return parameters
    End Function

    Private Shared Function GenerateModuleRaiseMethods(moduleInfo As ModuleInfo) As String
        Dim code As New System.Text.StringBuilder

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
                Dim typeName = pInfo.Type
                For Each ns As String In namespaces
                    If pInfo.Type.StartsWith(ns) Then
                        typeName = pInfo.Type.Substring(ns.Length + 1)
                        Exit For
                    End If
                Next ns
                paramList.Add($"{pInfo.Name} As {typeName}")
            Next pInfo
            Dim params = String.Join(", ", paramList)

            ' Build argument list for RaiseEvent
            Dim argList As New List(Of String)
            For Each pInfo As ParameterInfo In evtInfo.Parameters
                argList.Add(pInfo.Name)
            Next pInfo
            Dim args = String.Join(", ", argList)

            ' Generate the raise method
            code.AppendLine($"    ''' <summary>")
            code.AppendLine($"    ''' Raises the {evtInfo.EventName} event.")
            code.AppendLine($"    ''' </summary>")

            ' Add parameter documentation
            For Each pInfo As ParameterInfo In evtInfo.Parameters
                code.AppendLine($"    ''' <param name=""{pInfo.Name}"">The {pInfo.Name} parameter.</param>")
            Next pInfo

            code.AppendLine($"    Public Sub RaiseEvent_{evtInfo.EventName}({params})")
            code.AppendLine($"        RaiseEvent {evtInfo.EventName}({args})")
            code.AppendLine($"    End Sub")
            code.AppendLine()
        Next evtInfo

        ' End module
        code.Append("End Module")

        Return code.ToString()
    End Function
End Class