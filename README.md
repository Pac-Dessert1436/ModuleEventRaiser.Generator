# `ModuleEventRaiser.Generator` - An Event Raiser Generator for VB.NET Modules

> **Version 1.2.5.1 (Latest)**: A reliability and performance release that adds missing namespace imports for guaranteed compilation on legacy projects and removes an unnecessary field from the scheduler. This version also introduces a `loggerAction` parameter to the `RaiseScheduledEvents` method, along with graceful exception handling/traceback.
>
> **v1.2.0**: ⚠️ **BREAKING CHANGE** - Unified event scheduler architecture! Each module now has an `EventScheduler` property instead of separate scheduler modules. Cleaner design, better encapsulation, same powerful functionality.

## Description
`ModuleEventRaiser.Generator` is a .NET source generator that automatically creates event raiser methods for events declared in VB.NET modules. It helps developers to raise events in a consistent, efficient, and well-documented manner, reducing boilerplate code and improving code readability. **Key points regarding memory management** with events are now included in this section: [Important Notes on This Package](#important-notes-on-this-package).

Currently available as a NuGet package: `dotnet add package ModuleEventRaiser.Generator --version 1.2.5.1`. **Enterprise-ready and fully compatible with `Option Infer Off`** - making it perfect for healthcare, financial, and other regulated industries with strict coding standards. _For more information on prior versions of this package, see [Version History](#version-history) at the end of this documentation._

## Important Notes on This Package
### ⚠️ Critical: Memory Management with Events
Events declared in VB.NET modules and raised via this package involve **standard .NET strong references**. When your event handlers subscribed in module events are linked to short-lived objects, keep in mind that **the event handlers MUST be always removed** the moment the short-lived objects are disposed:

```vb
Public Class MyApp
    Implements IDisposable

    Private _state As AppState
    Private _isDisposed As Boolean
    ' ... more fields & properties ...

    Public Sub New()
        AddHandler MyEvents.StateChanged, AddressOf OnStateChanged
    End Sub

    ' ... more instance/static methods ...

    Public Sub Dispose() Implements IDisposable.Dispose
        Dispose(disposing:=True)
        GC.SuppressFinalize(Me)
    End Sub

    Protected Overridable Sub Dispose(disposing As Boolean)
        If Not _isDisposed Then
            If disposing Then
                ' CRITICAL: Remove event handlers right here!
                RemoveHandler MyEvents.StateChanged, AddressOf OnStateChanged

                ' ... more managed resources disposed here ...
            End If
            ' Optional: Dispose unmanaged resources
            _isDisposed = True
        End If
    End Sub
End Class
```
**Why this matters**: The package does NOT contain methods that automatically remove event handlers. NEITHER would the native `Handles` keyword in VB.NET automatically do so. *__Without proper cleanup, especially when the event publishers are long-lived in your VB.NET app, the memory leak will definitely take place.__*

### Exception Handling in `RaiseScheduledEvents` Method (Available in v1.2.5.1)

Starting with v1.2.5.1, the `RaiseScheduledEvents` method (in the `ModuleEventScheduler` class) supports optional exception handling. You can pass a logger callback to catch and log any exceptions thrown during event processing. The method will continue raising remaining events even if one of them fails, ensuring that a single faulty handler does not block the rest. 

**Example usage:**
``` vb
EventScheduler.RaiseScheduledEvents(
    loggerAction:=Sub(ex As Exception) Debug.WriteLine($"Error raising event: {ex.Message}")
)
```

> **Note**: If `loggerAction` is not provided, the program will be `Stop`ped with error traceback. This feature was not available in previous versions.

### Other Notes
- The source generator only works with VB.NET modules and does not support classes or structures.
- The generator includes `Imports System` by default in generated files.
- Additional imports for custom types are now properly recognized - no other settings required.
  - e.g. `Public Event CollidePoint(rect As RectangleF, point As Vector2)` (in VB.NET MonoGame projects)
  - This will include `Imports Microsoft.Xna.Framework` in the generated source file.
- **For version 1.1.5**: It is recommended to use parameterized events (e.g., `Public Event MyEvent(sender As Object, e As EventArgs)`) for better clarity, though delegate pattern events (e.g., `Public Event MyEvent As EventHandler`) are still fully supported and useful, especially since `EventHandler` itself provides descriptive parameter naming.
- **For version 1.1.7+**: The following parameter names are reserved and should not be used when defining module events:
  - `withPriority`: Used for priority-based event scheduling
  - `withDelaySec`: Used for optional delay in async event raising
- **For version 1.2.0+**: The generated `ModuleEventScheduler` class is scoped to the current assembly's root namespace. Assemblies referencing each other do not share a single global instance. Each assembly gets its own independent `ModuleEventScheduler` class.
- **For version 1.2.5+**: The generated code now explicitly imports `System.Threading.Tasks` in addition to `System.Linq` introduced in v1.2.3. If you are targeting a legacy .NET framework or have removed these namespaces from your project-level imports, upgrade to 1.2.5 and reload your project to regenerate the source. Alternatively, add the following to your `.vbproj` file:
  ```xml
  <ItemGroup>
    <Import Include="System.Threading.Tasks" />
    <Import Include="System.Linq" />
  </ItemGroup>
  ```

## Key Features
- **Automatic Code Generation**: Generates event raiser methods for all events in VB.NET modules
- **Well-Documented**: Includes comprehensive XML documentation for all generated methods with `<see cref="EventName"/>` references and usage examples
- **Unified Event Scheduler (Version 1.2.0)**: Single shared `ModuleEventScheduler` class across all modules for consistent event scheduling
- **Event Accessibility Support (Version 1.2.0)**: Individual events now respect their declared accessibility levels (Public/Friend)
- **Module Accessibility Support (Version 1.1.8)**: Automatically detects and preserves module accessibility levels (Public/Friend)
- **Parameter Handling**: Correctly handles event parameters with proper types
- **Custom Event Types**: Supports both standard `EventHandler` and custom event types
- **Partial Modules**: Uses partial modules to seamlessly integrate with existing code
- **Incremental Generation**: Uses the latest incremental generator pattern for fast builds
- **Async Raiser Support**: Generates asynchronous `RaiseEventAsync_*` methods for all events
- **Event Scheduler Property (Version 1.2.0)**: Each module provides an `EventScheduler` property for accessing the unified scheduler
- **Automatic Namespace Detection**: Automatically detects and includes required namespaces for event parameter types
- **Delegate Pattern Support (Version 1.1.3+)**: Generates event raiser methods for events defined using delegate pattern (e.g. `Public Event MyEvent As EventHandler`)
- **Optional Delay for Async Events (Version 1.1.7)**: Allows specifying a delay in seconds when raising events asynchronously, useful for simulating real-world event timing (Note: Parameter name `withDelaySec` is reserved to avoid conflicts with other event parameters)
- **Priority-Based Event Scheduling (Version 1.1.7)**: Supports prioritizing scheduled events for more flexible event management, with higher priority events being raised first (Note: Parameter name `withPriority` is reserved to avoid conflicts with other event parameters)
- **Enhanced Parameter Validation (Version 1.1.8)**: Uses `NameOf(withDelaySec)` for user-friendly exception messages
- **Proper Module Scope (Version 1.1.8)**: Generated methods respect the original module's accessibility level
- **Weak Event Support (Version 1.2.2+)**: `WeakMulticastEvent` class prevents memory leaks in event-driven architectures while maintaining full compatibility with standard VB.NET event patterns
- **Stable FIFO Ordering (Version 1.2.3)**: Events with the same priority are raised in insertion order - no more non-deterministic ordering
- **Race-Condition-Safe Weak References (Version 1.2.3)**: Uses `WeakReference(Of T).TryGetTarget()` for reliable handler tracking, with `HandlerEntry` struct for accurate delegate matching
- **Explicit `System.Linq` Import (Version 1.2.3)**: Generated `ModuleEventScheduler` now explicitly imports `System.Linq` for consistent compilation across all project types
- **Critical `Friend Event` Handling Fix (Version 1.2.4)**: Fixed a hidden bug in the source generator's `Friend Event` handling path that could cause compilation errors and visibility mismatches for projects using `Friend Event` declarations. _Prior versions of the source generator incorrectly used `Internal` that doesn't exist in VB.NET._
- **Reliable Builds & Efficient Scheduler (Version 1.2.5.1)**: Added missing namespace imports for guaranteed compilation on legacy projects and removed the unnecessary `_nextOrder` field from `ModuleEventScheduler` to reduce memory allocations. This version also introduced a `loggerAction` parameter to the `RaiseScheduledEvents`, along with graceful exception handling/traceback without swallowing exceptions.

## Prerequisites
- [Visual Studio 2026](https://visualstudio.microsoft.com/vs/)
- [.NET SDK 6.0+](https://dotnet.microsoft.com/download/) (but **.NET 10** is recommended)
- VB.NET project targeting .NET Standard 2.0 or later

## Installation
1. **Clone the repository** and navigate to the project directory:
    ```bash
    git clone https://github.com/Pac-Dessert1436/ModuleEventRaiser.Generator.git
    cd ModuleEventRaiser.Generator
    ```

2. **Build the source generator project**:
    ```bash
    dotnet build
    ```

3. **Reference the source generator** in your VB.NET project by adding the following to your `.vbproj` file:
    ```xml
    <ItemGroup>
        <ProjectReference Include="..\ModuleEventRaiser.Generator\ModuleEventRaiser.Generator.vbproj"
            OutputItemType="Analyzer" ReferenceOutputAssembly="false" />
    </ItemGroup>
    ```
4. You can also **install the source generator via NuGet** - no manual configuration required:
   ```bash
   dotnet add package ModuleEventRaiser.Generator --version 1.2.5.1
   ```
   - Version 1.2.5.1 adds missing namespace imports for guaranteed compilation on legacy projects and removes the unnecessary `_nextOrder` field from the scheduler. _The previous version (1.2.5) has been **deprecated** for swallowing exceptions in the `RaiseScheduledEvents` method of the `ModuleEventScheduler` class._

## Example Usage

### Input: VB.NET Module with Events

#### Basic Single Namespace Usage
```vb
Partial Public Module MyEvents
    ' Standard event pattern (parameterized)
    Public Event TemperatureChanged(temperature As Double)
    Public Event HumidityChanged(humidity As Double)
    Public Event LightLevelChanged(lightLevel As Integer)
    
    ' Delegate pattern (using explicit delegate types)
    Public Event MyEvent As EventHandler
    Public Event CustomEvent As Action(Of String, Integer)
    Public Event GameEvent As Action(Of GameState)
End Module
```

#### Multi-Namespace Support (Actually introduced in 1.1.7.9+)
Define event modules in different namespaces for better organization. Note that the namespace declarations are naturally on top of the project's root namespace:

**GameEvents.vb** (in `{RootNamespace}.Events` namespace):
```vb
Namespace Events
    Partial Public Module GameEvents
        Public Event PlayerDied(playerId As Integer)
        Public Event ScoreUpdated(newScore As Integer)
        Public Event LevelCompleted(levelId As Integer)
    End Module
End Namespace
```

**UIEvents.vb** (in `{RootNamespace}.UI.Events` namespace):
```vb
Namespace UI.Events
    Partial Public Module UIEvents
        Public Event ButtonClicked(buttonName As String)
        Public Event MenuOpened(menuId As Integer)
        Public Event DialogClosed(dialogId As Integer)
    End Module
End Namespace
```

**AudioEvents.vb** (in `{RootNamespace}.Audio.Events` namespace):
```vb
Namespace Audio.Events
    Partial Public Module AudioEvents
        Public Event SoundPlayed(soundId As Integer)
        Public Event MusicChanged(trackId As Integer)
        Public Event VolumeChanged(newVolume As Double)
    End Module
End Namespace
```

### Output: Generated Event Raiser Methods

**Documentation follows the same pattern**:
- `RaiseEvent_*` methods: Raises the * event (direct invocation).
> NOTE: The above method is the simplest way to raise events, but synchronous.
- `RaiseEventAsync_*` methods: Asynchronously raises the * event. Use this method only in desktop apps, networking, etc. DO NOT USE THIS METHOD WHEN WRITING GAME LOGIC IN GAME FRAMEWORKS (MonoGame, FNA, etc.).
- `ScheduleEvent_*` methods: Schedules the * event to be raised later. Useful for game frameworks (MonoGame, FNA, etc.).

**Key Features in Generated Code**:
- **`Option Infer Off` Compatibility**: All generated code uses explicit type declarations
- **Multi-Namespace Support**: Automatic namespace handling for organized project structures
- **Enhanced Parameter Documentation**: Improved XML documentation with descriptive parameter names
- **Priority-Based Scheduling**: Control event execution order with priority values
- **Optional Async Delays**: Add delays to async event raising for timing control
- **Event Accessibility**: Generated methods respect individual event accessibility (Public/Friend)
- **Unified EventScheduler Property**: Each module provides access to the shared `ModuleEventScheduler` class

```vb
' <auto-generated>
'     This code was generated by `ModuleEventRaiser.Generator`.
'     Changes to this file may cause incorrect behavior and will be lost if
'     the code is regenerated.
' </auto-generated>

Option Explicit On
Option Strict On

Imports System
' NOTE: Additional imports for custom types will be automatically added here.

Partial Public Module MyEvents

    ''' <summary>
    ''' Provides access to the unified event scheduler for this module.
    ''' </summary>
    ''' <value>A shared instance of <see cref="ModuleEventScheduler"/> for scheduling and raising events.</value>
    ''' <remarks>
    ''' <para>
    ''' This property provides a thread-safe mechanism to schedule events to be raised later,
    ''' which is particularly useful in game development frameworks (MonoGame, FNA, Unity, etc.)
    ''' where raising events during the update phase can cause performance issues.
    ''' </para>
    ''' <para>
    ''' <b>Usage Example:</b>
    ''' <code lang="vb">
    ''' ' Schedule an event to be raised later
    ''' EventScheduler.ScheduleEventAction(Sub() RaiseEvent MyEvent(arg1, arg2))
    '''
    ''' ' Later, typically in the Draw phase, raise all scheduled events:
    ''' EventScheduler.RaiseScheduledEvents()
    ''' </code>
    ''' </para>
    ''' <para>
    ''' For more information about event scheduling, see the <see cref="ModuleEventScheduler"/> class.
    ''' </para>
    ''' </remarks>
    Public ReadOnly Property EventScheduler As New ModuleEventScheduler

    Public Sub RaiseEvent_TemperatureChanged(temperature As Double)
        RaiseEvent TemperatureChanged(temperature)
    End Sub

    Public Async Function RaiseEventAsync_TemperatureChanged(temperature As Double, Optional withDelaySec As Double = 0) As Task
        If withDelaySec < 0 Then Throw New ArgumentOutOfRangeException(NameOf(withDelaySec), "Delay seconds must be non-negative.")
        If withDelaySec > 0 Then Await Task.Delay(TimeSpan.FromSeconds(withDelaySec))
        Await Task.Run(Sub() RaiseEvent TemperatureChanged(temperature))
    End Function

    Public Sub ScheduleEvent_TemperatureChanged(temperature As Double, Optional withPriority As Integer = 0)
        MyEvents.EventScheduler.ScheduleEventAction(Sub() RaiseEvent TemperatureChanged(temperature), withPriority)
    End Sub

    Public Sub RaiseEvent_HumidityChanged(humidity As Double)
        RaiseEvent HumidityChanged(humidity)
    End Sub

    Public Async Function RaiseEventAsync_HumidityChanged(humidity As Double, Optional withDelaySec As Double = 0) As Task
        If withDelaySec < 0 Then Throw New ArgumentOutOfRangeException(NameOf(withDelaySec), "Delay seconds must be non-negative.")
        If withDelaySec > 0 Then Await Task.Delay(TimeSpan.FromSeconds(withDelaySec))
        Await Task.Run(Sub() RaiseEvent HumidityChanged(humidity))
    End Function

    Public Sub ScheduleEvent_HumidityChanged(humidity As Double, Optional withPriority As Integer = 0)
        MyEvents.EventScheduler.ScheduleEventAction(Sub() RaiseEvent HumidityChanged(humidity), withPriority)
    End Sub

    Public Sub RaiseEvent_LightLevelChanged(lightLevel As Integer)
        RaiseEvent LightLevelChanged(lightLevel)
    End Sub

    Public Async Function RaiseEventAsync_LightLevelChanged(lightLevel As Integer, Optional withDelaySec As Double = 0) As Task
        If withDelaySec < 0 Then Throw New ArgumentOutOfRangeException(NameOf(withDelaySec), "Delay seconds must be non-negative.")
        If withDelaySec > 0 Then Await Task.Delay(TimeSpan.FromSeconds(withDelaySec))
        Await Task.Run(Sub() RaiseEvent LightLevelChanged(lightLevel))
    End Function

    Public Sub ScheduleEvent_LightLevelChanged(lightLevel As Integer, Optional withPriority As Integer = 0)
        MyEvents.EventScheduler.ScheduleEventAction(Sub() RaiseEvent LightLevelChanged(lightLevel), withPriority)
    End Sub

    ' NEW in 1.1.3: Delegate pattern event raising methods (documentation follows the same pattern)
    Public Sub RaiseEvent_MyEvent(sender As Object, e As EventArgs)
        RaiseEvent MyEvent(sender, e)
    End Sub

    Public Async Function RaiseEventAsync_MyEvent(sender As Object, e As EventArgs, Optional withDelaySec As Double = 0) As Task
        If withDelaySec < 0 Then Throw New ArgumentOutOfRangeException(NameOf(withDelaySec), "Delay seconds must be non-negative.")
        If withDelaySec > 0 Then Await Task.Delay(TimeSpan.FromSeconds(withDelaySec))
        Await Task.Run(Sub() RaiseEvent MyEvent(sender, e))
    End Sub

    Public Sub ScheduleEvent_MyEvent(sender As Object, e As EventArgs, Optional withPriority As Integer = 0)
        MyEvents.EventScheduler.ScheduleEventAction(Sub() RaiseEvent MyEvent(sender, e), withPriority)
    End Sub

    ' ... More delegate pattern event raising methods ...
End Module
```

### Unified event scheduler class in `ModuleEventScheduler.vb` (Enhanced in v1.2.5.1)
```vb
' <auto-generated>
'     This code was generated by `ModuleEventRaiser.Generator`.
'     Changes to this file may cause incorrect behavior and will be lost if
'     the code is regenerated.
' </auto-generated>

Option Explicit On
Option Strict On
Imports System
Imports System.Collections.Generic
Imports System.Linq
Imports System.Diagnostics

''' <summary>
''' Provides a unified event scheduling mechanism for modules, allowing events to be deferred until a later time.
''' </summary>
''' <remarks>
''' <para>
''' This class is especially useful in game development frameworks such as MonoGame, FNA, and Unity,
''' where raising events during the update phase can lead to performance issues or race conditions.
''' By scheduling events for later execution, typically during the draw phase, you can keep frame rates
''' stable and make event handling more predictable.
''' </para>
''' <para>
''' <b>Priority System:</b> Events can be scheduled with different priority values. Higher-priority
''' events are raised first. When multiple events share the same priority, they are raised in
''' first-in, first-out (FIFO) order.
''' </para>
''' <para>
''' <b>Thread Safety:</b> All methods in this class are thread-safe and may be called from any thread.
''' </para>
''' <para>
''' <b>Usage Example:</b>
''' <code lang=""vb"">
''' ' Schedule an event with the default priority by wrapping a RaiseEvent call
''' EventScheduler.ScheduleEventAction(
'''     Sub()
'''         Debug.WriteLine($""[ModuleEventScheduler] MyEvent raised with args: {{arg1}}, {{arg2}}"")
'''         RaiseEvent_MyEvent(arg1, arg2)
'''     End Sub)
'''
''' ' Schedule a high-priority event in the same way
''' EventScheduler.ScheduleEventAction(
'''     Sub()
'''         Debug.WriteLine($""[ModuleEventScheduler] CriticalEvent raised with data: {{data}}"")
'''         RaiseEvent_CriticalEvent(data)
'''     End Sub, priorityValue:=10)
'''
''' ' Later, typically during the Draw phase in the game framework:
''' EventScheduler.RaiseScheduledEvents()
''' </code>
''' </para>
''' </remarks>
Public NotInheritable Class ModuleEventScheduler
    Private Structure EventItem
        Public ReadOnly [Event] As Action
        Public ReadOnly Priority As Integer
        Public ReadOnly Order As Integer

        Public Sub New([event] As Action, priority As Integer, order As Integer)
            Me.Event = [event]
            Me.Priority = priority
            Me.Order = order
        End Sub
    End Structure

    Private ReadOnly _pendingEvents As New List(Of EventItem)
    Private ReadOnly _lock As New Object

    ''' <summary>
    ''' Schedules an event action for later execution with an optional priority value.
    ''' </summary>
    ''' <param name=""eventAction"">The event action to schedule. This is typically a lambda that raises an event.</param>
    ''' <param name=""priorityValue"">The priority value for the event (default value: 0).
    ''' Higher-priority events are raised first. When multiple events share the same priority,
    ''' they are raised in FIFO order.</param>
    ''' <exception cref=""ArgumentNullException"">Thrown when <paramref name=""eventAction""/> is null.</exception>
    ''' <remarks>
    ''' This method is thread-safe and may be called from any thread. The scheduled event will be
    ''' raised when <see cref=""RaiseScheduledEvents""/> is invoked.
    ''' </remarks>
    Public Sub ScheduleEventAction(eventAction As Action, Optional priorityValue As Integer = 0)
        ArgumentNullException.ThrowIfNull(eventAction)
        SyncLock _lock
            _pendingEvents.Add(New EventItem(eventAction, priorityValue, _pendingEvents.Count))
        End SyncLock
    End Sub

    ''' <summary>
    ''' Raises all scheduled event actions in priority order. Exceptions will be logged or traced if any event action fails.
    ''' </summary>
    ''' <param name=""loggerAction"">An optional action to log exceptions that occur while raising events.</param>
    ''' <remarks>
    ''' <para>
    ''' This method is thread-safe and should be called during a phase where event handling is safe,
    ''' such as during the 'Draw' phase in game frameworks.
    ''' </para>
    ''' <para>
    ''' Events are raised in the following order:
    ''' <list type=""number"">
    ''' <item><description>Events with higher priority values are raised first</description></item>
    ''' <item><description>Events within the same priority level are raised in FIFO order</description></item>
    ''' </list>
    ''' </para>
    ''' <para>
    ''' After raising all events, the pending events queue is cleared. This method can be called
    ''' multiple times; each call will raise all events that were scheduled since the last call.
    ''' </para>
    ''' </remarks>
    Public Sub RaiseScheduledEvents(Optional loggerAction As Action(Of Exception) = Nothing)
        Dim actionsToRaise As Action() = Array.Empty(Of Action)()
        SyncLock _lock
            If _pendingEvents.Count = 0 Then Exit Sub
            actionsToRaise = Aggregate evt As EventItem In _pendingEvents
                             Order By evt.Priority Descending, evt.Order Ascending
                             Select evt.Event Into ToArray()
            _pendingEvents.Clear()
        End SyncLock

        ' Raise all events outside the lock with exception handling/logging.
        ' If `loggerAction` is provided, use it to log exceptions.
        ' Otherwise, trace the error and stop the process if attached.
        For Each atn As Action In actionsToRaise
            Try
                atn.Invoke()
            Catch ex As Exception
                If loggerAction IsNot Nothing Then
                    loggerAction.Invoke(ex)
                Else
                    Trace.TraceError(ex.ToString())
                    If Debugger.IsAttached Then Stop
                End If
            End Try
        Next atn
    End Sub

    ''' <summary>
    ''' Gets the number of events that are currently pending execution.
    ''' </summary>
    ''' <value>The number of pending events.</value>
    ''' <remarks>
    ''' This property is thread-safe and may be called from any thread. It can be useful for debugging
    ''' or for implementing logic that depends on the current number of pending events.
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
    ''' This method is thread-safe and may be called from any thread. Use it when you need to cancel
    ''' all pending events, such as during scene transitions or when resetting game state. After calling
    ''' this method, <see cref=""PendingEventCount""/> will be zero.
    ''' </remarks>
    Public Sub ClearScheduledEvents()
        SyncLock _lock
            _pendingEvents.Clear()
        End SyncLock
    End Sub
End Class
```

### How to Use the Generated Methods

#### Basic Synchronous Event Raising
```vb
' In another part of your code (e.g. a class that needs to raise events)
Private Sub UpdateEnvironmentalData()
    Dim newTemp As Double = 25.5
    Dim newHumidity As Double = 75.3
    Dim newLightLevel As Integer = 80
    
    ' Use the generated methods to raise events
    ' NOTE: Module names in VB.NET can be usually omitted
    RaiseEvent_TemperatureChanged(newTemp)
    RaiseEvent_HumidityChanged(newHumidity)
    RaiseEvent_LightLevelChanged(newLightLevel)
End Sub
```

#### Asynchronous Event Raising
```vb
' Using async/await pattern
Private Async Function UpdateEnvironmentalDataAsync() As Task
    Dim newTemp As Double = 25.5
    Dim newHumidity As Double = 75.3
    Dim newLightLevel As Integer = 80
    
    ' Use the generated async methods
    Await RaiseEventAsync_TemperatureChanged(newTemp)
    Await RaiseEventAsync_HumidityChanged(newHumidity)
    Await RaiseEventAsync_LightLevelChanged(newLightLevel)
End Function
```

#### Scheduled Event Raising (for Game Frameworks)
```vb
' In a game framework like MonoGame or FNA
Private Sub Update(gameTime As GameTime)
    ' Game logic that determines when to raise events
    Dim playerScore As Integer = CalculatePlayerScore()
    Dim enemyCount As Integer = GetEnemyCount()
    
    ' Schedule events to be raised later
    ' These will be queued and can be raised during the render phase
    ScheduleEvent_ScoreChanged(playerScore)
    ScheduleEvent_EnemyCountChanged(enemyCount)
End Sub

' In your game's Draw method or main loop
Private Sub Draw(gameTime As GameTime)
    ' Raise all scheduled events before rendering
    MyEvents.EventScheduler.RaiseScheduledEvents()
    
    ' Render game graphics
    ' ...
End Sub
```

## Technical Details

### How It Works
1. **Syntax Analysis**: The generator analyzes your VB.NET code to find events declared within modules
2. **Event Information Collection**: It collects details about each event, including its name, parameters, and containing module
3. **Namespace Detection**: It automatically detects and collects required namespaces for event parameter types
4. **Code Generation**: For each event, it generates:
   - A synchronous `RaiseEvent_*` method
   - An asynchronous `RaiseEventAsync_*` method
   - A scheduled `ScheduleEvent_*` method
5. **Event Scheduler Generation**: It creates a unified `ModuleEventScheduler` class shared across all modules
6. **Output**: The generated code is written to separate files named `{ModuleName}_EventRaisers.g.vb`

### Supported Event Patterns
The generator supports:
- Events with standard `EventHandler` type
- Events with custom delegate types
- Events with any number of parameters
- Events with different parameter types
- Events with custom types from external libraries (e.g., MonoGame, FNA)

### Generated Code Features
- **XML Documentation**: Each generated method includes summary and parameter documentation
- **Option Statements**: Includes `Option Explicit On` and `Option Strict On`
- **Auto-Generated Header**: Clearly marks generated code with a header
- **Automatic Imports**: Automatically includes `Imports System` and any required namespaces for event parameter types
- **Synchronous Methods**: Standard `RaiseEvent_*` methods for immediate event raising
- **Asynchronous Methods**: `RaiseEventAsync_*` methods for async event raising with optional delay
- **Scheduled Methods**: `ScheduleEvent_*` methods for deferred event raising with priority support (ideal for game frameworks)
- **Event Scheduler Property**: Each module provides an `EventScheduler` property for accessing the unified `ModuleEventScheduler` class
- **Well-Formatted Code**: Proper indentation and spacing for readability

### The Event Scheduler
The generated `ModuleEventScheduler` class provides a thread-safe way to schedule events to be raised later, which is particularly useful in game frameworks like MonoGame and FNA where you want to avoid raising events during the update phase.

#### Key Features
- **Thread-Safe Operation**: Uses synchronization to ensure thread safety
- **Event Queuing**: Queues events to be raised later
- **Batch Processing**: Raises all scheduled events at once

#### Usage Example (Game Framework)
```vb
' In your game's Update method
Private Sub Update(gameTime As GameTime)
    ' Game logic that determines when to raise events
    If playerScoreChanged Then
        ScheduleEvent_ScoreChanged(newScore)
    End If
    
    If enemyCountChanged Then
        ScheduleEvent_EnemyCountChanged(newEnemyCount)
    End If
End Sub

' In your game's Draw method
Private Sub Draw(gameTime As GameTime)
    ' Raise all scheduled events before rendering
    MyEvents.EventScheduler.RaiseScheduledEvents()
    
    ' Render game graphics
    ' ...
End Sub
```

## Benefits with this Package
- **Reduced Boilerplate Code**: No need to manually write event raiser methods
- **Consistent Pattern**: All event raisers follow the same pattern
- **Improved Readability**: Clear, well-documented raise methods
- **Fewer Errors**: Eliminates typos and parameter mismatches
- **Maintenance Friendly**: Automatically updates when events change
- **Automatic Namespace Management**: No need to manually add imports for custom types
- **Versatile Event Raising**: Choose between synchronous, asynchronous, or scheduled event raising
- **Flexible Async Options**: Optional delay parameter for async event raising
- **Priority-Based Scheduling**: Control the order of event execution with priority values
- **Game Framework Compatibility**: Scheduled event raising with priorities is ideal for game frameworks like MonoGame and FNA
- **Thread-Safe Operation**: The event scheduler uses synchronization to ensure thread safety

## Migration Guide: Version 1.1.x → 1.2.0

### The Breaking Change
Version 1.2.0 introduces a unified event scheduler that replaces the previous per-module scheduler approach. This is a minor breaking change that requires simple code updates.

### What Changed
- **Before (1.1.x)**: Each module had its own `{ModuleName}EventScheduler` module
- **After (1.2.0)**: All modules share a single `ModuleEventScheduler` class, accessed via the `EventScheduler` property on each module

### Required Code Changes

#### 1. Update Scheduler Access
Replace all references to `{ModuleName}EventScheduler` with `{ModuleName}.EventScheduler`:

**Before (1.1.x):**
```vb
MyEventsEventScheduler.RaiseScheduledEvents()
MyEventsEventScheduler.PendingEventCount
MyEventsEventScheduler.ClearScheduledEvents()
```

**After (1.2.0):**
```vb
MyEvents.EventScheduler.RaiseScheduledEvents()
MyEvents.EventScheduler.PendingEventCount
MyEvents.EventScheduler.ClearScheduledEvents()
```

#### 2. Update Direct Scheduler Method Calls
If you were calling scheduler methods directly:

**Before (1.1.x):**
```vb
MyEventsEventScheduler.ScheduleEventAction(Sub() RaiseEvent MyEvent(arg1, arg2), priorityValue:=10)
```

**After (1.2.0):**
```vb
MyEvents.EventScheduler.ScheduleEventAction(Sub() RaiseEvent MyEvent(arg1, arg2), priorityValue:=10)
```

#### 3. No Changes Needed For
- `RaiseEvent_*` methods - unchanged
- `RaiseEventAsync_*` methods - unchanged
- `ScheduleEvent_*` methods - unchanged (they internally use the new scheduler)
- Event declarations - unchanged
- Event handlers - unchanged

### Benefits of the Change
- **Unified Scheduler**: All modules share the same scheduler, making event management more consistent
- **Better Resource Usage**: Single scheduler instance instead of multiple per-module schedulers
- **Simplified API**: Access scheduler through module property instead of separate module
- **Enhanced Documentation**: Improved XML documentation with usage examples
- **Event Accessibility**: Individual events now respect their declared accessibility levels

### Example Migration
**Before (1.1.x):**
```vb
' In your game's Draw method
Private Sub Draw(gameTime As GameTime)
    ' Raise all scheduled events from different modules
    GameEventsEventScheduler.RaiseScheduledEvents()
    UIEventsEventScheduler.RaiseScheduledEvents()
    AudioEventsEventScheduler.RaiseScheduledEvents()
    
    ' Render game graphics
    ' ...
End Sub
```

**After (1.2.0):**
```vb
' In your game's Draw method
Private Sub Draw(gameTime As GameTime)
    ' Raise all scheduled events from different modules
    GameEvents.EventScheduler.RaiseScheduledEvents()
    UIEvents.EventScheduler.RaiseScheduledEvents()
    AudioEvents.EventScheduler.RaiseScheduledEvents()
    
    ' Render game graphics
    ' ...
End Sub
```

### Automated Migration
You can use Find and Replace in Visual Studio to update your code:
- Find: `{ModuleName}EventScheduler`
- Replace: `{ModuleName}.EventScheduler`

Replace `{ModuleName}` with your actual module names (e.g., `MyEvents`, `GameEvents`, etc.).

## Version History
**⚠️ Breaking Change in version 1.2.0**:
- **Scheduler Access**: Previously, each module had its own `{ModuleName}EventScheduler` module. Now, all modules share a single `ModuleEventScheduler` class, accessed via the `EventScheduler` property on each module.
- **Code Migration**: Replace `{ModuleName}EventScheduler.ScheduleEventAction(...)` with `{ModuleName}.EventScheduler.ScheduleEventAction(...)` (see [Migration Guide](#migration-guide-version-11x--120) at the bottom of the documentation)

**New in version 1.2.3**:
- **Stable FIFO ordering within same priority** - Events scheduled with equal priority values now reliably execute in the order they were added (previously relied on LINQ's unstable sort)
- **`ModuleEventScheduler` data structure refinement** - Switched from `Queue(Of EventItem)` to `List(Of EventItem)` with a monotonic `Order` field for semantically correct priority-plus-order sorting
- **Explicit `System.Linq` import** - The generated `ModuleEventScheduler` now explicitly imports the `System.Linq` namespace for consistent compilation across all project types
- **Race-condition-safe weak references in `WeakMulticastEvent`** - Replaced the old non-generic `WeakReference` with `WeakReference(Of TDelegate)`, using `TryGetTarget()` instead of the broken `IsAlive` → `Target` pattern
- **NullReferenceException fix in `WeakMulticastEvent.RemoveHandler`** - Now uses the `HandlerEntry` struct with `MethodInfo` + target tracking for reliable matching
- **`ActiveHandlerCount` no longer mutates state** - Property getter previously called `RemoveAll()` inside the lock; now purely counts live handlers without side effects
- **`RemoveDeadHandlers()` public method** - Explicit API for scavenging handlers whose targets have been garbage collected
- **Null guard on `RemoveHandler(Nothing)`** - No longer throws unexpectedly when called with a null handler
- **Better delegate matching** - The new `HandlerEntry` struct compares both `MethodInfo` and target object for accurate handler removal

**New in version 1.2.5.1**:
- **Added missing namespace imports** - Ensures generated code compiles even when `System.Threading.Tasks` is not explicitly imported. If you are targeting a legacy framework or have removed `System.Linq` and `System.Threading.Tasks` from your project-level imports, upgrade to 1.2.5 and reload your project to regenerate the source.
  > **Note**: If you cannot upgrade to 1.2.5, configure your project-level imports on your legacy VB.NET projects:
  > ```xml
  > <ItemGroup>
  >   <Import Include="System.Threading.Tasks" />
  >   <Import Include="System.Linq" />
  > </ItemGroup>
  > ```
- **Removed the `_nextOrder` field from `ModuleEventScheduler`** - This field was unnecessary and could increase memory usage. The scheduler now preserves ordering using the current `_pendingEvents.Count`, reducing allocations while keeping stable priority-based execution.
- **More robust error handling in `ModuleEventScheduler`** - Improved exception logging and tracing in the `RaiseScheduledEvents` method to help diagnose and resolve issues more effectively. _The previous version (1.2.5) has been deprecated, because `loggerAction?.Invoke(ex)` can swallow exceptions silently when `loggerAction` is not provided._

**New in version 1.2.4**:
- **Critical `Friend Event` handling bug fix** - Fixed a hidden bug in the source generator's `Friend Event` handling path that could cause compilation errors and visibility mismatches for projects using `Friend Event` declarations

**Existing features from version 1.1.x**: 
- **Module Accessibility Support**: Automatically detects and preserves module accessibility levels (Public/Friend)
- **Priority-based event scheduling** - control the order events are raised with priority values
- **Enhanced asynchronous methods** - add optional delays to async event raising
- **Improved parameter documentation** - better XML documentation for generated methods
- **Delegate pattern detection** - supports both traditional parameter lists and delegate-based events like `As EventHandler`
- **Multiple event module support** - resolves ambiguity in method calls and supports multiple event modules like `GameEvents`, `UIEvents`, `AudioEvents` and more
- **Improved Parameter Validation** - uses `NameOf(withDelaySec)` for user-friendly exception messages

## License
This project is licensed under the BSD 3-Clause License. See the [LICENSE](LICENSE) file for details.
