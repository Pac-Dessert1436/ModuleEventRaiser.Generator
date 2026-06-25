# `ModuleEventRaiser.Generator` - An Event Raiser Generator for VB.NET Modules

> **Version 1.2.1 (Latest)**: Minor breaking change with unified event scheduler and enhanced features:
> - _**Unified Event Scheduler**: Single shared scheduler class across all modules (breaking change)_
> - **Event Accessibility Support**: Individual events now respect their declared accessibility (Public, Friend or Private)
> - **Enhanced XML Documentation**: Comprehensive documentation with usage examples
> - **Improved namespace wrapping** with module accessibility support (Public/Friend)
> - **Note on Namespace Isolation**: When assemblies do not share a common root namespace, each assembly gets its own independent `ModuleEventScheduler` class. _No need to worry about namespace conflicts when working with multiple assemblies._ See [Important Notes on this Package → Other Notes](#other-notes) for more details.

| Version | Status |
|---------|--------|
| 1.2.1 | ✅ Latest with unified scheduler and event accessibility |
| 1.2.0 | ✅ Unified scheduler and event accessibility |
| 1.1.8 | ✅ Fully functional with per-module schedulers |
| 1.1.7.10 | ✅ Legacy .NET support with traditional if-check |
| 1.1.7.9 | ✅ Namespace support works |
| 1.1.7.5 | ❌ Broken (deprecated, unlisted) |

## Description
`ModuleEventRaiser.Generator` is a .NET source generator that automatically creates event raiser methods for events declared in VB.NET modules. It helps developers to raise events in a consistent, efficient, and well-documented manner, reducing boilerplate code and improving code readability. **Key points regarding memory management** with events are now included in this section: [Important Notes on this Package](#important-notes-on-this-package)

Currently available as a NuGet package: `dotnet add package ModuleEventRaiser.Generator --version 1.2.1`. **Enterprise-ready and fully compatible with `Option Infer Off`** - making it perfect for healthcare, financial, and other regulated industries with strict coding standards.

> **v1.2.1 Latest Update**: Cosmetic improvements to the generated `ModuleEventScheduler` code — explicit `Imports` statements and `ReadOnly` structure fields for better immutability.
>
> **v1.2.0**: Breaking change with **unified event scheduler** and **event accessibility support**. This version introduces a single shared `ModuleEventScheduler` class across all modules, replacing the previous per-module scheduler approach. Individual events now respect their declared accessibility levels (Public, Friend or Private).

**New in version 1.2.0**: 
- **Unified `ModuleEventScheduler`**: Single shared scheduler class across all modules (breaking change from per-module schedulers)
- **Event Accessibility Support**: Individual events now respect their declared accessibility (Public/Friend)
- **Enhanced XML Documentation**: Comprehensive documentation with usage examples for all generated members
- **`EventScheduler` Property**: Each module now has an `EventScheduler` property providing access to the unified scheduler

**Breaking Change in version 1.2.0**:
- **Scheduler Access**: Previously, each module had its own `{ModuleName}EventScheduler` module. Now, all modules share a single `ModuleEventScheduler` class, accessed via the `EventScheduler` property on each module.
- **Code Migration**: Replace `{ModuleName}EventScheduler.ScheduleEventAction(...)` with `{ModuleName}.EventScheduler.ScheduleEventAction(...)` (see [Migration Guide](#migration-guide-version-11x--120) at the bottom of the documentation)

**Existing features from version 1.1.x**: 
- **Module Accessibility Support**: Automatically detects and preserves module accessibility levels (Public/Friend)
- **Priority-based event scheduling** - control the order events are raised with priority values
- **Enhanced asynchronous methods** - add optional delays to async event raising
- **Improved parameter documentation** - better XML documentation for generated methods
- **Delegate pattern detection** - supports both traditional parameter lists and delegate-based events like `As EventHandler`
- **Multiple event module support** - resolves ambiguity in method calls and supports multiple event modules like `GameEvents`, `UIEvents`, `AudioEvents` and more
- **Improved Parameter Validation** - uses `NameOf(withDelaySec)` for user-friendly exception messages

## Important Notes on this Package
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

## Prerequisites
- [Visual Studio 2026](https://visualstudio.microsoft.com/vs/)
- [.NET SDK 6.0+](https://dotnet.microsoft.com/download/)
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
   dotnet add package ModuleEventRaiser.Generator --version 1.2.1
   ```
   - Version 1.2.1 introduces cosmetic improvements to the generated `ModuleEventScheduler` code. Version 1.2.0 introduced **unified event scheduler** and **event accessibility support**, with enhanced XML documentation and comprehensive usage examples.

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

### New in 1.2.0: Unified event scheduler class in `ModuleEventScheduler.vb`
```vb
Option Explicit On
Option Strict On

Imports System
Imports System.Collections.Generic

''' <summary>
''' Provides a unified event scheduling mechanism for modules, enabling deferred event execution.
''' </summary>
''' <remarks>
''' <para>
''' This class is particularly useful in game development frameworks (MonoGame, FNA, Unity, etc.) 
''' where raising events during the update phase can cause performance issues or race conditions.
''' By scheduling events to be raised later (typically during the draw phase), you can maintain
''' consistent frame rates and ensure thread-safe event handling.
''' </para>
''' <para>
''' <b>Priority System:</b> Events can be scheduled with different priority values. Higher priority
''' events are raised first. Events with the same priority are raised in first-in-first-out (FIFO) order.
''' </para>
''' <para>
''' <b>Thread Safety:</b> All methods in this class are thread-safe and can be called from any thread.
''' </para>
''' <para>
''' <b>Usage Example:</b>
''' <code lang="vb">
''' ' Schedule an event with default priority, using a wrapped RaiseEvent method
''' EventScheduler.ScheduleEventAction(
'''     Sub() 
'''         Debug.WriteLine($""[ModuleEventScheduler] MyEvent raised with args: {{arg1}}, {{arg2}}"")
'''         RaiseEvent_MyEvent(arg1, arg2)
'''     End Sub)
''' ' Schedule a high-priority event with similar logic
''' EventScheduler.ScheduleEventAction(
'''     Sub() 
'''         Debug.WriteLine($""[ModuleEventScheduler] CriticalEvent raised with data: {{data}}"")
'''         RaiseEvent_CriticalEvent(data)
'''     End Sub, priorityValue:=10)
''' ' Later, typically in the `Draw` phase within the game framework:
''' EventScheduler.RaiseScheduledEvents()
''' </code>
''' </para>
''' </remarks>
Public NotInheritable Class ModuleEventScheduler
    Private Structure EventItem
        Public ReadOnly [Event] As Action
        Public ReadOnly Priority As Integer

        Public Sub New([event] As Action, priority As Integer)
            Me.Event = [event]
            Me.Priority = priority
        End Sub
    End Structure

    Private ReadOnly _pendingEvents As New Queue(Of EventItem)
    Private ReadOnly _lock As New Object

    ''' <summary>
    ''' Schedules an event action to be raised later with an optional priority value.
    ''' </summary>
    ''' <param name="eventAction">The event action to schedule. This is typically a lambda that raises an event.</param>
    ''' <param name="priorityValue">The priority value of the event (default is 0).
    ''' Events with higher priority values are raised first. Events with the same priority are raised in FIFO order.</param>
    ''' <exception cref="ArgumentNullException">Thrown when <paramref name="eventAction"/> is null.</exception>
    ''' <remarks>
    ''' This method is thread-safe and can be called from any thread. The scheduled event will be
    ''' raised when <see cref="RaiseScheduledEvents"/> is called.
    ''' </remarks>
    Public Sub ScheduleEventAction(eventAction As Action, Optional priorityValue As Integer = 0)
        ArgumentNullException.ThrowIfNull(eventAction)
        SyncLock _lock
            _pendingEvents.Enqueue(New EventItem(eventAction, priorityValue))
        End SyncLock
    End Sub

    ''' <summary>
    ''' Raises all scheduled event actions in priority order.
    ''' </summary>
    ''' <remarks>
    ''' <para>
    ''' This method is thread-safe and should be called during a phase where event handling is safe,
    ''' such as during the 'Draw' phase in game frameworks.
    ''' </para>
    ''' <para>
    ''' Events are raised in the following order:
    ''' <list type="number">
    ''' <item><description>Events with higher priority values are raised first</description></item>
    ''' <item><description>Events within the same priority level are raised in FIFO order</description></item>
    ''' </list>
    ''' </para>
    ''' <para>
    ''' After raising all events, the pending events queue is cleared. This method can be called
    ''' multiple times; each call will raise all events that were scheduled since the last call.
    ''' </para>
    ''' </remarks>
    Public Sub RaiseScheduledEvents()
        Dim actionsToRaise As Action() = Array.Empty(Of Action)()
        SyncLock _lock
            If _pendingEvents.Count = 0 Then Exit Sub
            actionsToRaise = Aggregate e In _pendingEvents Order By e.Priority Descending
                                 Select e.Event Into ToArray()
            _pendingEvents.Clear()
        End SyncLock

        ' Raise all events outside the lock to avoid deadlocks
        Array.ForEach(actionsToRaise, Sub(atn) atn.Invoke())
    End Sub

    ''' <summary>
    ''' Gets the number of pending events currently scheduled to be raised.
    ''' </summary>
    ''' <value>The number of pending events.</value>
    ''' <remarks>
    ''' This property is thread-safe and can be called from any thread. It can be useful for
    ''' debugging or for implementing logic that depends on the number of pending events.
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
    ''' This method is thread-safe and can be called from any thread. Use this method when you
    ''' need to cancel all pending events, such as during scene transitions or when resetting
    ''' game state. After calling this method, <see cref="PendingEventCount"/> will be zero.
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

## License
This project is licensed under the BSD 3-Clause License. See the [LICENSE](LICENSE) file for details.