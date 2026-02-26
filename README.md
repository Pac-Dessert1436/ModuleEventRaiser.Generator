# `ModuleEventRaiser.Generator` - An Event Raiser Generator for VB.NET Modules

## Description
`ModuleEventRaiser.Generator` is a .NET source generator that automatically creates event raiser methods for events declared in VB.NET modules. It helps developers to raise events in a consistent, efficient, and well-documented manner, reducing boilerplate code and improving code readability.

Currently available as a NuGet package: `dotnet add package ModuleEventRaiser.Generator --version 1.1.7.2`. Having undergone frequent version updates recently, this source generator is **stable and feature-complete for its intended use case**. Updates in the future will be considered only for:
- Critical bug fixes
- Compatibility with new .NET versions
- Truly compelling feature requests

Version 1.1.7 introduces optional delay in seconds for async event raising, together with priority-based event scheduling. It also adds reserved parameter names for these new features, and now it is here to stay, working quietly in the background.

> NOTE: Version 1.1.7.2 works exactly the same as 1.1.7 except for XML documentation adjustments.

**Important Notes:**
- The source generator only works with VB.NET modules and does not support classes or structures.
- The generator includes `Imports System` by default in generated files.
- Additional imports for custom types are now properly recognized - no other settings required.
  - e.g. `Public Event CollidePoint(rect As RectangleF, point As Vector2)` (in VB.NET MonoGame projects)
  - This will include `Imports Microsoft.Xna.Framework` in the generated source file.
- **For version 1.1.5**: It is recommended to use parameterized events (e.g., `Public Event MyEvent(sender As Object, e As EventArgs)`) for better clarity, though delegate pattern events (e.g., `Public Event MyEvent As EventHandler`) are still fully supported and useful, especially since `EventHandler` itself provides descriptive parameter naming.
- **For version 1.1.7+**: The following parameter names are reserved and should not be used when defining module events:
  - `withPriority`: Used for priority-based event scheduling
  - `withDelaySec`: Used for optional delay in async event raising

## Key Features
- **Automatic Code Generation**: Generates event raiser methods for all events in VB.NET modules
- **Well-Documented**: Includes XML documentation for all generated methods
- **Parameter Handling**: Correctly handles event parameters with proper types
- **Custom Event Types**: Supports both standard `EventHandler` and custom event types
- **Partial Modules**: Uses partial modules to seamlessly integrate with existing code
- **Incremental Generation**: Uses the latest incremental generator pattern for fast builds
- **Async Raiser Support**: Generates asynchronous `RaiseEventAsync_*` methods for all events
- **Event Scheduler**: Generates a dedicated `{ModuleName}EventScheduler` module for each event module, allowing events to be scheduled and raised later - ideal for game frameworks like MonoGame and FNA
- **Automatic Namespace Detection**: Automatically detects and includes required namespaces for event parameter types
- **Delegate Pattern Support (Version 1.1.3+)**: Generates event raiser methods for events defined using delegate pattern (e.g. `Public Event MyEvent As EventHandler`)
- **Optional Delay for Async Events (Version 1.1.7)**: Allows specifying a delay in seconds when raising events asynchronously, useful for simulating real-world event timing (Note: Parameter name `withDelaySec` is reserved to avoid conflicts with other event parameters)
- **Priority-Based Event Scheduling (Version 1.1.7)**: Supports prioritizing scheduled events for more flexible event management, with higher priority events being raised first (Note: Parameter name `withPriority` is reserved to avoid conflicts with other event parameters)

## Prerequisites
- [Visual Studio 2026](https://visualstudio.microsoft.com/vs/)
- [.NET SDK 8.0+](https://dotnet.microsoft.com/download)
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
   dotnet add package ModuleEventRaiser.Generator --version 1.1.7.2
   ```
   - Versions 1.1.7 introduces optional delay in seconds for async event raising, together with priority-based event scheduling, and version 1.1.7.2 tweaks the XML documentation for the generated methods.

## Example Usage

### Input: VB.NET Module with Events
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

### Output: Generated Event Raiser Methods

**Documentation follows the same pattern**:
- `RaiseEvent_*` methods: Raises the * event (direct invocation).
> NOTE: The above method is the simplest way to raise events, but synchronous.
- `RaiseEventAsync_*` methods: Asynchronously raises the * event. Use this method only in desktop apps, networking, etc. DO NOT USE THIS METHOD WHEN WRITING GAME LOGIC IN GAME FRAMEWORKS (MonoGame, FNA, etc.).
- `ScheduleEvent_*` methods: Schedules the * event to be raised later. Useful for game frameworks (MonoGame, FNA, etc.).

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

    Public Sub RaiseEvent_TemperatureChanged(temperature As Double)
        RaiseEvent TemperatureChanged(temperature)
    End Sub

    Public Async Function RaiseEventAsync_TemperatureChanged(temperature As Double, Optional withDelaySec As Double = 0) As Task
        ArgumentOutOfRangeException.ThrowIfNegative(withDelaySec)
        If withDelaySec > 0 Then Await Task.Delay(TimeSpan.FromSeconds(withDelaySec))
        Await Task.Run(Sub() RaiseEvent TemperatureChanged(temperature))
    End Function

    Public Sub ScheduleEvent_TemperatureChanged(temperature As Double, Optional withPriority As Integer = 0)
        MyEventsEventScheduler.ScheduleEventAction(Sub() RaiseEvent TemperatureChanged(temperature), withPriority)
    End Sub

    Public Sub RaiseEvent_HumidityChanged(humidity As Double)
        RaiseEvent HumidityChanged(humidity)
    End Sub

    Public Async Function RaiseEventAsync_HumidityChanged(humidity As Double, Optional withDelaySec As Double = 0) As Task
        ArgumentOutOfRangeException.ThrowIfNegative(withDelaySec)
        If withDelaySec > 0 Then Await Task.Delay(TimeSpan.FromSeconds(withDelaySec))
        Await Task.Run(Sub() RaiseEvent HumidityChanged(humidity))
    End Function

    Public Sub ScheduleEvent_HumidityChanged(humidity As Double, Optional withPriority As Integer = 0)
        MyEventsEventScheduler.ScheduleEventAction(Sub() RaiseEvent HumidityChanged(humidity), withPriority)
    End Sub

    Public Sub RaiseEvent_LightLevelChanged(lightLevel As Integer)
        RaiseEvent LightLevelChanged(lightLevel)
    End Sub

    Public Async Function RaiseEventAsync_LightLevelChanged(lightLevel As Integer, Optional withDelaySec As Double = 0) As Task
        ArgumentOutOfRangeException.ThrowIfNegative(withDelaySec)
        If withDelaySec > 0 Then Await Task.Delay(TimeSpan.FromSeconds(withDelaySec))
        Await Task.Run(Sub() RaiseEvent LightLevelChanged(lightLevel))
    End Function

    Public Sub ScheduleEvent_LightLevelChanged(lightLevel As Integer, Optional withPriority As Integer = 0)
        MyEventsEventScheduler.ScheduleEventAction(Sub() RaiseEvent LightLevelChanged(lightLevel), withPriority)
    End Sub

    ' NEW in 1.1.3: Delegate pattern event raising methods (documentation follows the same pattern)
    Public Sub RaiseEvent_MyEvent(sender As Object, e As EventArgs)
        RaiseEvent MyEvent(sender, e)
    End Sub

    Public Async Function RaiseEventAsync_MyEvent(sender As Object, e As EventArgs, Optional withDelaySec As Double = 0) As Task
        ArgumentOutOfRangeException.ThrowIfNegative(withDelaySec)
        If withDelaySec > 0 Then Await Task.Delay(TimeSpan.FromSeconds(withDelaySec))
        Await Task.Run(Sub() RaiseEvent MyEvent(sender, e))
    End Sub

    Public Sub ScheduleEvent_MyEvent(sender As Object, e As EventArgs, Optional withPriority As Integer = 0)
        MyEventsEventScheduler.ScheduleEventAction(Sub() RaiseEvent MyEvent(sender, e), withPriority)
    End Sub

    ' ... More delegate pattern event raising methods ...
End Module

''' <summary>
''' Schedules event actions from the MyEvents module to be raised later. 
''' Useful for game frameworks (MonoGame, FNA, etc.) where you want to avoid raising events 
''' directly during the update phase.
''' </summary>
''' <remarks>
''' This module provides a thread-safe way to queue events and raise them at a later time,
''' particularly important in game development to maintain consistent frame rates and avoid
''' race conditions.
''' </remarks>
Public Module MyEventsEventScheduler
    Private ReadOnly _pendingEvents As New Queue(Of ([event] As Action, priority As Integer))
    Private ReadOnly _lock As New Object

    ''' <summary>
    ''' Schedules an event action to be raised later.
    ''' </summary>
    ''' <param name=""eventAction"">The event action to schedule.</param>
    ''' <param name=""priorityValue"">The priority value of the event (default is 0).
    ''' Events with higher priority values are raised first.</param>
    ''' <remarks>
    ''' This method is thread-safe and can be called from any thread.
    ''' </remarks>
    Public Sub ScheduleEventAction(eventAction As Action, Optional priorityValue As Integer = 0)
        SyncLock _lock
            _pendingEvents.Enqueue((eventAction, priorityValue))
        End SyncLock
    End Sub

    ''' <summary>
    ''' Raises all scheduled event actions defined in this module. Events within the same
    ''' priority level are raised in first-in-first-out (FIFO) order.
    ''' </summary>
    ''' <remarks>
    ''' This method is thread-safe and should be called during a phase where event handling 
    ''' is safe (e.g., during the 'Draw' phase in game frameworks). All scheduled events are 
    ''' raised in the order they were scheduled, with HIGHER PRIORITY events raised FIRST.
    ''' </remarks>
    Public Sub RaiseScheduledEvents()
        Dim actionsToRaise = Array.Empty(Of Action)()
        SyncLock _lock
            If _pendingEvents.Count = 0 Then Exit Sub
            actionsToRaise = Aggregate e In _pendingEvents Order By e.priority Descending
                                 Select e.event Into ToArray()
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
End Module
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
    MyEventsEventScheduler.RaiseScheduledEvents()
    
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
5. **Event Scheduler Generation**: It creates a dedicated `{ModuleName}EventScheduler` module for each event module
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
- **Event Scheduler Module**: Dedicated `{ModuleName}EventScheduler` module for managing scheduled events with priority-based ordering
- **Well-Formatted Code**: Proper indentation and spacing for readability

### The Event Scheduler
The generated `{ModuleName}EventScheduler` module provides a thread-safe way to schedule events to be raised later, which is particularly useful in game frameworks like MonoGame and FNA where you want to avoid raising events during the update phase.

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
    MyEventsEventScheduler.RaiseScheduledEvents()
    
    ' Render game graphics
    ' ...
End Sub
```

## Benefits
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

## License
This project is licensed under the BSD 3-Clause License. See the [LICENSE](LICENSE) file for details.