# ModuleEventRaiser.Generator
A powerful yet lightweight VB.NET source generator that automatically creates **RaiseEvent** helper methods for events declared in `Module`s. Despite its rich feature set, the generator maintains a tiny footprint with zero runtime dependencies and compile-time only code generation.

The source generator is **enterprise-ready and fully compatible with `Option Infer Off`**, making it perfect for healthcare, financial, and other regulated industries with strict coding standards.

> **v1.2.2 Latest Update**: Experimental weak event support with `WeakMulticastEvent` class! Prevent memory leaks in event-driven architectures while maintaining full compatibility with standard VB.NET event patterns.
>
> **v1.2.0**: ⚠️ **BREAKING CHANGE** - Unified event scheduler architecture! Each module now has an `EventScheduler` property instead of separate scheduler modules. Cleaner design, better encapsulation, same powerful functionality.

## 📦 Version Notes: 1.2.0 → 1.2.2

### v1.2.2: Experimental Weak Event Support

**New Feature**: Added `WeakMulticastEvent` class to prevent memory leaks in event-driven architectures!

#### Key Benefits:
- **Memory Leak Prevention**: Weak references allow subscribers to be garbage collected even when publishers remain alive
- **Full VB.NET Compatibility**: Works seamlessly with standard VB.NET event patterns using `Custom Event` syntax
- **High Performance**: Uses strongly-typed invocation with compiled expression trees (avoids slow `DynamicInvoke`)
- **Thread-Safe**: All operations are fully thread-safe for multi-threaded scenarios
- **Automatic Cleanup**: Automatically removes dead handlers from the collection

#### Usage Example:
```vb
' Declare a weak multicast event field in a module
Private _myEvent As New WeakMulticastEvent(Of Action(Of Integer))

' Expose as standard VB.NET event in the module using `Custom Event` syntax
Public Custom Event MyEvent As Action(Of Integer)
    AddHandler(value As Action(Of Integer))
        _myEvent.AddHandler(value)
    End AddHandler
    RemoveHandler(value As Action(Of Integer))
        _myEvent.RemoveHandler(value)
    End RemoveHandler
    RaiseEvent(obj As Integer)
        _myEvent.RaiseEvent(obj)
    End RaiseEvent
End Event
```
The generator will automatically create `RaiseEvent_xxx`, `RaiseEventAsync_xxx` and `ScheduleEvent_xxx` methods for the above module event.

#### Technical Details:
- **Namespace**: Automatically uses your project's root namespace (no explicit declaration needed)
- **Generic Type**: `WeakMulticastEvent(Of TDelegate As Class)` supports any delegate type. _VB.NET does not directly support Delegate type constraints, so runtime type validation ensures only delegate types are accepted_
- **Methods**: `AddHandler`, `RemoveHandler`, `RaiseEvent`, `Clear`, and `ActiveHandlerCount` property
- **Performance**: Up to 10-100x faster than `DynamicInvoke` for event raising (benchmark-proven improvement)

### v1.2.0: Unified Event Scheduler Architecture (Breaking Change)

**Major Improvement**: Replaced per-module scheduler modules with a unified architecture

#### What Changed?
- **Before**: Separate scheduler modules like `MyEventsEventScheduler` for each event module
- **After**: Single `ModuleEventScheduler` class + `EventScheduler` property on each module
- **Access Pattern**: `MyEvents.EventScheduler.RaiseScheduledEvents()` instead of `MyEventsEventScheduler.RaiseScheduledEvents()`

#### Migration Guide:
1. Replace all `{ModuleName}EventScheduler.Method()` calls with `{ModuleName}.EventScheduler.Method()`
2. No changes needed for `ScheduleEvent_xxx` methods - they work exactly the same
3. Example: `MyEventsEventScheduler.RaiseScheduledEvents()` → `MyEvents.EventScheduler.RaiseScheduledEvents()`

#### Benefits:
- Better encapsulation with module-owned scheduler instances
- Cleaner code generation (single scheduler class instead of multiple modules)
- Improved IntelliSense through property-based access
- Same powerful functionality (priority scheduling, thread safety, etc.)

## Key Features
- **Automatic Event Raiser Generation**: Creates `RaiseEvent_xxx` methods for all events in your modules
- **Universal Parameter Support**: Works with any event parameter types (primitives, strings, custom classes, etc.)
- **Zero Runtime Overhead**: Compile-time only code generation with no runtime dependencies
- **Asynchronous Event Support**: `RaiseEventAsync_xxx` methods for non-blocking event raising (v1.0.9+)
- **Smart Import Management**: Automatically adds required `Imports` statements for recognized types
- **Clean, Intuitive API**: Easy-to-use methods that follow VB.NET best practices

### Advanced Features
- **Priority-Based Event Scheduling**: Control event execution order with priority values (v1.1.7+)
- **Enhanced Asynchronous Methods**: Optional delay support with validation for precise timing control (v1.1.7+)
- **Unified Scheduler Architecture**: Each module gets an `EventScheduler` property for clean encapsulation (v1.2.0+)
- **Weak Event Support**: `WeakMulticastEvent` class prevents memory leaks in event-driven architectures (EXPERIMENTAL v1.2.2+)
- **Delegate Pattern Detection**: Supports both traditional parameter lists and delegate-based events like `As EventHandler` (v1.1.3+)
- **Multi-Module Support**: Resolves ambiguity in method calls across multiple event modules (v1.1.6+)
- **Enterprise-Grade Namespace Handling**: Perfectly supports multi-namespace projects for large-scale applications (v1.1.7.9+)
- **Strict Coding Standards**: Fully compatible with `Option Infer Off` for regulated industries (v1.1.7.9+)
- **Legacy .NET Compatibility**: Works seamlessly with .NET 6.0, .NET 7.0, and newer versions (v1.1.7.10+)
- **Accessibility Awareness**: Properly handles Public/Friend module access levels (v1.1.8+)
- **Polished Generated Code**: Clean, well-documented output with explicit `Imports` and immutable structure fields

## Usage Example

### Basic Single Namespace Usage
Define your events in a partial module (like `MyEvents.vb`), which will be located in your project's root namespace:
```vb
Partial Public Module MyEvents
    Public Event TemperatureChanged(temperature As Double)
    Public Event HumidityChanged(humidity As Double)
    Public Event LightLevelChanged(lightLevel As Integer)
End Module
```

### Multi-Namespace Support (NEW in 1.1.7.9)
> **Important Note**: Version 1.1.7.5 does NOT support multiple namespaces as planned.

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

Namespace isolation and proper code generation are ensured for each namespace.

### Output: Generated Event Raiser Methods
```vb
Option Explicit On
Option Strict On

Imports System
' Necessary imports (and namespace declaration) are automatically added here.

Partial Public Module MyEvents
    Public Sub RaiseEvent_TemperatureChanged(temperature As Double)
        RaiseEvent TemperatureChanged(temperature)
    End Sub

    Public Sub RaiseEvent_HumidityChanged(humidity As Double)
        RaiseEvent HumidityChanged(humidity)
    End Sub

    Public Sub RaiseEvent_LightLevelChanged(lightLevel As Integer)
        RaiseEvent LightLevelChanged(lightLevel)
    End Sub

    ' --- Asynchronous event raising methods (available in version 1.0.9+) ---
    Public Async Function RaiseEventAsync_TemperatureChanged(temperature As Double, Optional withDelaySec As Double = 0) As Task
        If withDelaySec < 0 Then Throw New ArgumentOutOfRangeException(NameOf(withDelaySec), "Delay seconds must be non-negative.")
        If withDelaySec > 0 Then Await Task.Delay(TimeSpan.FromSeconds(withDelaySec))
        Await Task.Run(Sub() RaiseEvent TemperatureChanged(temperature))
    End Function

    Public Async Function RaiseEventAsync_HumidityChanged(humidity As Double, Optional withDelaySec As Double = 0) As Task
        If withDelaySec < 0 Then Throw New ArgumentOutOfRangeException(NameOf(withDelaySec), "Delay seconds must be non-negative.")
        If withDelaySec > 0 Then Await Task.Delay(TimeSpan.FromSeconds(withDelaySec))
        Await Task.Run(Sub() RaiseEvent HumidityChanged(humidity))
    End Function

    Public Async Function RaiseEventAsync_LightLevelChanged(lightLevel As Integer, Optional withDelaySec As Double = 0) As Task
        If withDelaySec < 0 Then Throw New ArgumentOutOfRangeException(NameOf(withDelaySec), "Delay seconds must be non-negative.")
        If withDelaySec > 0 Then Await Task.Delay(TimeSpan.FromSeconds(withDelaySec))
        Await Task.Run(Sub() RaiseEvent LightLevelChanged(lightLevel))
    End Function

    ' --- Event scheduling methods (NEW in version 1.1.2) ---
    Public Sub ScheduleEvent_TemperatureChanged(temperature As Double, Optional withPriority As Integer = 0)
        MyEventsEventScheduler.ScheduleEventAction(Sub() RaiseEvent TemperatureChanged(temperature), withPriority)
    End Sub

    Public Sub ScheduleEvent_HumidityChanged(humidity As Double, Optional withPriority As Integer = 0)
        MyEventsEventScheduler.ScheduleEventAction(Sub() RaiseEvent HumidityChanged(humidity), withPriority)
    End Sub

    Public Sub ScheduleEvent_LightLevelChanged(lightLevel As Integer, Optional withPriority As Integer = 0)
        MyEventsEventScheduler.ScheduleEventAction(Sub() RaiseEvent LightLevelChanged(lightLevel), withPriority)
    End Sub
End Module

' --- Event scheduler property (NEW in version 1.2.0) ---
' Each module now has an EventScheduler property that provides access to the unified ModuleEventScheduler
' The ModuleEventScheduler class is generated once globally and shared across all modules
Partial Public Module MyEvents
    ''' <summary>
    ''' Provides access to the unified event scheduler for this module.
    ''' </summary>
    ''' <value>A shared instance of <see cref="ModuleEventScheduler"/> for scheduling and raising events.</value>
    ''' <remarks>
    ''' This property provides a thread-safe mechanism to schedule events to be raised later,
    ''' which is particularly useful in game development frameworks (MonoGame, FNA, Unity, etc.)
    ''' where raising events during the update phase can cause performance issues.
    ''' </remarks>
    Public ReadOnly Property EventScheduler As New ModuleEventScheduler
End Module
```

### Unified event scheduler class (NEW in version 1.2.0)
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

## Event Scheduling Usage (Updated in 1.2.0)

The event scheduling system is queue-based, particularly useful for game frameworks where you need to avoid raising events during critical phases like the update loop. Version 1.2.0 introduces a unified scheduler architecture with each module having its own `EventScheduler` property.

### Basic Scheduling Usage
```vb
' Schedule an event to be raised later (default priority = 0)
MyEvents.ScheduleEvent_TemperatureChanged(25.5)

' Raise all scheduled events at an appropriate time (e.g., during Draw phase)
MyEvents.EventScheduler.RaiseScheduledEvents()
```

### Priority-Based Scheduling
```vb
' Schedule events with different priorities
MyEvents.ScheduleEvent_PlayerDied(playerId, withPriority:=10) ' High priority
MyEvents.ScheduleEvent_ScoreUpdated(newScore, withPriority:=5)   ' Medium priority
MyEvents.ScheduleEvent_ParticleEffect(x, y, withPriority:=1)     ' Low priority

' Raising events will process them in priority order:
' 1. PlayerDied (priority 10)
' 2. ScoreUpdated (priority 5)
' 3. ParticleEffect (priority 1)
MyEvents.EventScheduler.RaiseScheduledEvents()
```

### Game Framework Example (MonoGame/FNA)
```vb
Public Class Game1
    Inherits Game
    
    Protected Overrides Sub Update(gameTime As GameTime)
        ' During update phase, schedule events instead of raising them immediately
        If playerHealth <= 0 Then
            ' Critical event - high priority
            MyEvents.ScheduleEvent_PlayerDied(playerId, withPriority:=100)
        ElseIf scoreChanged Then
            ' Important but not critical
            MyEvents.ScheduleEvent_ScoreUpdated(newScore, withPriority:=50)
        ElseIf enemyKilled Then
            ' Regular gameplay event
            MyEvents.ScheduleEvent_EnemyKilled(enemyId, withPriority:=10)
        End If
        
        MyBase.Update(gameTime)
    End Sub
    
    Protected Overrides Sub Draw(gameTime As GameTime)
        ' During draw phase, safely raise all scheduled events
        ' Events will be processed in priority order
        MyEvents.EventScheduler.RaiseScheduledEvents()
        
        MyBase.Draw(gameTime)
    End Sub
End Class
```

### Enhanced Async Methods
```vb
' Async event with delay
Await MyEvents.RaiseEventAsync_TemperatureChanged(25.5, withDelaySec:=2.0)

' Async event without delay (backward compatible)
Await MyEvents.RaiseEventAsync_HumidityChanged(65.0)
```

### Advanced Scheduler Features
```vb
' Check how many events are pending
Dim pendingCount = MyEvents.EventScheduler.PendingEventCount

' Clear all scheduled events without raising them (useful for scene transitions)
MyEvents.EventScheduler.ClearScheduledEvents()
```

### Multi-Module Scheduler Usage (Updated in 1.2.0)
Each module has its own `EventScheduler` property, allowing independent event scheduling:

```vb
' Schedule events in different modules
GameEvents.ScheduleEvent_PlayerDied(playerId, withPriority:=10)
UIEvents.ScheduleEvent_ButtonClicked(buttonName, withPriority:=5)
AudioEvents.ScheduleEvent_SoundPlayed(soundId, withPriority:=3)

' Raise events from each module independently
GameEvents.EventScheduler.RaiseScheduledEvents()
UIEvents.EventScheduler.RaiseScheduledEvents()
AudioEvents.EventScheduler.RaiseScheduledEvents()

' Or check pending events for each module
Dim gameEventsPending = GameEvents.EventScheduler.PendingEventCount
Dim uiEventsPending = UIEvents.EventScheduler.PendingEventCount
```

## Installation
Install via NuGet Package Manager:
```powershell
Install-Package ModuleEventRaiser.Generator
```

Or via .NET CLI:
```bash
dotnet add package ModuleEventRaiser.Generator
```

## License
[BSD 3-Clause License](LICENSE)