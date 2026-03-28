# ModuleEventRaiser.Generator
A lightweight VB.NET source generator that automatically creates **RaiseEvent** helper methods for events declared in Modules, stable and feature-complete for its intended use case.

> **v1.1.7.3 Bug Fix**: Fixed invalid leading comma generation in method signatures when event parameter list is empty (caused compilation error).

**New in version 1.1.7+**: 
- **Priority-based event scheduling** - control the order events are raised with priority values
- **Enhanced asynchronous methods** - add optional delays to async event raising
- **Improved parameter documentation** - better XML documentation for generated methods
- Comprehensive **event scheduling system** with thread-safe queue management, perfect for game frameworks (MonoGame, FNA, etc.)
- **Delegate pattern detection** - supports both traditional parameter lists and delegate-based events like `As EventHandler`
- **Multiple event module support** - resolves ambiguity in method calls and supports multiple event modules like `GameEvents`, `UIEvents`, `AudioEvents` and more

## 📦 Version Notes: 1.1.6 → 1.1.7+

### What's New in 1.1.7+?

Version 1.1.7+ includes significant enhancements to both the event scheduling system and asynchronous event handling, making it more powerful and flexible than ever.

### 🔥 Key New Features

#### 1. Priority-Based Event Scheduling
- **Control event order**: Assign priority values to scheduled events
- **Higher priority first**: Events with larger priority values are raised before lower ones
- **Flexible syntax**: `ScheduleEvent_xxx(...parameters, Optional withPriority As Integer = 0)`

#### 2. Enhanced Asynchronous Methods
- **Optional delay support**: Add `withDelaySec` parameter to async methods
- **Validation**: Automatic validation to ensure non-negative delay values
- **Improved syntax**: `RaiseEventAsync_xxx(...parameters, Optional withDelaySec As Double = 0)`

#### 3. Improved Documentation
- **Better XML docs**: Enhanced parameter descriptions and method documentation
- **Clearer usage guidelines**: Improved comments for game framework compatibility
- **Self-documenting code**: More descriptive generated code

### 🛠️ Technical Changes

| Feature | Version 1.1.6 | Version 1.1.7+ |
|---------|---------------|---------------|
| ScheduleEvent signature | `ScheduleEvent_xxx(...params)` | `ScheduleEvent_xxx(...params, Optional withPriority As Integer = 0)` |
| RaiseEventAsync signature | `RaiseEventAsync_xxx(...params)` | `RaiseEventAsync_xxx(...params, Optional withDelaySec As Double = 0)` |
| Event scheduling order | FIFO queue | Priority-based (FIFO within same priority) |
| Async delay support | ❌ No | ✅ Yes |

### 🎯 Who Should Upgrade?
- **Game developers**: Priority-based scheduling helps manage event order
- **Async users**: Delay support adds flexibility to async event handling
- **Documentation lovers**: Improved XML docs for better IntelliSense
- **Everyone**: Backward-compatible with all previous versions

### 📊 Version Comparison

| Version | Priority Scheduling | Async Delays | Multi-Module Support | Documentation |
|---------|---------------------|--------------|----------------------|---------------|
| 1.1.5 | ❌ No | ❌ No | ⚠️ Limited | Good |
| 1.1.6 | ❌ No | ❌ No | ✅ Full | Good |
| 1.1.7+ | ✅ Yes | ✅ Yes | ✅ Full | Excellent |

---

## 📦 Version Notes: 1.1.5 → 1.1.6
*__Version 1.1.5 is stable and fully functional. If you're using a single module for your events, there's no need to upgrade__*. Everything works perfectly with only one module for events in your VB.NET project on version 1.1.5.

### What's New in 1.1.6?

Version 1.1.6 includes a small but meaningful improvement for projects that use **multiple event modules** (e.g., `GameEvents`, `UIEvents`, `AudioEvents`).

### 🔧 The Change
Method calls inside generated `ScheduleEvent_xxx()` methods are now **fully qualified**, eliminating any potential module conflicts.

| Version | Generated Code | Works with 1 Module | Works with 2+ Modules |
|---------|----------------|---------------------|------------------------|
| 1.1.5 | `ScheduleEventAction(...)` | ✅ Yes | ⚠️ May conflict |
| 1.1.6 | `{ModuleName}EventScheduler.ScheduleEventAction(...)` | ✅ Yes | ✅ Yes |

### 👥 Who Should Upgrade?
- You're using **multiple event modules** in your project
- You want to ensure **no ambiguity** in method resolution
- You prefer **explicit, self-documenting code**

### 🤝 Who Can Stay on 1.1.5?
- You're using a **single module** for all events
- Your current setup **already works** and you're happy with it
- You prefer **stability over the latest tweaks**

Both versions are valid. Choose what feels right for your project.

---

> 💡 **Tip**: If you're unsure, version 1.1.7(.2) is always a safe upgrade - it's backward-compatible with all previous versions and adds powerful new features.

## Features
- Automatically generate `RaiseEvent_xxx` methods for Module events
- Supports any event parameter types (Double, String, custom classes, etc.)
- Zero runtime dependencies
- Compile-time only code generation
- Clean and easy to use
- Automatically adds required `Imports` for recognized types
- `RaiseEventAsync_xxx` methods for asynchronous event raising (available in version 1.0.9+)
- **New in 1.1.2**: `ScheduleEvent_xxx` methods for deferred event raising
- **New in 1.1.2**: Thread-safe event scheduler with queue management, perfect for game frameworks (MonoGame, FNA, etc.)
- **New in 1.1.3**: Delegate pattern detection that supports both traditional parameter lists and delegate-based events like `As EventHandler`
- **New in 1.1.6**: **Multiple event module support** - fully qualified method calls for no ambiguity
- **New in 1.1.7**: **Priority-based event scheduling** - control event order with priority values
- **New in 1.1.7**: **Enhanced asynchronous methods** - optional delay support with validation
- **New in 1.1.7.2**: Improved documentation for event scheduling methods, and better XML docs and IntelliSense for generated methods

## Usage Example
Define your events in a partial module (like `MyEvents.vb`):
```vb
Partial Public Module MyEvents
    Public Event TemperatureChanged(temperature As Double)
    Public Event HumidityChanged(humidity As Double)
    Public Event LightLevelChanged(lightLevel As Integer)
End Module
```

The generator automatically creates:
```vb
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
        ArgumentOutOfRangeException.ThrowIfNegative(withDelaySec)
        If withDelaySec > 0 Then Await Task.Delay(TimeSpan.FromSeconds(withDelaySec))
        Await Task.Run(Sub() RaiseEvent TemperatureChanged(temperature))
    End Function

    Public Async Function RaiseEventAsync_HumidityChanged(humidity As Double, Optional withDelaySec As Double = 0) As Task
        ArgumentOutOfRangeException.ThrowIfNegative(withDelaySec)
        If withDelaySec > 0 Then Await Task.Delay(TimeSpan.FromSeconds(withDelaySec))
        Await Task.Run(Sub() RaiseEvent HumidityChanged(humidity))
    End Function

    Public Async Function RaiseEventAsync_LightLevelChanged(lightLevel As Integer, Optional withDelaySec As Double = 0) As Task
        ArgumentOutOfRangeException.ThrowIfNegative(withDelaySec)
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

' --- Event scheduler module (NEW in version 1.1.2, Updated in 1.1.7) ---
''' <summary>
''' Schedules event actions from the MyEvents module to be raised later. 
''' Useful for game frameworks (MonoGame, FNA, etc.) where you want to avoid raising events 
''' during the update phase.
''' </summary>
Public Module MyEventsEventScheduler
    Private ReadOnly _pendingEvents As New Queue(Of ([event] As Action, priority As Integer))
    Private ReadOnly _lock As New Object

    ''' <summary>
    ''' Schedules an event action to be raised later.
    ''' </summary>
    ''' <param name="eventAction">The event action to schedule.</param>
    ''' <param name="priorityValue">The priority value of the event (default is 0).
    ''' Events with higher priority values are raised first.</param>
    Public Sub ScheduleEventAction(eventAction As Action, Optional priorityValue As Integer = 0)
        SyncLock _lock
            _pendingEvents.Enqueue((eventAction, priorityValue))
        End SyncLock
    End Sub

    ''' <summary>
    ''' Raises all scheduled event actions defined in this module.
    ''' </summary>
    ''' <remarks>
    ''' All scheduled events are raised in priority order (higher priority first),
    ''' and FIFO order within the same priority level.
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
    Public Sub ClearScheduledEvents()
        SyncLock _lock
            _pendingEvents.Clear()
        End SyncLock
    End Sub
End Module
```

## Event Scheduling Usage (New in 1.1.2, Enhanced in 1.1.7)

The event scheduling system is queue-based, particularly useful for game frameworks where you need to avoid raising events during critical phases like the update loop. Version 1.1.7 adds priority-based scheduling for more control over event order.

### Basic Scheduling Usage
```vb
' Schedule an event to be raised later (default priority = 0)
MyEvents.ScheduleEvent_TemperatureChanged(25.5)

' Raise all scheduled events at an appropriate time (e.g., during Draw phase)
MyEventsEventScheduler.RaiseScheduledEvents()
```

### Priority-Based Scheduling (NEW in 1.1.7)
```vb
' Schedule events with different priorities
MyEvents.ScheduleEvent_PlayerDied(playerId, withPriority:=10) ' High priority
MyEvents.ScheduleEvent_ScoreUpdated(newScore, withPriority:=5)   ' Medium priority
MyEvents.ScheduleEvent_ParticleEffect(x, y, withPriority:=1)     ' Low priority

' Raising events will process them in priority order:
' 1. PlayerDied (priority 10)
' 2. ScoreUpdated (priority 5)
' 3. ParticleEffect (priority 1)
MyEventsEventScheduler.RaiseScheduledEvents()
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
        MyEventsEventScheduler.RaiseScheduledEvents()
        
        MyBase.Draw(gameTime)
    End Sub
End Class
```

### Enhanced Async Methods (NEW in 1.1.7)
```vb
' Async event with delay
Await MyEvents.RaiseEventAsync_TemperatureChanged(25.5, withDelaySec:=2.0)

' Async event without delay (backward compatible)
Await MyEvents.RaiseEventAsync_HumidityChanged(65.0)
```

### Advanced Scheduler Features
```vb
' Check how many events are pending
Dim pendingCount = MyEventsEventScheduler.PendingEventCount

' Clear all scheduled events without raising them (useful for scene transitions)
MyEventsEventScheduler.ClearScheduledEvents()
```

## Installation
Install via NuGet Package Manager:
```
Install-Package ModuleEventRaiser.Generator
```

Or via .NET CLI:
```
dotnet add package ModuleEventRaiser.Generator
```

## License
[BSD 3-Clause License](LICENSE)