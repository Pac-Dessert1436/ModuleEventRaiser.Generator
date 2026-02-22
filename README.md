# ModuleEventRaiser.Generator
A lightweight VB.NET source generator that automatically creates **RaiseEvent** helper methods for events declared in Modules. 

**New in version 1.1.2 or later**: 
- Comprehensive **event scheduling system** with thread-safe queue management, perfect for game frameworks (MonoGame, FNA, etc.)
- **Delegate pattern detection** (available in version 1.1.3+) - supports both traditional parameter lists and delegate-based events like `As EventHandler`
- **Multiple event module support** (available in version 1.1.6) - resolves ambiguity in method calls and supports multiple event modules like `GameEvents`, `UIEvents`, `AudioEvents` and more

## 📦 Version Notes: 1.1.5 → 1.1.6
*__Version 1.1.5 is stable and fully functional. If you're using a single module for your events, there's no need to upgrade__*. Everything works perfectly with only one module for events in your VB.NET project on version 1.1.5.

### What's New in 1.1.6?

Version 1.1.6 includes a small but meaningful improvement for projects that use **multiple event modules** (e.g., `GameEvents`, `UIEvents`, `AudioEvents`).

#### 🔧 The Change
Method calls inside generated `ScheduleEvent_xxx()` methods are now **fully qualified**, eliminating any potential module conflicts.

| Version | Generated Code | Works with 1 Module | Works with 2+ Modules |
|---------|----------------|---------------------|------------------------|
| 1.1.5 | `ScheduleEventAction(...)` | ✅ Yes | ⚠️ May conflict |
| 1.1.6 | `{ModuleName}EventScheduler.ScheduleEventAction(...)` | ✅ Yes | ✅ Yes |

#### 👥 Who Should Upgrade?
- You're using **multiple event modules** in your project
- You want to ensure **no ambiguity** in method resolution
- You prefer **explicit, self-documenting code**

#### 🤝 Who Can Stay on 1.1.5?
- You're using a **single module** for all events
- Your current setup **already works** and you're happy with it
- You prefer **stability over the latest tweaks**

Both versions are valid. Choose what feels right for your project.

---

> 💡 **Tip**: If you're unsure, 1.1.6 is always a safe upgrade - it's backward-compatible with all 1.1.5 features and adds no breaking changes.

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
    Public Async Function RaiseEventAsync_TemperatureChanged(temperature As Double) As Task
        Await Task.Run(Sub() RaiseEvent TemperatureChanged(temperature))
    End Function

    Public Async Function RaiseEventAsync_HumidityChanged(humidity As Double) As Task
        Await Task.Run(Sub() RaiseEvent HumidityChanged(humidity))
    End Function

    Public Async Function RaiseEventAsync_LightLevelChanged(lightLevel As Integer) As Task
        Await Task.Run(Sub() RaiseEvent LightLevelChanged(lightLevel))
    End Function

    ' --- Event scheduling methods (NEW in version 1.1.2) ---
    Public Sub ScheduleEvent_TemperatureChanged(temperature As Double)
        MyEventsEventScheduler.ScheduleEventAction(Sub() RaiseEvent TemperatureChanged(temperature))
    End Sub

    Public Sub ScheduleEvent_HumidityChanged(humidity As Double)
        MyEventsEventScheduler.ScheduleEventAction(Sub() RaiseEvent HumidityChanged(humidity))
    End Sub

    Public Sub ScheduleEvent_LightLevelChanged(lightLevel As Integer)
        MyEventsEventScheduler.ScheduleEventAction(Sub() RaiseEvent LightLevelChanged(lightLevel))
    End Sub
End Module

' --- Event scheduler module (NEW in version 1.1.2) ---
''' <summary>
''' Schedules event actions from the MyEvents module to be raised later. 
''' Useful for game frameworks (MonoGame, FNA, etc.) where you want to avoid raising events 
''' during the update phase.
''' </summary>
Public Module MyEventsEventScheduler
    Private ReadOnly _pendingEvents As New List(Of Action)
    Private ReadOnly _lock As New Object()

    ''' <summary>
    ''' Schedules an event action to be raised later.
    ''' </summary>
    Public Sub ScheduleEventAction(eventAction As Action)
        SyncLock _lock
            _pendingEvents.Add(eventAction)
        End SyncLock
    End Sub

    ''' <summary>
    ''' Raises all scheduled event actions defined in this module.
    ''' </summary>
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

## Event Scheduling Usage (New in 1.1.2)

The event scheduling system is queue-based, particularly useful for game frameworks where you need to avoid raising events during critical phases like the update loop.

### Basic Scheduling Usage
```vb
' Schedule an event to be raised later
MyEvents.ScheduleEvent_TemperatureChanged(25.5)

' Raise all scheduled events at an appropriate time (e.g., during Draw phase)
MyEventsEventScheduler.RaiseScheduledEvents()
```

### Game Framework Example (MonoGame/FNA)
```vb
Public Class Game1
    Inherits Game
    
    Protected Overrides Sub Update(gameTime As GameTime)
        ' During update phase, schedule events instead of raising them immediately
        If temperatureChanged Then
            MyEvents.ScheduleEvent_TemperatureChanged(newTemperature)
        End If
        
        MyBase.Update(gameTime)
    End Sub
    
    Protected Overrides Sub Draw(gameTime As GameTime)
        ' During draw phase, safely raise all scheduled events
        MyEventsEventScheduler.RaiseScheduledEvents()
        
        MyBase.Draw(gameTime)
    End Sub
End Class
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
BSD 3-Clause License