# ModuleEventRaiser.Generator
A lightweight VB.NET source generator that automatically creates **RaiseEvent** helper methods for events declared in Modules. **Enterprise-ready and fully compatible with `Option Infer Off`** - making it perfect for healthcare, financial, and other regulated industries with strict coding standards.

> **v1.2.0 Latest Update**: ⚠️ **BREAKING CHANGE** - Unified event scheduler architecture! Each module now has an `EventScheduler` property instead of separate scheduler modules. Cleaner design, better encapsulation, same powerful functionality.

**Existing features in versions 1.1.x**: 
- **Priority-based event scheduling** - control the order events are raised with priority values
- **Enhanced asynchronous methods** - add optional delays to async event raising
- **Improved parameter documentation** - better XML documentation for generated methods
- Comprehensive **event scheduling system** with thread-safe queue management, perfect for game frameworks (MonoGame, FNA, etc.)
- **Delegate pattern detection** - supports both traditional parameter lists and delegate-based events like `As EventHandler`
- **Multiple event module support** - resolves ambiguity in method calls and supports multiple event modules like `GameEvents`, `UIEvents`, `AudioEvents` and more
- **Namespace support** - properly wraps events in namespaces, preventing naming conflicts
- **Option Infer Off compatibility** - fully compatible with `Option Infer Off` for strict coding standards
- **Legacy .NET support** - compatible with legacy .NET version such as .NET 6.0 and .NET 7.0, by replacing `ArgumentOutOfRangeException.ThrowIfNegative` with a traditional if-check.

## 📦 Version Notes: 1.1.x → 1.2.0

### ⚠️ BREAKING CHANGE: Unified Event Scheduler Architecture

Version 1.2.0 introduces a **major architectural improvement** to the event scheduling system. This is a breaking change that requires code updates when upgrading from previous versions.

### 🎯 What Changed?

#### Before (1.1.x and earlier):
Each module had its own separate event scheduler module:
```vb
' Separate scheduler module for each event module
Public Module MyEventsEventScheduler
    Public Sub ScheduleEventAction(...)
    Public Sub RaiseScheduledEvents()
    Public ReadOnly Property PendingEventCount As Integer
    Public Sub ClearScheduledEvents()
End Module

' Usage
MyEventsEventScheduler.RaiseScheduledEvents()
MyEventsEventScheduler.PendingEventCount
```

#### After (1.2.0):
A unified `ModuleEventScheduler` class is generated globally, and each module gets an `EventScheduler` property:
```vb
' Unified scheduler class (generated once globally)
Public NotInheritable Class ModuleEventScheduler
    Public Sub ScheduleEventAction(...)
    Public Sub RaiseScheduledEvents()
    Public ReadOnly Property PendingEventCount As Integer
    Public Sub ClearScheduledEvents()
End Class

' Each module gets an EventScheduler property
Partial Public Module MyEvents
    Public ReadOnly Property EventScheduler As New ModuleEventScheduler
End Module

' Usage
MyEvents.EventScheduler.RaiseScheduledEvents()
MyEvents.EventScheduler.PendingEventCount
```

### 🔄 Migration Guide

#### Step 1: Update scheduler method calls
Replace all references to `{ModuleName}EventScheduler` with `{ModuleName}.EventScheduler`:

| Old Code (1.1.x) | New Code (1.2.0) |
|------------------|------------------|
| `MyEventsEventScheduler.RaiseScheduledEvents()` | `MyEvents.EventScheduler.RaiseScheduledEvents()` |
| `GameEventsEventScheduler.RaiseScheduledEvents()` | `GameEvents.EventScheduler.RaiseScheduledEvents()` |
| `MyEventsEventScheduler.PendingEventCount` | `MyEvents.EventScheduler.PendingEventCount` |
| `MyEventsEventScheduler.ClearScheduledEvents()` | `MyEvents.EventScheduler.ClearScheduledEvents()` |

#### Step 2: Update scheduler property access
If you have any code that references the scheduler modules directly:
```vb
' Old code (1.1.x)
MyEventsEventScheduler.RaiseScheduledEvents()

' New code (1.2.0, with a dot notation)
MyEvents.EventScheduler.RaiseScheduledEvents()
```

#### Step 3: No changes needed for ScheduleEvent_xxx methods
The `ScheduleEvent_xxx` methods in each module remain unchanged:
```vb
' This still works the same way
MyEvents.ScheduleEvent_TemperatureChanged(25.5, withPriority:=10)
```

### ✅ Benefits of the New Architecture

1. **Better Encapsulation**: Each module owns its scheduler instance through a property
2. **Cleaner Code Generation**: Single `ModuleEventScheduler` class instead of multiple scheduler modules
3. **Consistent API**: All scheduler functionality accessed through the module's `EventScheduler` property
4. **Same Functionality**: All features (priority scheduling, thread safety, etc.) work exactly the same
5. **Better IntelliSense**: Property-based access provides better IDE integration

### 🛠️ Technical Changes

| Feature | 1.1.x | 1.2.0 |
|---------|-------|-------|
| Scheduler Architecture | Per-module scheduler modules | Unified `ModuleEventScheduler` class |
| Access Pattern | `MyEventsEventScheduler.Method()` | `MyEvents.EventScheduler.Method()` |
| Code Generation | Multiple scheduler modules | Single scheduler class + module properties |
| Functionality | ✅ Full | ✅ **Full (same)** |
| Thread Safety | ✅ Yes | ✅ **Yes (same)** |
| Priority Support | ✅ Yes | ✅ **Yes (same)** |
| Module Accessibility Detection | ✅ Yes | ✅ **Yes (same)** |
| Namespace Support | ✅ Excellent | ✅ **Excellent (same)** |
| Option Infer Off Compatibility | ✅ Enterprise | ✅ **Enterprise (same)** |

### 🎯 Who Should Upgrade to 1.2.0?

#### 🚀 **Must Upgrade**
- **All users**: This is the latest stable version with architectural improvements
- **New projects**: Start with the cleaner, more maintainable architecture
- **Library authors**: Provide users with the best possible API design

#### ✅ **Upgrade Required (Breaking Change)**
- **Existing projects**: Must update scheduler method calls (see Migration Guide above)
- **Code using scheduler modules**: Replace `{ModuleName}EventScheduler` with `{ModuleName}.EventScheduler`

#### 💡 **Upgrade Effort**
- **Low effort**: Simple find-and-replace for scheduler method calls
- **High value**: Cleaner architecture, better encapsulation, future-proof design

### 📊 Migration Checklist

- [ ] Replace all `{ModuleName}EventScheduler.RaiseScheduledEvents()` with `{ModuleName}.EventScheduler.RaiseScheduledEvents()`
- [ ] Replace all `{ModuleName}EventScheduler.PendingEventCount` with `{ModuleName}.EventScheduler.PendingEventCount`
- [ ] Replace all `{ModuleName}EventScheduler.ClearScheduledEvents()` with `{ModuleName}.EventScheduler.ClearScheduledEvents()`
- [ ] Update any direct references to scheduler modules
- [ ] Test your event scheduling functionality after upgrade
- [ ] Verify that all scheduled events are raised correctly

## Features
- Automatically generate `RaiseEvent_xxx` methods for Module events
- Supports any event parameter types (Double, String, custom classes, etc.)
- Zero runtime dependencies
- Compile-time only code generation
- Clean and easy to use
- Automatically adds required `Imports` for recognized types
- `RaiseEventAsync_xxx` methods for asynchronous event raising (available in version 1.0.9+)
- **Event scheduling system** with queue management, perfect for game frameworks (MonoGame, FNA, etc.)
- **Unified scheduler architecture** (NEW in 1.2.0) - each module has an `EventScheduler` property for cleaner encapsulation
- **Priority-based event scheduling** - control event order with priority values (available in version 1.1.7+)
- **Enhanced asynchronous methods** - optional delay support with validation (available in version 1.1.7+)
- **Delegate pattern detection** that supports both traditional parameter lists and delegate-based events like `As EventHandler` (available in version 1.1.3+)
- **Multiple event module support** - fully qualified method calls for no ambiguity (available in version 1.1.6+)
- **Multi-namespace support** - ultimate feature completeness for enterprise projects (available in version 1.1.7.9+)
- **`Option Infer Off` compatibility** - explicit typing for healthcare, financial, and regulated industries (available in version 1.1.7.9+)
- **Legacy .NET support** - compatible with legacy .NET version such as .NET 6.0 and .NET 7.0 (available in version 1.1.7.10+)
- **Module accessibility detection** - proper handling of Public/Friend modules (available in version 1.1.8+)

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
        Public [Event] As Action
        Public Priority As Integer
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