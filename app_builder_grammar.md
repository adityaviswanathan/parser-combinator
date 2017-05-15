App Builder Grammar Sketch
==========================

## Example Usage
```
Completed = Property {
	AppType = Enums.AppType.BOOL
	Entity = Todo
	Name = "Completed"
}
Todo = Entity {
	Properties = [Property {
		AppType = Enums.AppType.STRING
		Entity = Todo
		Name = "Payload"
	}, Completed]
	Name = "Todo"
}
TodoField = Component {
	ComponentType = Enums.ComponentType.DYNAMIC
	ElementType = Enums.ElementType.INPUT
	Row = 0
	Column = 0
	Width = 8
	Offset = 0
}
TodoSave = Component {
	ComponentType = Enums.ComponentType.DYNAMIC
	ElementType = Enums.ElementType.BUTTON
	Row = 0
	Column = 8
	Width = 4
	Offset = 0
}
TodoSaveEvent = Event {
	EventType = Enums.EventType.CLICK
	Component = TodoSave
	Page = Home
}
TodoLineItemFilter = Filter {
	Expression = Expression {
		ConnectiveType = Enums.ConnectiveType.AND
		Expression = Expression {
			OperatorType = Enums.OperatorType.EQUALS
			Args = [Arg {
				ArgType = Enums.ArgType.REFERENCE
				ReferenceArg = ReferenceArg {
					Entity = Todo
					Property = Completed
				}
			}, Arg {
				ArgType = Enums.ArgType.STATIC
				PrimitiveArg = PrimitiveArg {
					AppType = Enums.AppType.BOOL
					Value = "False"
				}
			}]
		}
	}
}
TodoSaveGetListener = Listener {
	ListenerType = Enums.ListenerType.GET
	Event = TodoSaveEvent
	Filter = TodoLineItemFilter
}
TodoSaveSetListener = Listener {
	ListenerType = Enums.ListenerType.SET
	Event = TodoSaveEvent
}
TodoLineItem = Template {
	Components = [TodoField, TodoSave]
	Events = [TodoSaveEvent]
	Listeners = [TodoSaveGetListener, TodoSaveSetListener]
}
Home = Page
```

## Language Enums

### `ComponentType` domain
- STATIC
- DYNAMIC

### `ElementType` domain

- HEADER
- PARAGRAPH
- INPUT
- BUTTON
- CHECKBOX

### `EventType` domain
- ONLOAD
- CLICK
- MOUSEOVER
- MOUSEOUT
- MOUSEIN
- MOUSEOUT

### `ListenerType` domain
- GET
- SET

### `ConnectiveType` domain
- AND
- OR

### `OperatorType` domain
- EQUALS
- GTHAN
- GETHAN
- LTHAN
- LETHAN
- HAS
- LIKE

### `ArgType` domain
- STATIC
- EXPR
- REFERENCE
- CLIENT

### `AppType` domain
 - INT
 - FLOAT
 - BOOL
 - STRING
 - DATE
 - REFERENCE

## Language Keywords

### `Page` properties
- Templates (n >= 0): `Template[]`
- Name (n = 1): `String`

### `Template` properties
- Components (n >= 1): `Component[]`
- Events (n >= 0): `Event[]`
- Listeners (n >= 0): `Listener[]`

### `Component` properties
- ComponentType (n = 1): `ComponentType`
- ElementType (n = 1): `ElementType`
- Row (n >= 0); `Int`
- Column (0 <= n <= 12); `Int`
- Width (0 <= n <= 12); `Int`
- Offset (0 <= n <= 12); `Int`

### `Event` properties
- EventType (n = 1): `EventType`
- Component (n = 1): `Component`
- Page (n = 1): `Page`

### `Listener` properties
- ListenerType (n = 1): `ListenerType`
- Event (n = 1): `Event`
- Filters (n >= 0): `Filter[]`

### `Filter` properties
- Expression (0 <= n <= 1): `Expression`
- Listener (n = 1): `Listener`

### `Connective` properties
- ConnectiveType (n = 1): `ConnectiveType`
- Expressions (n = 2): `Expression[]`

### `Expression` properties
- OperatorType (n = 1): `OperatorType`
- Args (n >= 1): `Arg[]`

### `Arg` properties
- ArgType (n = 1): `ArgType`
- Expression ((0 <= n <= 1)): `Expression`
- Component (0 <= n <= 1): `Component`
- Reference (0 <= n <= 1): `ReferenceArg`
- Primitive (0 <= n <= 1): `PrimitiveArg`

### `ReferenceArg` properties
- Entity (n = 1): `Entity`
- Property (n = 1): `Property`

### `PrimitiveArg` properties
- AppType (n = 1): `AppType`
- Value (n = 1): `String`

### `Entity` properties
- Properties (n >= 1): `Property[]`
- Name (n = 1): `String`

### `Property` properties
- AppType (n = 1): `AppType`
- Name (n = 1): `String`
