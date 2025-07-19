# AutoGen Message Types

This document provides a comprehensive guide to the message types available in AutoGen's AgentChat module. Messages are the primary means of communication between agents and for signaling internal events.

## Types of Messages
At a high level, messages in AgentChat can be categorized into two types: agent-agent messages and an agent’s internal events and messages.

### Agent-Agent Messages
AgentChat supports many message types for agent-to-agent communication. They belong to subclasses of the base class `BaseChatMessage`. Concrete subclasses covers basic text and multimodal communication, such as `TextMessage` and `MultiModalMessage`.

### Agent Events
AgentChat also supports the concept of events - messages that are internal to an agent. These messages are used to communicate events and information on actions **within the agent itself**, and belong to subclasses of the base class `BaseAgentEvent`.

Examples of these include `ToolCallRequestEvent`, which indicates that a request was made to call a tool, and `ToolCallExecutionEvent`, which contains the results of tool calls.

Typically, events are created by the agent itself and are contained in the `inner_messages` field of the `Response` returned from `on_messages`. If you are building a custom agent and have events that you want to communicate to other entities (e.g., a UI), you can include these in the `inner_messages` field of the `Response`. We will show examples of this in Custom Agents.

You can read about the full set of messages supported in AgentChat in the [messages module](https://microsoft.github.io/autogen/stable/reference/python/autogen_agentchat.messages.html#module-autogen_agentchat.messages).

## Custom Message Types
You can create custom message types by subclassing the base class `BaseChatMessage` or `BaseAgentEvent`. This allows you to define your own message formats and behaviors, tailored to your application. Custom message types are useful when you write custom agents.

## Class Inheritance Tree

```
BaseMessage (ABC)
├── BaseChatMessage (ABC)
│   ├── BaseTextChatMessage (ABC)
│   │   └── TextMessage
│   │
│   ├── MultiModalMessage
│   ├── StopMessage
│   ├── HandoffMessage
│   ├── ToolCallSummaryMessage
│   └── StructuredMessage[StructuredContentType]
│
└── BaseAgentEvent (ABC)
    ├── ToolCallRequestEvent
    ├── CodeGenerationEvent
    ├── CodeExecutionEvent
    ├── ToolCallExecutionEvent
    ├── UserInputRequestedEvent
    ├── MemoryQueryEvent
    ├── ModelClientStreamingChunkEvent
    ├── ThoughtEvent
    ├── SelectSpeakerEvent
    └── SelectorEvent
```

### Key Points:
1. **BaseMessage** is the root abstract base class for all messages and events.
2. **BaseChatMessage** is for agent-to-agent communication.
3. **BaseAgentEvent** is for signaling internal agent events.
4. `StructuredMessage` is a generic class that works with any Pydantic model.
5. `BaseTextChatMessage` is an abstract base class specifically for text-based messages.
6. All event types inherit from `BaseAgentEvent` and are used for internal agent state changes.

## Base Message Classes

### `BaseMessage`
The abstract base class for all message types in AgentChat. Provides core functionality for message serialization and deserialization.

**Key Methods:**
- `to_text()`: Converts message content to a string representation
- `dump()`: Converts the message to a JSON-serializable dictionary
- `load()`: Creates a message from a dictionary of JSON-serializable data

### `BaseChatMessage`
Abstract base class for all chat messages used in agent-to-agent communication.

**Key Attributes:**
- `id`: Unique identifier for the message
- `source`: Name of the agent that sent the message
- `models_usage`: Model client usage information
- `metadata`: Additional metadata about the message
- `created_at`: Timestamp of message creation

**Key Methods:**
- `to_model_text()`: Converts content to text-only representation for models
- `to_model_message()`: Converts to a `UserMessage` for model clients

### `BaseTextChatMessage`
Base class for text-only chat messages with implementations for text conversion methods.

**Key Attributes:**
- `content`: The text content of the message

### `BaseAgentEvent`
Base class for agent events, which are used to signal observable events to users and applications.

**Key Attributes:**
- `id`: Unique identifier for the event
- `source`: Name of the agent that generated the event
- `models_usage`: Model client usage information
- `metadata`: Additional metadata
- `created_at`: Timestamp of event creation

## Concrete Message Types

### `TextMessage`
A simple text message with string content.

**Usage:**
```python
from autogen_agentchat.messages import TextMessage

message = TextMessage(content="Hello, world!", source="agent1")
```

### `MultiModalMessage`
A message that can contain both text and images.

**Usage:**
```python
from autogen_agentchat.messages import MultiModalMessage, Image
from PIL import Image as PILImage

# Create an image
img = Image(PILImage.open("example.png"))
message = MultiModalMessage(content=["Here's an image:", img], source="agent1")
```

### `StructuredMessage`
A generic message type for structured content using Pydantic models.

**Usage:**
```python
from pydantic import BaseModel
from autogen_agentchat.messages import StructuredMessage

class OrderDetails(BaseModel):
    order_id: str
    items: list[str]
    total: float

order = OrderDetails(order_id="123", items=["item1", "item2"], total=99.99)
message = StructuredMessage[OrderDetails](
    content=order,
    source="order_processor",
    format_string="New order #{content.order_id} with {len(content.items)} items. Total: ${content.total}"
)
```

### `StopMessage`
A message requesting the conversation to stop.

**Usage:**
```python
from autogen_agentchat.messages import StopMessage

stop_msg = StopMessage(source="agent1")
```

### `HandoffMessage`
A message requesting handoff to another agent.

**Usage:**
```python
from autogen_agentchat.messages import HandoffMessage

handoff = HandoffMessage(
    target="specialist_agent",
    context=[previous_messages],
    source="general_agent"
)
```

## Event Types

### `ToolCallRequestEvent`
Signals a request to use tools.

### `ToolCallExecutionEvent`
Signals the execution of tool calls.

### `CodeGenerationEvent`
Signals code generation by an agent.

### `CodeExecutionEvent`
Signals code execution by an agent.

### `UserInputRequestedEvent`
Signals that user input has been requested.

### `MemoryQueryEvent`
Signals the results of memory queries.

### `ModelClientStreamingChunkEvent`
Signals a text output chunk from a model client in streaming mode.

### `ThoughtEvent`
Signals the thought process of a model.

### `SelectSpeakerEvent`
Signals the selection of speakers for a conversation.

## Utilities

### `MessageFactory`
A factory for creating messages from JSON-serializable dictionaries, useful for deserialization.

### `StructuredMessageFactory`
A component that creates structured chat messages from Pydantic models or JSON schemas.

**Usage:**
```python
from autogen_agentchat.messages import StructuredMessageFactory

# Define a schema
schema = {
    "type": "object",
    "properties": {
        "name": {"type": "string"},
        "age": {"type": "integer"}
    },
    "required": ["name"]
}

# Create a factory
factory = StructuredMessageFactory(
    json_schema=schema,
    format_string="User {name} is {age} years old."
)

# Create a message
message = factory.StructuredMessage(
    source="system",
    content=factory.ContentModel(name="Alice", age=30)
)
```

## Best Practices

1. Use `TextMessage` for simple text-based communication
2. Use `StructuredMessage` for complex, typed data exchange
3. Use appropriate event types to signal agent state changes
4. Implement custom message types by subclassing `BaseChatMessage` or `BaseAgentEvent`
5. Use the factory pattern for message deserialization