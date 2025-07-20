# Agents in AutoGen

In AutoGen, an agent is an entity that can send and receive messages, and act on them. This document provides an overview of the core agent types available in the `autogen_agentchat` library.

## Agent Abilities

AutoGen agents are equipped with a variety of powerful abilities that allow them to handle complex tasks. Here are some of the key features with examples:

- **Multi-Modal Input**: Agents can process not just text, but also images. You can include image URLs in the content for the agent to analyze.

  ```python
  import asyncio
  from autogen_agentchat.agents import AssistantAgent
  from autogen_agentchat.messages import ImageMessage

  async def main():
      agent = AssistantAgent("assistant")
      image_message = ImageMessage(content="What is in this image?", image_url="http://example.com/image.png")
      response = await agent.run(task=image_message)
      print(response.text_content)

  # asyncio.run(main())
  ```

- **Tool Use & Workbench**: Equip agents with tools to extend their capabilities. The workbench provides a sandboxed environment for code execution.

  ```python
  import asyncio
  from autogen_agentchat.agents import AssistantAgent
  from autogen_agentchat.tools.web_search import WebSearch

  async def main():
      web_search_tool = WebSearch()
      agent = AssistantAgent("assistant", tools=[web_search_tool])
      response = await agent.run(task="Search for 'AutoGen Framework' on the web.")
      print(response.text_content)

  # asyncio.run(main())
  ```

- **Streaming Responses**: Stream responses for real-time interaction, receiving tokens or messages as they are generated.

  ```python
  import asyncio
  from autogen_agentchat.agents import AssistantAgent

  async def main():
      agent = AssistantAgent("assistant")
      async for message in agent.run_stream(task="Tell me a short story."):
          if message.text_content:
              print(message.text_content, end="", flush=True)

  # asyncio.run(main())
  ```

- **Structured Output**: Force agents to return structured data like JSON or Pydantic models for predictable and reliable outputs.

  ```python
  import asyncio
  from pydantic import BaseModel
  from autogen_agentchat.agents import AssistantAgent

  class UserProfile(BaseModel):
      name: str
      email: str

  async def main():
      agent = AssistantAgent("assistant")
      response = await agent.run(task="Create a user profile for John Doe with email john.doe@example.com", response_model=UserProfile)
      if isinstance(response.content, UserProfile):
          print(f"Name: {response.content.name}, Email: {response.content.email}")

  # asyncio.run(main())
  ```

- **Model Context Management**: Control the conversation history sent to the LLM to manage token usage and costs.

  ```python
  import asyncio
  from autogen_agentchat.agents import AssistantAgent
  from autogen_core.model_context import BufferedChatCompletionContext

  async def main():
      # Only use the last 5 messages in the context
      agent = AssistantAgent(
          name="assistant",
          model_context=BufferedChatCompletionContext(buffer_size=5)
      )
      # ... run the agent

  # asyncio.run(main())
  ```

## Core Agent Types

### `BaseChatAgent`

The `BaseChatAgent` is the abstract base class for all chat agents in AutoGen. It defines the essential interface and lifecycle for any agent participating in a chat.

**Key Characteristics:**
- **Abstract**: You cannot use this class directly. Instead, you create new agents by subclassing it.
- **Stateful**: Agents are designed to be stateful, maintaining their internal state between messages.
- **Core Methods**: Subclasses must implement key methods like `on_messages` (to handle incoming messages), `on_reset` (to reset the agent's state), and `produced_message_types` (to declare what kind of messages the agent can send).

### `AssistantAgent`

The `AssistantAgent` is the primary workhorse agent in AutoGen. It is a versatile, LLM-powered agent capable of solving tasks, using tools, and generating human-like responses.

**Key Features:**
- **LLM-Powered**: Uses a language model (like GPT-4) to understand context, generate responses, and decide on actions.
- **Tool Use**: Can be equipped with a `Workbench` containing tools (e.g., `CodeExecutor`, custom functions) to perform actions beyond text generation.
- **Multi-Modal**: Can process messages containing both text and images.
- **Streaming**: Supports streaming responses for real-time applications.
- **Structured Output**: Can be configured to produce responses that conform to a specific Pydantic model.

**Usage Example:**
```python
from autogen_agentchat import AssistantAgent
from autogen_core.llm import ModelClient

# Create an agent powered by an LLM
assistant = AssistantAgent(
    name="assistant",
    model_client=ModelClient(model="gpt-4"),
    system_message="You are a helpful assistant that can solve tasks and write code.",
)
```

### `UserProxyAgent`

The `UserProxyAgent` acts as a proxy for a human user, allowing for human-in-the-loop interactions within an agent conversation.

**Key Features:**
- **Human Input**: Its primary role is to solicit input from a human. It does this via a customizable `input_func`.
- **Blocking**: When this agent is active, it typically pauses the agent workflow until the human provides a response.
- **Handoffs**: It is often used in handoff scenarios where an automated agent needs to ask a human for clarification, approval, or the next step.

**Usage Example:**
```python
from autogen_agentchat.agents import UserProxyAgent

# This agent will prompt the human for input in the console by default
human_proxy = UserProxyAgent(
    name="human_proxy",
    description="A proxy for a human user to provide input."
)
```

### `CodeExecutorAgent`

The `CodeExecutorAgent` is a specialized agent designed to execute code blocks found in messages.

**Key Features:**
- **Code Execution**: Its main purpose is to run code snippets using a configured `CodeExecutor` (e.g., `DockerCommandLineCodeExecutor`).
- **Two Modes of Operation**:
    1.  **Execution-Only**: If not provided with a `model_client`, it will only execute code blocks from incoming messages.
    2.  **Generate and Execute**: If a `model_client` is provided, it can generate code based on a prompt, execute it, and even reflect on the results to correct errors.
- **Security**: It is highly recommended to use a sandboxed environment, like Docker, for execution to prevent security risks.

**Usage Example:**
```python
from autogen_agentchat.agents import CodeExecutorAgent
from autogen_ext.code_executors.docker import DockerCommandLineCodeExecutor

# Create a code executor in a Docker container
code_executor = DockerCommandLineCodeExecutor(container_name="my-autogen-container")

# Create an agent to execute code
code_executor_agent = CodeExecutorAgent(
    name="code_executor",
    code_executor=code_executor,
)
```

## Agent Execution Methods

When working with AutoGen agents, it's important to understand the key methods that drive their execution. These methods operate at different levels of abstraction, from high-level task execution to low-level model interaction.

### High-Level Task Execution: `run` and `run_stream`

These are the primary methods you will use to initiate a task with an agent or a team.

*   **`run(task: ...)`**: Executes a task from start to finish and returns the final result. This method is best when you only need the complete outcome and don't require real-time updates. It returns a `TaskResult` object containing the full conversation history.

*   **`run_stream(task: ...)`**: Executes a task and provides a live stream of events. This method is ideal for interactive applications where you want to display results as they are generated. It returns an `AsyncGenerator` that yields messages and events, with the final item being the `TaskResult`.

### Mid-Level Agent Logic: `on_messages` and `on_messages_stream`

These methods define the core internal logic of an agent. They are called by the agent runtime system and are typically not invoked directly in user code.

*   **`on_messages(messages: ...)`**: Processes a batch of incoming messages and returns a single, complete `Response` object. The `run` method uses this internally.

*   **`on_messages_stream(messages: ...)`**: Processes incoming messages and returns a stream of intermediate events (like tool calls or thoughts) before yielding the final `Response`. The `run_stream` method relies on this for its live updates.

### Low-Level Model Interaction: `create`

This method is not part of the agent itself but belongs to the **`ModelClient`** that the agent uses to communicate with the LLM.

*   **`create(messages: ...)`**: Sends a request to the LLM API and waits for a single, complete response. Agents like `AssistantAgent` call this on their `model_client` to get the model's reply. The streaming equivalent is `create_stream`.

### Summary of the Call Hierarchy

The relationship between these methods can be summarized as follows:

1.  You call **`run()`** or **`run_stream()`** to start a task.
2.  The runtime system invokes the agent's **`on_messages()`** or **`on_messages_stream()`** method.
3.  The agent's logic then calls **`create()`** or **`create_stream()`** on its `model_client` to get a response from the LLM.

This layered architecture provides both a simple, high-level API for running tasks and the flexibility to customize the agent's internal behavior.