# Agents in AutoGen

In AutoGen, an agent is an entity that can send and receive messages, and act on them. This document provides an overview of the core agent types available in the `autogen_agentchat` library.

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

## Teams in AutoGen

In AutoGen, a **Team** (or **Group Chat**) allows multiple agents to collaborate to solve a task. The framework provides different strategies for managing the conversation flow and deciding which agent speaks next.

### `RoundRobinGroupChat`

The `RoundRobinGroupChat` is the simplest way to form a team. Agents take turns speaking in a fixed, round-robin order. This is useful for structured conversations where each agent needs to contribute in a predictable sequence.

**Key Features:**
- **Sequential Turns**: Agents speak one after another in the order they are provided.
- **Predictable Flow**: The conversation order is deterministic.
- **Termination**: The chat can end based on a `max_turns` limit or a custom `termination_condition`.

**Usage Example:**
```python
import asyncio
from autogen_agentchat.agents import AssistantAgent
from autogen_agentchat.teams import RoundRobinGroupChat
from autogen_agentchat.conditions import TextMentionTermination
from autogen_core.llm import ModelClient

async def main():
    model_client = ModelClient(model="gpt-4")

    # Create the agents
    agent1 = AssistantAgent("agent1", model_client=model_client, system_message="You are agent 1.")
    agent2 = AssistantAgent("agent2", model_client=model_client, system_message="You are agent 2.")

    # Define a condition for the chat to terminate
    termination_condition = TextMentionTermination(text="TERMINATE")

    # Form a round-robin team
    team = RoundRobinGroupChat(
        participants=[agent1, agent2],
        termination_condition=termination_condition
    )

    # Start the chat
    async for message in team.run_stream(task="Agent 1, say hello. Agent 2, say goodbye."):
        print(f"{message.source}: {message.text_content}")

asyncio.run(main())
```

### `SelectorGroupChat`

The `SelectorGroupChat` offers a more dynamic way to manage conversations. It uses an LLM to intelligently select the next agent to speak based on the current context of the conversation. This is ideal for complex, multi-step tasks where the next action is not always predictable.

**Key Features:**
- **Intelligent Speaker Selection**: An LLM analyzes the conversation and chooses the most appropriate agent to speak next.
- **Flexible Conversation Flow**: The order of speakers is determined dynamically, allowing for more natural and efficient problem-solving.
- **Customizable Prompt**: You can guide the LLM's selection process with a custom `selector_prompt`.

**Usage Example:**
```python
import asyncio
from autogen_agentchat.agents import AssistantAgent, UserProxyAgent
from autogen_agentchat.teams import SelectorGroupChat
from autogen_core.llm import ModelClient

async def main():
    model_client = ModelClient(model="gpt-4")

    # The user proxy agent will act as the entry point for the user's request
    user_proxy = UserProxyAgent("user_proxy")

    # The coder will write the code
    coder = AssistantAgent(
        "coder",
        model_client=model_client,
        system_message="You are a senior developer. Write python code to solve the task."
    )

    # The executor will run the code
    executor = AssistantAgent(
        "executor",
        model_client=model_client,
        system_message="You execute the code and report the results."
    )

    # Form a team with an LLM-based selector
    team = SelectorGroupChat(
        participants=[user_proxy, coder, executor],
        model_client=model_client,
        selector_prompt="Based on the conversation, who should speak next?"
    )

    # Start the chat
    async for message in team.run_stream(task="Write a python script to print 'Hello, AutoGen!' and then run it."):
        print(f"{message.source}: {message.text_content}")

asyncio.run(main())
```

## Conclusion

You have now learned about the core building blocks of AutoGen: **Messages** and **Agents**. You've seen how to create different types of agents and how to orchestrate them into **Teams** to collaboratively solve complex tasks. With this knowledge, you are ready to start building your own multi-agent applications with AutoGen.
