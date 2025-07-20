# Teams in AutoGen


In AutoGen, a **Team** (or **Group Chat**) allows multiple agents to collaborate to solve a task. The framework provides different strategies for managing the conversation flow and deciding which agent speaks next.

## Team Types

### 1. `RoundRobinGroupChat`

The `RoundRobinGroupChat` is the simplest way to form a team. Agents take turns speaking in a fixed, round-robin order. This is useful for structured conversations where each agent needs to contribute in a predictable sequence.

**Key Features:**
- **Sequential Turns**: Agents speak one after another in the order they are provided.
- **Predictable Flow**: The conversation order is deterministic.
- **Use Case**: Ideal for simple, linear workflows.

**Usage Example:**
```python
import asyncio
from autogen_agentchat.agents import AssistantAgent
from autogen_agentchat.teams import RoundRobinGroupChat

async def main():
    agent1 = AssistantAgent("agent1", system_message="You are agent 1.")
    agent2 = AssistantAgent("agent2", system_message="You are agent 2.")

    team = RoundRobinGroupChat(participants=[agent1, agent2])

    async for message in team.run_stream(task="Agent 1, say hello. Agent 2, say goodbye."):
        print(f"{message.source.name}: {message.text_content}")

# asyncio.run(main())
```

### 2. `SelectorGroupChat`

The `SelectorGroupChat` offers a more dynamic way to manage conversations. It uses an LLM to intelligently select the next agent to speak based on the current context of the conversation.

**Key Features:**
- **Intelligent Speaker Selection**: An LLM analyzes the conversation and chooses the most appropriate agent to speak next.
- **Flexible Conversation Flow**: The order of speakers is determined dynamically.
- **Use Case**: Best for complex, multi-step tasks where the next action is not always predictable.

**Usage Example:**
```python
import asyncio
from autogen_agentchat.agents import AssistantAgent, UserProxyAgent
from autogen_agentchat.teams import SelectorGroupChat
from autogen_core.llm import ModelClient

async def main():
    model_client = ModelClient(model="gpt-4")
    user_proxy = UserProxyAgent("user_proxy")
    coder = AssistantAgent("coder", model_client=model_client, system_message="You are a developer.")

    team = SelectorGroupChat(
        participants=[user_proxy, coder],
        model_client=model_client,
        selector_prompt="Based on the conversation, who should speak next?"
    )

    async for message in team.run_stream(task="Write a python script to print 'Hello, AutoGen!'"):
        print(f"{message.source.name}: {message.text_content}")

# asyncio.run(main())
```

### 3. `Swarm`

A `Swarm` team uses a `HandoffMessage` to explicitly pass control from one agent to another. This provides a structured but flexible way to guide the conversation, as the agent currently speaking decides who should speak next.

**Key Features:**
- **Explicit Handoff**: Control is passed via a dedicated `HandoffMessage`.
- **Directed Conversation**: The current speaker directs the flow of the conversation.
- **Use Case**: Suitable for workflows where the next step is clear to the current agent.

**Usage Example:**
```python
import asyncio
from autogen_agentchat.agents import AssistantAgent
from autogen_agentchat.teams import Swarm
from autogen_agentchat.messages import HandoffMessage

async def main():
    agent1 = AssistantAgent("agent1", system_message="I am agent 1. I will hand off to agent 2.")
    agent2 = AssistantAgent("agent2", system_message="I am agent 2. I will finish the task.")

    team = Swarm(participants=[agent1, agent2])

    task = HandoffMessage(source="user", target="agent1", content="Start the task.")

    async for message in team.run_stream(task=task):
        print(f"{message.source.name}: {message.text_content}")

# asyncio.run(main())
```

### 4. `MagenticOneGroupChat`

This is the most advanced team type. It uses a sophisticated **orchestrator** that maintains a **ledger** of facts and a dynamic **plan** to guide the conversation. The orchestrator can re-plan if the team gets stuck, making it robust for complex, open-ended tasks.

**Key Features:**
- **Ledger-Based Orchestration**: Maintains a structured record of facts and a plan.
- **Dynamic Re-planning**: The orchestrator can adapt the plan if progress stalls.
- **Use Case**: Designed for complex, long-running tasks that require reasoning and planning.

**Usage Example:**
```python
import asyncio
from autogen_agentchat.agents import AssistantAgent
from autogen_agentchat.teams import MagenticOneGroupChat
from autogen_core.llm import ModelClient

async def main():
    model_client = ModelClient(model="gpt-4")
    researcher = AssistantAgent("researcher", model_client=model_client, system_message="I am a researcher.")

    team = MagenticOneGroupChat(
        participants=[researcher],
        model_client=model_client
    )

    async for message in team.run_stream(task="What is the airspeed velocity of an unladen swallow?"):
        print(f"{message.source.name}: {message.text_content}")

# asyncio.run(main())
```

## Team lifecycle

### Creating a Team

A team is a collection of agents that work together. To create a team, you need to define the participants.

```python
from autogen_agentchat.agents import AssistantAgent
from autogen_agentchat.teams import RoundRobinGroupChat

# Create the agents
agent1 = AssistantAgent("agent1", system_message="You are agent 1.")
agent2 = AssistantAgent("agent2", system_message="You are agent 2.")

# Form a round-robin team
team = RoundRobinGroupChat(
    participants=[agent1, agent2]
)
```

### Running a Team

Once you have a team, you can run it to perform a task using the `run` or `run_stream` method.

```python
import asyncio

async def main():
    # Start the chat stream
    async for message in team.run_stream(task="Agent 1, say hello. Agent 2, say goodbye."):
        print(f"{message.source.name}: {message.text_content}")

# asyncio.run(main())
```

### Observing a Team

You can observe the team's progress by printing the messages as they are generated from the stream.

### Resetting a Team

You can reset a team to its initial state, which clears the conversation history.

```python
await team.reset()
```

### Stopping a Team

You can stop a team's execution. This is useful when you want to manually intervene or end the task prematurely.

```python
# This would typically be called from a separate task or callback
# For demonstration, we assume the team is running and we have a reference to it.
# team.stop()
```

### Resuming a Team

After stopping a team, you can resume its execution from where it left off.

```python
# This would also be called from a separate context
# async for message in team.resume_stream():
#     print(f"{message.source.name}: {message.text_content}")
```

### Aborting a Team

Aborting a team stops its execution immediately and prevents it from being resumed.

```python
# team.abort()
```

### Single-Agent Team

You can create a team with a single agent. This is useful for tasks where you want to leverage the team's lifecycle management features (like termination conditions) for a single agent.

```python
from autogen_agentchat.conditions import TextMessageTermination

# Define a condition for the chat to terminate
termination_condition = TextMessageTermination(text="TERMINATE")

single_agent_team = RoundRobinGroupChat(
    participants=[agent1],
    termination_condition=termination_condition
)
```

The key is to focus on the termination condition. In this example, we use a `TextMessageTermination` condition that stops the team when the agent produces a message containing the word "TERMINATE". You can use other termination conditions to control the agent. See the documentation on Termination Conditions for more details.
