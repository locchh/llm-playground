## Introduction

**AutoGen** is a framework for creating multi-agent AI applications that can act autonomously or work alongside humans.

**Framework Package Hierarchy**

```
autogen-framework/
├── autogen-core/                    # Core runtime and abstractions
│   └── src/autogen_core/
│       ├── __init__.py
│       ├── _base_agent.py          # Base agent classes
│       ├── _agent_runtime.py       # Runtime infrastructure
│       ├── _routed_agent.py        # Message routing
│       ├── _single_threaded_agent_runtime.py
│       ├── models/                 # Core model definitions
│       ├── tools/                  # Tool definitions
│       ├── memory/                 # Memory management
│       ├── code_executor/          # Code execution
│       ├── logging.py              # Logging utilities
│       └── utils/                  # Utility functions
│
├── autogen-agentchat/              # High-level agent and team implementations
│   └── src/autogen_agentchat/
│       ├── __init__.py
│       ├── agents/                 # Built-in agent implementations
│       │   ├── _base_chat_agent.py    # Base chat agent
│       │   ├── _assistant_agent.py    # Assistant agent
│       │   ├── _code_executor_agent.py # Code executor
│       │   ├── _user_proxy_agent.py   # User proxy
│       │   └── _society_of_mind_agent.py
│       ├── teams/                  # Team implementations
│       │   └── _group_chat/
│       │       ├── _base_group_chat.py      # Base team class
│       │       ├── _round_robin_group_chat.py
│       │       ├── _selector_group_chat.py
│       │       ├── _swarm_group_chat.py
│       │       └── _magentic_one/           # MagenticOne implementation
│       ├── messages.py             # Message types
│       ├── base/                   # Base classes
│       ├── conditions/             # Termination conditions
│       └── tools/                  # Agent tools
│
├── autogen-ext/                    # Extensions and integrations
│   └── src/autogen_ext/
│       ├── __init__.py
│       ├── agents/                 # Extended agent implementations
│       ├── models/                 # Model integrations (OpenAI, Anthropic, etc.)
│       ├── tools/                  # Extended tool implementations
│       ├── code_executors/         # Code execution environments
│       ├── memory/                 # Memory implementations
│       ├── cache_store/            # Caching implementations
│       ├── runtimes/               # Runtime implementations
│       ├── auth/                   # Authentication
│       ├── experimental/           # Experimental features
│       └── teams/                  # Extended team implementations
│
├── autogen-studio/                 # Web UI and development tools
├── autogen-magentic-one/          # MagenticOne specific implementations
├── autogen-test-utils/            # Testing utilities
├── pyautogen/                     # Legacy compatibility layer
├── agbench/                       # Benchmarking tools
├── magentic-one-cli/             # Command line interface
└── component-schema-gen/         # Schema generation tools
```

**Key Architectural Layers**

1. **Core Layer (autogen-core)**: Provides fundamental abstractions like BaseAgent, message routing, runtime infrastructure

2. **AgentChat Layer (autogen-agentchat)**: Implements concrete agents (Assistant, CodeExecutor, UserProxy) and teams (RoundRobin, Selector, Swarm)

3. **Extensions Layer (autogen-ext)**: Provides integrations with external services, models, tools, and specialized implementations

4. **Tools & Applications**: Higher-level applications like Studio (web UI), CLI tools, and benchmarking

**Package Dependencies**

- `autogen-agentchat` depends on `autogen-core`
- `autogen-ext` extends both core and agentchat
- Applications like `autogen-studio` build on top of the framework

## TODO

- *Systematize the concepts* ✅
- *Deep dive into the concepts*
- *Apply your learning in hands-on, real-world situations*

## References

[autogen-agentchat](https://microsoft.github.io/autogen/stable/user-guide/agentchat-user-guide)

[autogen-api-reference](https://microsoft.github.io/autogen/stable/reference/index.html)

[autogen-repo](https://github.com/microsoft/autogen)
