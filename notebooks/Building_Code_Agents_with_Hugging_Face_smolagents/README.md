# A Range of Agency Levels

| Agency Level | Description                                                 | How that's called    | Example Pattern                                           |
|--------------|-------------------------------------------------------------|----------------------|-----------------------------------------------------------|
| ☆☆☆          | LLM output has no impact on program flow                    | Simple processor     | `process_llm_output(llm_response)`                        |
| ★☆☆          | LLM output determines basic control flow                    | Router               | `if llm_decision(): path_a() else: path_b()`              |
| ★★☆          | LLM output determines function execution                    | Tool call            | `run_function(llm_chosen_tool, llm_chosen_args)`          |
| ★★★          | LLM output controls iteration and program continuation      | Multi-step Agent     | `while llm_should_continue(): execute_next_step()`        |
| ★★★          | One agentic workflow can start another agentic workflow     | Multi-Agent          | `if llm_trigger(): execute_agent()`                       |


Here is the content from the image converted into Markdown format:


# The Road Towards Increased Agency

| Level            | Description                                                                 | Example Code Snippet                                           | Date         | Milestone Paper                                                                 |
|------------------|-----------------------------------------------------------------------------|----------------------------------------------------------------|--------------|----------------------------------------------------------------------------------|
| **Router**        | LLM output controls if/else switch.                                        | —                                                              | 29 Mar 2022  | *Language Models that Seek for Knowledge: Modular Search & Generation for Dialogue and Prompt Completion* |
| **Tool-calling**  | LLM output determines function call:                                       | `run_function(llm_chosen_tool, llm_chosen_args)`              | 1 Jun 2022   | *WebGPT: Browser-assisted question-answering with human feedback*              |
| **Multi-step agent** | LLM output determines if the execution loop continues:                | `while post_process(llm_output):`<br>`    … # Perform an action` | 10 Mar 2023  | *ReAct: Synergizing Reasoning and Acting in Language Models*                   |
| **Code agent**    | LLM can write and execute code, defining new tools if needed.              | —                                                              | 7 Jun 2024   | *Executable Code Actions Elicit Better LLM Agents*                             |


Would you like me to merge this with the previous table into one file or keep them separate?
