### Basic Usages

- Summary
- Extract
- Infering
- Transform, Translate
- Expanding
- Completion

### Function calling

#### Create chatbots that answer questions by calling external tools (e.g., like ChatGPT Plugins)

- Convert queries such as *Email Anya to see if she wants to get coffee next Friday* to a function call like `send_email(to: string, body: string)`

- *What’s the weather like in Boston?* to `get_current_weather(location: string, unit: 'celsius' | 'fahrenheit')`

#### Convert natural language into API calls or database queries

- Convert *Who are my top ten customers this month?* to an internal API call such as `get_customers_by_revenue(start_date: string, end_date: string, limit: int)`

- *How many orders did Acme, Inc. place last month?* to a SQL query using `sql_query(query: string)`

#### Extract structured data from text

- Define a function called `extract_people_data(people: [{name: string, birthday: string, location: string}])`, to extract all people mentioned in a Wikipedia article.

### Prompting Principles

#### Principle 1: Write clear and specific instructions

- **Tactic 1**: Use delimiters to clearly indicate distinct parts of the input
- **Tactic 2**: Ask for a structured output
- **Tactic 3**: Ask the model to check whether conditions are satisfied
- **Tactic 4**: "Few-shot" prompting

#### Principle 2: Give the model time to “think”

- **Tactic 1**: Specify the steps required to complete a task
- **Tactic 2**: Instruct the model to work out its own solution before rushing to a conclusion

### Refs

https://github.com/locchh/llm-playground/blob/main/notebooks/Evaluating_AI_Agents/L3.ipynb

https://cookbook.openai.com/examples/orchestrating_agents


[chat completions endpoint](https://platform.openai.com/docs/guides/chat)

[reasoning](https://platform.openai.com/docs/guides/reasoning)

[structured-outputs](https://platform.openai.com/docs/guides/structured-outputs)

[function calling](https://platform.openai.com/docs/guides/function-calling)

[orchestrating_agents](https://cookbook.openai.com/examples/orchestrating_agents)

[openai docs](https://platform.openai.com/docs/overview)

[json-schema](https://json-schema.org/)