# Designing Effective Prompts and Datasets for LLMs

This document outlines essential aspects of working with prompts for Large Language Models (LLMs), covering prompt components, advanced design strategies, and considerations for dataset preparation for fine-tuning or alignment training.

---

## 1. System Prompt

### Description:

A system prompt sets the context, tone, behavior, or identity of the assistant. It helps guide the model's response style and capabilities.

### Example:

```json
{"role": "system", "content": "You are a helpful assistant."}
```

---

## 2. Role

### Description:

Each conversational turn is assigned a role indicating the speaker or source of input. Common roles include:

*   `system` – Establishes context.
*   `user` – Poses a query or instruction.
*   `assistant` – Provides the response.
*   `tool_call` – Represents a call to an external tool.
*   `tool_response` – Returns a tool's result.

### Example:

```json
{"role": "user", "content": "What is the capital of France?"}
```

---

## 3. Instruction

### Description:

Instructions are direct commands or requests to the model, used either in chat format or standalone dataset entries. They often appear in instruction-tuning datasets.

### Example (Instruction format):

```json
{
  "instruction": "Translate the following sentence into French.",
  "input": "Where is the library?",
  "output": "Où est la bibliothèque ?"
}
```

---

## 4. Few-shot Examples

### Description:

Few-shot examples are demonstrations included within a prompt to help the model understand the desired format and reasoning. These are especially helpful in zero-shot and few-shot prompting for inference.

### Example:

```text
Instruction:
Summarize the following text.

Example:
Text: "The cat sat on the mat."
Summary: "A cat was sitting on a mat."

Now try this one:
Text: "The sun rises in the east and sets in the west."
Summary:
```

---

## 5. Detail Data for f-String (Template Prompting)

### Description:

In application code, prompts are often dynamically constructed using Python f-strings or other templating methods. This makes it easy to insert variables or custom data into prompt structures.

### Example:

```python
instruction = "Explain how {concept} works in {language}."
prompt = instruction.format(concept="recursion", language="Python")
```

### Result:

```text
Explain how recursion works in Python.
```

---

## Summary Table (Prompt Components)

| Component         | Used In         | Purpose                                                       |
| ----------------- | --------------- | ------------------------------------------------------------- |
| System Prompt     | Chat, SFT       | Defines assistant behavior                                    |
| Role              | Chat, SFT       | Indicates who is speaking                                     |
| Instruction       | Inference, SFT  | Main task or directive                                        |
| Few-shot Examples | Prompting       | Guide model behavior through examples                         |
| f-string Details  | Prompt building | Enables dynamic generation of prompts with variable insertion |

---

## References and Usage (Prompt Components)

*   **OpenAI ChatML**: Uses `system`, `user`, and `assistant` roles in prompt structure.
*   **LLaMA 3 Chat**: Uses structured message format similar to OpenAI's.
*   **FLAN, Alpaca, Self-Instruct**: Use instruction-based datasets with `instruction`, `input`, and `output` fields.
*   **Prompt Engineering**: Few-shot and dynamic prompting using f-strings is common in production and research.

This component structure helps both in real-time interactions with LLMs and in training them via supervised fine-tuning or instruction tuning datasets.

---

## Advanced Prompt Design Strategies

Effective prompt design goes beyond simply listing components. It involves crafting prompts that elicit the desired behavior from the LLM with high accuracy and reliability.

### 1. Clarity and Specificity
-   **Description**: Be as clear and specific as possible in your instructions. Ambiguity leads to unpredictable responses. Define the task, context, expected output, and any constraints.
-   **Example**:
    -   *Less Clear*: "Write about dogs."
    -   *More Clear*: "Write a 500-word blog post about the benefits of adopting a rescue dog, focusing on their adaptability and the positive impact on both the dog and the adopter. The tone should be informative and heartwarming."

### 2. Iterative Refinement
-   **Description**: Prompt engineering is often an iterative process. Start with a simple prompt, test it, analyze the output, and refine the prompt based on the results. Experiment with different phrasings, components, and examples.
-   **Example**: If a summary prompt is too long, add a constraint: "Summarize the following text in no more than 100 words."

### 3. Chain-of-Thought (CoT) Prompting
-   **Description**: Encourage the model to "think step by step" by providing examples where the reasoning process is explicitly laid out before the final answer. This is particularly useful for complex reasoning tasks.
-   **Example**:
    ```text
    Question: Roger has 5 tennis balls. He buys 2 more cans of tennis balls. Each can has 3 tennis balls. How many tennis balls does he have now?
    Answer: Roger started with 5 balls. He bought 2 cans, and each can has 3 balls, so that's 2 * 3 = 6 more balls. Therefore, Roger now has 5 + 6 = 11 tennis balls.

    Question: The cafeteria had 23 apples. If they used 20 to make lunch and bought 6 more, how many apples do they have?
    Answer:
    ```
    (The model is expected to follow the step-by-step reasoning)

### 4. Self-Consistency
-   **Description**: A technique that complements CoT. Generate multiple diverse reasoning paths for the same prompt (e.g., by using a higher temperature) and then select the most consistent answer among the outputs. This improves accuracy on complex reasoning tasks.

### 5. Role Playing
-   **Description**: Assigning a persona or role to the LLM (often via the system prompt) can significantly shape its responses, making them more suitable for specific applications.
-   **Example**: "You are a Socratic tutor. Engage the user in a dialogue to help them understand the concept of photosynthesis, without directly giving them the answer."

### 6. Using Delimiters
-   **Description**: Use clear delimiters (e.g., triple backticks ` ``` `, XML tags `<example>`, hashes `###`) to separate different parts of your prompt, especially when providing context, instructions, and examples, or when input text might be ambiguous.
-   **Example**:
    ```text
    Summarize the text below, delimited by triple backticks, into a single sentence.
    ```
    The quick brown fox jumps over the lazy dog.
    ```
    ```

### 7. Specifying Output Format
-   **Description**: Clearly instruct the model on the desired output format (e.g., JSON, XML, markdown list, specific structure). This is crucial for programmatic use of LLM outputs.
-   **Example**: "Extract the names of all people and organizations mentioned in the following text. Provide the output as a JSON object with two keys: 'people' and 'organizations', where each key holds a list of strings."

### 8. Providing Context/Background Information
-   **Description**: If the task requires external knowledge or specific background, provide it within the prompt. Don't assume the model knows everything or has access to the latest information.
-   **Example**: "Given the following company policy document (sections A and B), answer the user's question about vacation days."

### 9. Constraints and Guardrails
-   **Description**: Explicitly state what the model *should not* do, or what topics to avoid. This helps in aligning the model's output with safety guidelines or specific requirements.
-   **Example**: "You are a customer service assistant for an electronics store. Answer questions about products and store policies. Do not provide medical or legal advice."

---

## Dataset Design for LLM Fine-Tuning and Alignment

High-quality datasets are crucial for successful LLM fine-tuning (SFT) and alignment (e.g., RLHF). The design of these datasets directly impacts the model's performance, capabilities, and safety.

### 1. Data Sourcing and Collection
-   **Description**: Identify diverse and relevant data sources. This can include publicly available datasets, proprietary data, web scrapes, or human-generated content.
-   **Considerations**:
    -   **Relevance**: Data should align with the target domain and tasks.
    -   **Diversity**: Cover a wide range of topics, styles, and complexities.
    -   **Scale**: Sufficient data is needed, but quality often trumps quantity.

### 2. Data Cleaning and Preprocessing
-   **Description**: Raw data is often noisy and requires cleaning. This includes removing duplicates, correcting errors, filtering irrelevant content, and normalizing text.
-   **Techniques**:
    -   Deduplication
    -   PII (Personally Identifiable Information) removal/masking
    -   Filtering low-quality or toxic content
    -   Text normalization (e.g., lowercasing, punctuation handling, if appropriate for the task)

### 3. Instruction-Tuning Datasets
-   **Description**: These datasets typically consist of (instruction, input, output) triplets. The goal is to teach the model to follow instructions effectively.
-   **Examples**: Alpaca, Dolly, FLAN, Self-Instruct.
-   **Key Elements**:
    -   **Instruction**: A clear directive for the model.
    -   **Input (Optional)**: Context or data for the instruction.
    -   **Output**: The desired response to the instruction and input.
-   **Generation**: Can be human-written, model-generated (e.g., using Self-Instruct with a powerful teacher model), or a combination.

### 4. Preference Datasets for RLHF
-   **Description**: Used for Reinforcement Learning from Human Feedback. These datasets consist of prompts and multiple responses, where human annotators rank or choose the preferred response.
-   **Format**: Typically (prompt, chosen_response, rejected_response).
-   **Purpose**: To train a reward model that learns human preferences, which then guides the LLM's fine-tuning.

### 5. Quality Control and Annotation
-   **Description**: Ensuring data quality is paramount. Human annotation is often required for creating instructions, generating high-quality responses, or ranking preferences.
-   **Best Practices**:
    -   Clear annotation guidelines.
    -   Multiple annotators for consistency checks (Inter-Annotator Agreement).
    -   Iterative review and feedback loops.

### 6. Data Formatting
-   **Description**: Datasets are typically stored in structured formats like JSON Lines (JSONL), CSV, or Parquet.
-   **JSONL Example for Instruction Tuning**:
    ```json
    {"instruction": "What is the capital of France?", "input": "", "output": "The capital of France is Paris."}
    {"instruction": "Summarize the following text.", "input": "Large language models are powerful AI systems...", "output": "Large language models are advanced AI capable of various text tasks."}
    ```

### 7. Handling Bias and Safety
-   **Description**: Datasets can inadvertently contain biases or unsafe content. Proactive measures are needed to identify and mitigate these issues.
-   **Strategies**:
    -   Diverse data sourcing to reduce demographic bias.
    -   Filtering harmful or toxic content.
    -   Augmenting datasets with examples that promote fairness and safety.
    -   Red-teaming: Actively trying to elicit undesirable behavior to identify and fix vulnerabilities.

### 8. Dataset Splitting
-   **Description**: Divide the dataset into training, validation, and test sets.
    -   **Training set**: Used to train the model.
    -   **Validation set**: Used to tune hyperparameters and monitor for overfitting during training.
    -   **Test set**: Used for final evaluation of the model's performance on unseen data.
-   **Considerations**: Ensure splits are representative and do not leak data between sets.

### 9. Maintaining Data Provenance
-   **Description**: Keep track of the origin and processing steps for your data. This is important for reproducibility, debugging, and understanding model behavior.
