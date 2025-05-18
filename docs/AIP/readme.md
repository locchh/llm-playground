### AI Coding Action × Capability Level

Here is a structured table that describes the relationship between **AI coding actions** and **capability levels of a coding project**, including:

| Capability Level ↓ / AI Coding Action →    | Understand                                   | Explain                                        | Modify                                                                        | Generate                                                        |
| ------------------------------------------ | -------------------------------------------- | ---------------------------------------------- | ----------------------------------------------------------------------------- | --------------------------------------------------------------- |
| **Core Logic Implementation**              | ✅ LLM-only<br>🧠 All languages (incl. COBOL) | ✅ LLM-only<br>📖 All languages                 | ⚠️ LLM-only for small<br>🛠️ LLM + tools for complex<br>👨‍💻 Best for modern | ✅ LLM-only (simple)<br>🛠️ Complex with tools<br>🧪 Modern best |
| **IO Interaction (File, CLI, API)**        | ✅ LLM-only<br>🧠 All languages               | ✅ LLM-only<br>📖 All languages                 | ⚠️ LLM-only<br>🛠️ Tools for test/run                                         | ⚠️ LLM-only for stubs<br>🛠️ Tools for realism                  |
| **Dependency Use (Library, SDK)**          | ✅ LLM-only<br>🧠 All languages               | ✅ LLM-only<br>📖 All languages                 | ⚠️ LLM-only<br>🛠️ Tools for install/test                                     | ⚠️ LLM-only for templates<br>🛠️ Tools to resolve versions      |
| **Application Layer (UI, Business Logic)** | ✅ LLM-only<br>🧠 Modern preferred            | ✅ LLM-only<br>📖 Modern UI logic easier        | 🛠️ LLM + tools<br>🧪 Modern (React, Spring)                                  | 🛠️ LLM + tools<br>📲 Focus on modern UX                        |
| **System Integration (Batch, DB, API)**    | ✅ LLM-only<br>🧠 COBOL, JCL, modern          | ✅ LLM-only<br>📖 Legacy and modern             | 🛠️ LLM + tools<br>🔌 Legacy (JCL) and modern                                 | 🛠️ LLM + tools<br>📡 Integration templates                     |
| **Distributed Platform (Cloud, Services)** | ✅ LLM + tools<br>🌐 Modern (K8s, AWS, etc.)  | ✅ LLM + tools<br>📖 Needs runtime/tool context | 🔧 LLM + tools<br>☁️ Modern only (Terraform, YAML)                            | 🛠️ Agent-only<br>☁️ Modern infra-as-code & APIs                |


### 🔍 Key Takeaways:

* **LLM-only** is often sufficient for *understanding*, *explaining*, and *light generation/modification*, especially in **isolated code blocks**.
* **LLM + Tools (Agents)** are necessary for:

  * Complex *modification* or *generation* across files and dependencies
  * Context-rich environments (e.g., distributed systems, cloud services)
  * Interaction with **build tools, CI/CD, runtime environments**
* **Legacy Code (COBOL, JCL)**:

  * Readable and explainable by LLMs
  * Modification or generation is harder, often benefits from domain-specific rules/tools
* **Modern Code (Python, Java, C++)**:

  * Well-supported for all actions
  * Ideal for generation, modification, and dynamic tool integration via agents
