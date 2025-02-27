# Coding Agent Actions
- Search & Discovery
- Context awareness
- Human Actions
- Tools (Create, Suggest, Edit, Verify)

# Agent Levels
- Human
- Copilots (Collaborative but small scope)
- Autonomous
- AI Flow (Collaborative and Indenpendently Powerful)

**Takeaways**

- *Context awareness brings in explicit knowledge*

- *Human Actions bring implicit intent*

- *Tools are what take action across search & discovery, state changes, and verification*

- *Larger models is not equal to better retrieval*

- *Multi-step agentic retrieval resembles human decision-making more closely than single-step RAG*

- *The Commit Message Bank Dataset is a newly developed dataset designed to reflect real-world use cases, as each commit corresponds to varying depths across multiple files*

# Let's surf

1. Add the repository to sources.list.d

```bash
curl -fsSL "https://windsurf-stable.codeiumdata.com/wVxQEIWkwPUEAGf3/windsurf.gpg" | sudo gpg --dearmor -o /usr/share/keyrings/windsurf-stable-archive-keyring.gpg
echo "deb [signed-by=/usr/share/keyrings/windsurf-stable-archive-keyring.gpg arch=amd64] https://windsurf-stable.codeiumdata.com/wVxQEIWkwPUEAGf3/apt stable main" | sudo tee /etc/apt/sources.list.d/windsurf.list > /dev/null
```

2. Update
```bash
sudo apt-get update
```

3. Install
```
sudo apt-get upgrade windsurf
```

# References

[The Cursor Editor](https://www.cursor.com/)

[The Windsurf Editor](https://codeium.com/windsurf)

[Build Apps with Windsurf’s AI Coding Agents](https://learn.deeplearning.ai/courses/build-apps-with-windsurfs-ai-coding-agents)