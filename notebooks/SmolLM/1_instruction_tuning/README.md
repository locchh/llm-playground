# Instruction Tuning

This module will guide you through instruction tuning language models. Instruction tuning involves adapting pre-trained models to specific tasks by further training them on task-specific datasets. This process helps models improve their performance on targeted tasks. 

In this module, we will explore two topics: 1) Chat Templates and 2) Supervised Fine-Tuning.

## 1️⃣ Chat Templates

Chat templates structure interactions between users and AI models, ensuring consistent and contextually appropriate responses. They include components like system prompts and role-based messages. For more detailed information, refer to the [Chat Templates](./chat_templates.md) section.

## 2️⃣ Supervised Fine-Tuning

Supervised Fine-Tuning (SFT) is a critical process for adapting pre-trained language models to specific tasks. It involves training the model on a task-specific dataset with labeled examples. For a detailed guide on SFT, including key steps and best practices, see the [Supervised Fine-Tuning](./supervised_fine_tuning.md) page.

## Exercise Notebooks

| Title | Description | Exercise | Link | Colab |
|-------|-------------|----------|------|-------|
| Chat Templates | Learn how to use chat templates with SmolLM2 and process datasets into chatml format | 🐢 Convert the `HuggingFaceTB/smoltalk` dataset into chatml format <br> 🐕 Convert the `openai/gsm8k` dataset into chatml format | [Notebook](./notebooks/chat_templates_example.ipynb) | <a target="_blank" href="https://colab.research.google.com/github/huggingface/smol-course/blob/main/1_instruction_tuning/notebooks/chat_templates_example.ipynb"><img src="https://colab.research.google.com/assets/colab-badge.svg" alt="Open In Colab"/></a> |
| Supervised Fine-Tuning | Learn how to fine-tune SmolLM2 using the SFTTrainer | 🐢 Use the `HuggingFaceTB/smoltalk` dataset<br>🐕 Try out the `bigcode/the-stack-smol` dataset<br>🦁 Select a dataset for a real world use case | [Notebook](./notebooks/sft_finetuning_example.ipynb) | <a target="_blank" href="https://colab.research.google.com/github/huggingface/smol-course/blob/main/1_instruction_tuning/notebooks/sft_finetuning_example.ipynb"><img src="https://colab.research.google.com/assets/colab-badge.svg" alt="Open In Colab"/></a> |

## Keytakeaways

- *A chat template is a part of the tokenizer and it specifies how to convert conversations into a single tokenizable string in the expected model format. Mistral-7B-Instruct and Zephyr-7B are finetuned from the same base model but they’re trained with different chat formats. Without chat templates, you have to manually write formatting code for each model and even minor errors can hurt performance. Chat templates offer a universal way to format chat inputs to any model.*

- *A chat template is a [Jinja template](https://jinja.palletsprojects.com/en/3.1.x/templates/) stored in the tokenizers [chat_template](https://huggingface.co/docs/transformers/main_classes/tokenizer#transformers.PreTrainedTokenizer.chat_template) attribute.*

- *The easiest way to start writing Jinja templates is to refer to existing templates. Use `print(tokenizer.chat_template)` on any chat model to see what template it’s using. Try starting with simple models that don’t call any tools or support RAG.*

- *A base model is trained on raw text data to predict the next token, while an instruct model is fine-tuned specifically to follow instructions and engage in conversations*

## References

- [Template writing](https://huggingface.co/docs/transformers/main/en/chat_templating_writing)
- [Template Designer Documentation¶](https://jinja.palletsprojects.com/en/stable/templates/)
- [Transformers documentation on chat templates](https://huggingface.co/docs/transformers/main/en/chat_templating)
- [Script for Supervised Fine-Tuning in TRL](https://github.com/huggingface/trl/blob/main/examples/scripts/sft.py)
- [`SFTTrainer` in TRL](https://huggingface.co/docs/trl/main/en/sft_trainer)
- [Direct Preference Optimization Paper](https://arxiv.org/abs/2305.18290)
- [Supervised Fine-Tuning with TRL](https://huggingface.co/docs/trl/main/en/tutorials/supervised_finetuning)
- [How to fine-tune Google Gemma with ChatML and Hugging Face TRL](https://www.philschmid.de/fine-tune-google-gemma)
- [Fine-tuning LLM to Generate Persian Product Catalogs in JSON Format](https://huggingface.co/learn/cookbook/en/fine_tuning_llm_to_generate_persian_product_catalogs_in_json_format)
