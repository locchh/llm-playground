# Fine-Tune LLMs Locally with InstructLab

**Estimated time needed:** 15 minutes

## Introduction

InstructLab is a library that allows easy fine-tuning of large language models (LLMs) on the local machine, including laptops. It is popular for its seamless synthetic data generation using a teacher model and the fine-tuning of a student model on that synthetic data.

InstructLab alleviates the problem of insufficient training examples to fine-tune the student model. However, instead of having examples, provide a few initial question-and-answer pairs from which the teacher model can generate synthetic question-and-answer pairs.

InstructLab is capable of fine-tuning models to impart them with new knowledge or a set of skills. Moreover, InstructLab provides a structured way of separating different pieces of knowledge and skill using a taxonomy, which allows easy augmentation and information updates on which models are fine-tuned.

In contrast, the model is fine-tuned using quantized low-rank adaptation (QLoRA), and it is quantized by default, allowing it to run locally on consumer-grade hardware, such as laptops.

## Objectives

After completing this reading you will be able to:

- Install InstructLab
- Apply InstructLab to chat with models from Hugging Face
- Apply InstructLab to generate synthetic examples using a teacher model
- Explain how to fine-tune a student model using synthetic examples
- Apply the fine-tuned student model on local hardware

---

## Install InstructLab

### Step 1: Set Up a New Directory

Run the following commands in a terminal:

```bash
mkdir instructlab
cd instructlab
```

### Step 2: Set Up a Virtual Environment

It is recommended to use a virtual environment such as `venv` or `pyenv`.

```bash
python3 -m venv --upgrade-deps venv
source venv/bin/activate
```

### Step 3: Install InstructLab

Run the following commands:

```bash
pip cache remove llama_cpp_python
pip install instructlab
```

### Step 4: Test the Installation

```bash
ilab
```

You should see output similar to:

```plaintext
Usage: ilab [OPTIONS] COMMAND [ARGS]...
CLI for interacting with InstructLab.
...
```

---

## Initialize InstructLab

Run the following command to initialize InstructLab:

```bash
ilab config init
```

Accept the default options, including cloning the taxonomy repo from `https://github.com/instructlab/taxonomy`.

### Download a Model

1. Download the default model:

```bash
ilab model download
```

2. Download another model (e.g., Granite 7B):

```bash
ilab download --repository instructlab/granite-7b-lab-GGUF --filename granite-7b-lab-Q4_K_M.gguf --hf-token <Access Token>
```

---

## Chat with Base Models

1. Serve the Merlinite model:

```bash
ilab model serve
```

2. In a new terminal session, navigate to the directory and activate the virtual environment:

```bash
source venv/bin/activate
```

3. Chat with the model:

```bash
ilab model chat
```

4. To chat with the Granite model:

```bash
ilab model serve --model-path models/granite-7b-lab-Q4_K_M.gguf
ilab model chat --model models/granite-7b-lab-Q4_K_M.gguf
```

---

## Fine-Tuning Using InstructLab

### Step 1: Generate Synthetic Data

Create a `qna.yaml` file with seed examples:

```yaml
version: 2
task_description: |
    Teach an LLM to tokenize.
created_by: YOUR_GITHUB_USERNAME
seed_examples:
  - question: |
      Tokenize "The quick brown fox jumps over the lazy dog."
    answer: |
      ['The', 'quick', 'brown', 'fox', 'jumps', 'over', 'the', 'lazy', 'dog', '.']
...
```

### Step 2: Fine-Tune the Model

Use the teacher model (Merlinite) to generate synthetic examples, and fine-tune the student model (Granite) on that data.

