# Glossary of Terms

## General Machine Learning Concepts

### Overfitting
- Occurs when a model learns the training data too well, including noise, and fails to generalize to unseen data.

### Underfitting
- Happens when a model is too simple to capture the underlying patterns in the data, leading to poor performance on both training and test sets.

### Catastrophic Forgetting
- A phenomenon in which a neural network forgets previously learned tasks when fine-tuned on new tasks.

### Data Leakage
- Occurs when information from outside the training dataset is used to create the model, leading to overly optimistic performance.

---

## Evaluation Metrics

### BLEU Score
- A metric for evaluating the quality of text generation tasks by comparing machine-generated text to human references using n-gram overlap.

### BERT Score
- Uses contextual embeddings from BERT to evaluate the similarity between machine-generated text and reference text.

### ROUGE Score
- Measures the overlap of n-grams, word sequences, and word pairs between machine-generated and reference texts, often used for summarization tasks.

### Perplexity
- Evaluates the quality of probabilistic models by measuring how well they predict a sample. Lower perplexity indicates better performance.

### METEOR
- Evaluates text generation by aligning words semantically and syntactically, focusing on recall.

### Pass@k
- Measures the probability of solving a task in at least one of the top-k attempts.

### Accuracy
- The proportion of correctly classified instances out of the total instances.

### Recall
- The proportion of relevant instances correctly identified out of all relevant instances.

### Precision
- The proportion of relevant instances out of all instances identified as relevant.

### F1-Score
- The harmonic mean of precision and recall, balancing both metrics.

---

## Pretraining and Fine-Tuning Techniques

### Self-Supervised Fine-Tuning
- Uses unlabeled data to learn representations by creating pseudo-labels or tasks.

### Masked Language Modeling (MLM)
- Predicts masked tokens in a sentence, often used in transformer models like BERT.

### Next Sentence Prediction (NSP)
- Predicts whether two sentences are consecutive, used to improve understanding of sentence relationships.

### Supervised Fine-Tuning
- Fine-tuning a model on labeled data to adapt it to specific tasks.

### Full Fine-Tuning
- Updating all model parameters on task-specific data.

### Parameter-Efficient Fine-Tuning (PEFT)
- Fine-tunes a subset of parameters, reducing computational cost.

### Selective Fine-Tuning
- Fine-tunes specific layers or modules of the model.

### Additive Fine-Tuning
- Adds new parameters to the model for specific tasks while keeping original parameters fixed.

### Adapters
- Lightweight modules inserted into the model that can be fine-tuned for new tasks.

### Reparameterization Fine-Tuning
- Modifies parameterization for efficient adaptation to new tasks.

### LoRA
- Fine-tunes low-rank matrices in the model, saving memory and computation.

### DoRA
- A derivative of LoRA designed for better efficiency and scaling.

### QLoRA
- Combines LoRA with quantized models for memory-efficient fine-tuning.

### 4-bit, 8-bit Quantization
- Reduces model size by representing parameters with lower-bit precision, saving memory and improving efficiency.

### Double Quantization
- Applies quantization at multiple stages for further efficiency.

### Soft-Prompts
- Learnable vectors added to prompts for task-specific adaptation.

### Prompt Tuning
- Fine-tunes prompts instead of model parameters to adapt models to new tasks.

### Prefix Tuning
- Optimizes prefixes added to input sequences for task-specific performance.

### P-Tuning
- Enhances prompt tuning with continuous parameter optimization.

### Multi-Task Tuning
- Fine-tunes a model on multiple tasks simultaneously to improve generalization.

---

## Advanced Fine-Tuning Techniques

### Instruction Tuning
- Fine-tunes a model to follow natural language instructions across tasks.

### Reinforcement Learning from Human Feedback (RLHF)
- Combines human feedback with reinforcement learning to fine-tune models for desired behavior.

### Constitutional AI
- Fine-tunes models using predefined principles instead of human feedback.

### Meta-Instruction Tuning
- Fine-tunes models on diverse tasks with hierarchical or meta-contextual instructions.

### Task-Specific Tuning (Few-Shot or Zero-Shot)
- Adapts models for specific tasks with limited (few-shot) or no (zero-shot) labeled data.

### Multimodal Instruction Tuning
- Extends instruction tuning to handle multiple modalities, like text and images.

### Chain-of-Thought Instruction Tuning
- Fine-tunes models to generate intermediate reasoning steps for better problem-solving.

### Expert-Tuning (Mixture of Experts)
- Trains specialized modules (experts) for specific tasks, enabling efficient task routing.

### Self-Reflective Training
- Models evaluate and refine their outputs iteratively to improve performance.

### Neural Symbolic Integration
- Combines neural networks with symbolic reasoning for better generalization and interpretability.

### Alignment Tuning
- Aligns models with specific values, societal norms, or operational goals.

### Hyper-Instruction Tuning
- Fine-tunes models for understanding abstract or multi-task instructions.

---

## Reinforcement Learning Techniques

### Reward Modeling
- Creates a model to predict human preferences for fine-tuning with reinforcement learning.

### RLHF
- Uses reward modeling and reinforcement learning to align models with human feedback.

### PPO (Proximal Policy Optimization)
- A reinforcement learning algorithm commonly used in RLHF.

### DPO (Direct Preference Optimization)
- Optimizes models directly based on preference scores without complex reward modeling.

### RLEF (Reinforcement Learning from Expert Feedback)
- Extends RLHF by incorporating feedback from domain experts.

---

## In-Context Learning Techniques

### In-Context Learning
- Enables models to perform tasks by providing task examples in the input context.

### Dynamic In-Context Learning
- Dynamically adapts the context or examples during inference for better performance.

### Self-Adaptive In-Context Learning
- Models generate or refine in-context examples during inference.

### Active In-Context Learning
- Selects task-relevant examples dynamically based on a scoring mechanism.

### Meta-In-Context Learning
- Uses meta-learning principles to improve example selection and adaptation.

### Contrastive In-Context Learning
- Includes contrasting examples to help the model distinguish patterns.

### Reinforced In-Context Learning
- Incorporates reinforcement learning to optimize example selection.

### Augmented In-Context Learning
- Combines in-context learning with external retrieval or data augmentation.

### Dynamic Prompt Optimization
- Iteratively refines prompts based on model outputs or feedback.

### Gradient-Guided Prompting
- Optimizes prompts using gradient-based methods for specific tasks.

---

## Reasoning Techniques

### Chain-of-Thought
- Encourages models to generate intermediate reasoning steps to improve task performance.

### Tree-of-Thought
- Models explore multiple reasoning paths, like a decision tree, for complex tasks.

### Graph-of-Thought
- Represents reasoning as a graph structure, capturing complex relationships between steps.
