import os
os.environ["CUDA_VISIBLE_DEVICES"] = "0" # Specify which GPU to use

import torch

from datasets import load_dataset
from trl import SFTTrainer

# Check available CUDA devices
print("Available CUDA devices:", torch.cuda.device_count())
print("Current CUDA device:", torch.cuda.current_device())
print("CUDA device name:", torch.cuda.get_device_name(torch.cuda.current_device()))

dataset = load_dataset("imdb", split="train")

trainer = SFTTrainer(
    "facebook/opt-350m",
    train_dataset=dataset,
    dataset_text_field="text",
    max_seq_length=512,
)
trainer.train()
