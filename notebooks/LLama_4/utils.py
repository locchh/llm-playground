import os
from dotenv import load_dotenv, find_dotenv

def load_env():
    _ = load_dotenv(find_dotenv())
    
def get_llama_api_key():
    load_env()
    llama_api_key = os.getenv("LLAMA_API_KEY")
    return llama_api_key

def get_llama_base_url():
    load_env()
    llama_base_url = os.getenv("LLAMA_BASE_URL")
    return llama_base_url

def get_together_api_key():
    load_env()
    together_api_key = os.getenv("TOGETHER_API_KEY")
    return together_api_key


from llama_api_client import LlamaAPIClient
def llama4(prompt, image_urls=[], model="Llama-4-Scout-17B-16E-Instruct-FP8"): # Llama-4-Maverick-17B-128E-Instruct-FP8
  image_urls_content = []
  for url in image_urls:
    image_urls_content.append({"type": "image_url", "image_url": {"url": url}})

  content = [{"type": "text", "text": prompt}]
  content.extend(image_urls_content)

  client = LlamaAPIClient(api_key=get_llama_api_key())

  response = client.chat.completions.create(
    model=model,
    messages=[{
        "role": "user",
        "content": content
    }],
    temperature=0
  )

  return response.completion_message.content.text


from together import Together
def llama4_together(prompt, image_urls=[], model="meta-llama/Llama-4-Scout-17B-16E-Instruct"):
  image_urls_content = []
  for url in image_urls:
    image_urls_content.append({"type": "image_url", "image_url": {"url": url}})

  content = [{"type": "text", "text": prompt}]
  content.extend(image_urls_content)

  client = Together(api_key = get_together_api_key())
  response = client.chat.completions.create(
    model=model,
    messages=[{
        "role": "user",
        "content": content
    }],
    temperature=0
  )

  return response.choices[0].message.content

import re
from pydantic import BaseModel
from typing import List
from PIL import Image
import matplotlib.pyplot as plt
import matplotlib.patches as patches

# Define a model for the bounding box
class BoundingBox(BaseModel):
    x1: float
    y1: float
    x2: float
    y2: float

# Define a model for the tool
class Tool(BaseModel):
    name: str
    bbox: BoundingBox

def parse_output(output: str) -> List[Tool]:
    # Use regular expressions to find all occurrences of <BBOX>...</BBOX>
    bboxes = re.findall(r'<BBOX>(.*?)</BBOX>', output)

    # Initialize an empty list to store the tools
    tools = []

    # Split the output into lines
    lines = output.split('\n')

    # Iterate over the lines
    for line in lines:
        # Check if the line contains a tool name
        if '**' in line:
            # Extract the tool name
            name = line.strip().replace('*', '').strip()

            # Find the corresponding bounding box
            bbox = bboxes.pop(0)

            # Split the bounding box into coordinates
            x1, y1, x2, y2 = map(float, bbox.split(','))

            # Create a Tool object and add it to the list
            tools.append(Tool(name=name, bbox=BoundingBox(x1=x1, y1=y1, x2=x2, y2=y2)))

    return tools

def draw_bounding_boxes(img_path: str, tools: List[Tool]) -> None:
    # Open the image using PIL
    img = Image.open(img_path)

    # Get the width and height of the image
    width, height = img.size

    # Create a figure and axis
    fig, ax = plt.subplots()

    # Display the image
    ax.imshow(img)

    # Iterate over the tools
    for tool in tools:
        # Create a rectangle patch
        rect = patches.Rectangle((tool.bbox.x1 * width, tool.bbox.y1 * height),
                                 (tool.bbox.x2 - tool.bbox.x1) * width,
                                 (tool.bbox.y2 - tool.bbox.y1) * height,
                                 linewidth=1, edgecolor='r', facecolor='none')

        # Add the patch to the axis
        ax.add_patch(rect)

        # Annotate the tool
        ax.text(tool.bbox.x1 * width, tool.bbox.y1 * height, tool.name, color='red')

    # Set the limits of the axis to the size of the image
    ax.set_xlim(0, width)
    ax.set_ylim(height, 0)

    # Show the plot
    plt.show()


import requests
from PIL import Image
from io import BytesIO
import matplotlib.pyplot as plt

def display_local_image(image_path):
    img = Image.open(image_path)
    plt.figure(figsize=(5,4), dpi=200)
    plt.imshow(img)
    plt.axis('off')
    plt.show()



import json
import re
from typing import Any, Dict


def parse_json(input_string: str):
    """
    Attempts to parse the given string as JSON. If direct parsing fails,
    it tries to extract a JSON snippet from code blocks formatted as:
        ```json
        ... JSON content ...
        ```
    or any code block delimited by triple backticks and then parses that content.
    Parameters:
        input_string (str): The input string which may contain JSON.
    Returns:
        The parsed JSON object.
    Raises:
        ValueError: If parsing fails even after attempting to extract a JSON snippet.
    """
    # Try to parse the string directly.
    try:
        return json.loads(input_string)
    except json.JSONDecodeError as err:
        error = err  # Proceed to try extracting a JSON snippet.
    # Define patterns to search for a JSON code block.
    patterns = [
        re.compile(r"```json\s*(.*?)\s*```", re.DOTALL | re.IGNORECASE),  # code block with "json" label
        re.compile(r"```(.*?)```", re.DOTALL)  # any code block delimited by triple backticks
    ]

    # Attempt extraction using each pattern in order.
    for pattern in patterns:
        match = pattern.search(input_string)
        if match:
            json_candidate = match.group(1).strip()
            try:
                return json.loads(json_candidate)
            except json.JSONDecodeError:
                # Continue trying if extraction from the code block didn't result in valid JSON.
                continue
    # If all attempts fail, raise an error.
    raise error

def evaluate(ground_truth: Any, predictions: Any, strict_json: bool = True) -> Dict[str, Any]:
    result = {
        "is_valid_json": False,
        "correct_categories": 0.,
        "correct_sentiment": False,
        "correct_urgency": False,
    }
    try:
        ground_truth = ground_truth if isinstance(ground_truth, dict) else (json.loads(ground_truth) if strict_json else parse_json(ground_truth))
        predictions = predictions if isinstance(predictions, dict) else (json.loads(predictions) if strict_json else parse_json(predictions))
    except (json.JSONDecodeError, ValueError):
        pass
    else:
        result["is_valid_json"] = True

        # Handle missing categories in predictions
        correct_categories = 0
        total_categories = len(ground_truth.get("categories", {}))

        if total_categories > 0 and "categories" in predictions:
            for k in ground_truth["categories"].keys():
                # Check if the category exists in predictions before comparing
                if k in predictions["categories"]:
                    if ground_truth["categories"][k] == predictions["categories"][k]:
                        correct_categories += 1
                # Missing category counts as incorrect

            result["correct_categories"] = correct_categories / total_categories

        # Handle missing sentiment and urgency fields
        result["correct_sentiment"] = predictions.get("sentiment") == ground_truth.get("sentiment", None)
        result["correct_urgency"] = predictions.get("urgency") == ground_truth.get("urgency", None)

    # Calculate total score
    correct_fields = [v for k, v in result.items() if k.startswith('correct_')]
    result["total"] = sum(correct_fields) / len(correct_fields) if correct_fields else 0.0

    return result
