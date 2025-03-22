from smolagents import CodeAgent, DuckDuckGoSearchTool, HfApiModel, load_tool, tool
import datetime
import requests
import pytz
import yaml
from tools.final_answer import FinalAnswerTool
from Gradio_UI import GradioUI

# Below is an example of a tool that does nothing. Amaze us with your creativity!
@tool
def my_custom_tool(arg1: str, arg2: int) -> str:
    # It's important to specify the return type for clarity and integration with the agent
    """A tool that does nothing yet.
    
    Args:
        arg1: the first argument (string)
        arg2: the second argument (integer)
    
    Returns:
        A placeholder string message
    """
    return "What magic will you build?"

@tool
def get_current_time_in_timezone(timezone: str) -> str:
    """A tool that fetches the current local time in a specified timezone.
    
    Args:
        timezone: A string representing a valid timezone (e.g., 'America/New_York').
    
    Returns:
        A string containing the current time in the requested timezone or an error message if invalid.
    """
    try:
        # Create timezone object
        tz = pytz.timezone(timezone)
        # Get current time in that timezone
        local_time = datetime.datetime.now(tz).strftime("%Y-%m-%d %H:%M:%S")
        return f"The current local time in {timezone} is: {local_time}"
    except Exception as e:
        # Handle invalid timezone inputs gracefully
        return f"Error fetching time for timezone '{timezone}': {str(e)}"

# Initialize the FinalAnswerTool, which ensures the agent produces a final response
final_answer = FinalAnswerTool()

# If the agent does not answer, the model might be overloaded.
# Alternative: Use a Hugging Face Endpoint that also supports the Qwen2.5 coder model.
model = HfApiModel(
    max_tokens=2096,  # Limit response length
    temperature=0.5,  # Controls randomness (0 = deterministic, 1 = more random)
    model_id='Qwen/Qwen2.5-Coder-32B-Instruct',  # Model ID (may be overloaded at times)
    custom_role_conversions=None,  # No custom role conversions applied
)

# Import a text-to-image generation tool from the Hugging Face Hub
image_generation_tool = load_tool("agents-course/text-to-image", trust_remote_code=True)

# Load prompt templates from a YAML file
with open("prompts.yaml", 'r') as stream:
    prompt_templates = yaml.safe_load(stream)
    
# Initialize a CodeAgent instance, which will use the model and tools
agent = CodeAgent(
    model=model,  # AI model to generate responses
    tools=[final_answer],  # List of tools available to the agent (FinalAnswerTool is required)
    max_steps=6,  # Maximum reasoning steps before finalizing an answer
    verbosity_level=1,  # Controls the amount of debugging/logging information
    grammar=None,  # No custom grammar rules
    planning_interval=None,  # No fixed planning interval
    name=None,  # No specific agent name assigned
    description=None,  # No specific description provided
    prompt_templates=prompt_templates  # Templates to structure agent's prompts
)

# Launch a Gradio UI to interact with the agent
GradioUI(agent).launch()
