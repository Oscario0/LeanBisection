import re
from pathlib import Path
from datetime import datetime
from run_llms import *

BI_TEST_PATH = Path(__file__).parent.parent / "src" / "certified_bisection.lean"
OUTPUT_DIR = Path(__file__).parent / "output"
OUTPUT_DIR.mkdir(exist_ok=True)

with open(BI_TEST_PATH, 'r') as f:
    bi_test_content = f.read()


def get_lean_code(text):
    """Extract Lean code from model output."""
    if not text:
        return None
    
    match = re.search(r'```(?:lean4?|lean)?\s*(.*?)```', text, re.DOTALL)
    if match:
        return match.group(1).strip()
    
    match = re.search(r'(theorem\s+\w+.*?)(?=\n\n|\Z)', text, re.DOTALL)
    if match:
        return match.group(1).strip()
    
    return text.strip()


def save_lean_code(lean_code, filename=None):
    """Save generated Lean code to a .lean file."""
    if filename is None:
        timestamp = datetime.now().strftime("%y%m%d")
        filename = f"theorem_{timestamp}.lean"
    
    if not filename.endswith('.lean'):
        filename += '.lean'
    
    output_path = OUTPUT_DIR / filename
    with open(output_path, 'w') as f:
        f.write(lean_code)
    
    return output_path


def generate_theorem(prompt: Prompt, runner, save=True, filename=None) :
    """Generate a theorem using the specified LLM runner."""
    result = runner.generate(prompt)
    
    if not result["success"]:
        return result
    
    lean_code = get_lean_code(result["output"])
    result["lean_code"] = lean_code
    
    if save and lean_code:
        output_path = save_lean_code(lean_code, filename)
        result["output_file"] = str(output_path)
    
    return result


if __name__ == "__main__":
    user_prompt = f"""Given this Lean 4 implementation of the bisection numerical method for finding roots of continuous functions: 
    {bi_test_content} Generate a theorem or theorems to prove that the Real-valued bisection function indeed returns the root (in the limit of infinite number of steps).
    Make sure to use the theorem the bisectionCore Function (main loop) and use/define helper theorems if necessary."""

    prompt = Prompt(user_prompt)
    runner = run_Anthropic()
    
    result = generate_theorem(prompt, runner)
    print("Lean Code:", result.get('lean_code'))
    print("Success:", result['success'])
    if result.get('output_file'):
        print("Saved to:", result['output_file'])
