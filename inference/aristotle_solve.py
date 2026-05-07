import asyncio
import logging
from pathlib import Path
import aristotlelib
from key import ARISTOTLE_API_KEY

# Configure logging
logging.basicConfig(
    level=logging.INFO,
    format="%(levelname)s - %(message)s"
)

# Set API key
aristotlelib.set_api_key(ARISTOTLE_API_KEY)


async def aristotle_prove(input_file_path: str) -> str:
    """Prove a Lean file using Aristotle and return the output file path."""
    output_file_path = input_file_path.replace(".lean", "_aristotle.lean")
    
    await aristotlelib.Project.prove_from_file(
        input_file_path=input_file_path,
        output_file_path=output_file_path,
    )
    
    return output_file_path


if __name__ == "__main__":
    # Default to lso
    default_input = str(Path(__file__).parent / "output" / "theorem_260114.lean")
    result = asyncio.run(aristotle_prove(default_input))
    print(f"Output saved to: {result}")
