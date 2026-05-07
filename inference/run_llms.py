from openai import OpenAI
from anthropic import Anthropic
from huggingface_hub import InferenceClient
from key import *


LEAN_SYSTEM_PROMPT = """You are an expert in Lean 4 theorem proving. Generate valid Lean 4 code based on the user's query.

It is important that the generated theorem must be self-contained. Include these import(s) at the top:
import Mathlib

Then include any necessary class/structure definitions that the theorem depends on.
Do NOT use `sorry` - instead use `by decide`, `by simp`, `by rfl`, or other valid proof tactics, or leave the proof as `sorry` only if explicitly asked."""


class Prompt:
    def __init__(self, user_content, system_content=LEAN_SYSTEM_PROMPT):
        self.system = system_content
        self.user = user_content


class run_OpenAI:
    def __init__(self, model="", max_tokens=3000, temperature=0.5):
        self.client = OpenAI(api_key=OPENAI_API_KEY)
        self.model = model
        self.max_tokens = max_tokens
        self.temperature = temperature

    def generate(self, prompt):
        try:
            response = self.client.chat.completions.create(
                model=self.model,
                messages=[
                    {"role": "system", "content": prompt.system},
                    {"role": "user", "content": prompt.user}
                ],
                max_tokens=self.max_tokens,
                temperature=self.temperature
            )
            return {"output": response.choices[0].message.content, "success": True}
        except Exception as e:
            return {"error": str(e), "success": False}


class run_Anthropic:
    def __init__(self, model="claude-opus-4-6", max_tokens=3000, temperature=0.5):
        self.client = Anthropic(api_key=ANTHROPIC_API_KEY)
        self.model = model
        self.max_tokens = max_tokens
        self.temperature = temperature

    def generate(self, prompt):
        try:
            response = self.client.messages.create(
                model=self.model,
                max_tokens=self.max_tokens,
                system=prompt.system,
                messages=[{"role": "user", "content": prompt.user}],
                temperature=self.temperature
            )
            return {"output": response.content[0].text, "success": True}
        except Exception as e:
            return {"error": str(e), "success": False}


class run_HuggingFace:
    def __init__(self, model="DeepSeek-Prover-V2-7B", max_tokens=3000, temperature=0.5):
        self.client = InferenceClient(token=HUGGINGFACE_API_TOKEN)
        self.model = model
        self.max_tokens = max_tokens
        self.temperature = temperature

    def generate(self, prompt):
        try:
            response = self.client.chat.completions.create(
                model=self.model,
                messages=[
                    {"role": "system", "content": prompt.system},
                    {"role": "user", "content": prompt.user}
                ],
                max_tokens=self.max_tokens,
                temperature=self.temperature
            )
            return {"output": response.choices[0].message.content, "success": True}
        except Exception as e:
            return {"error": str(e), "success": False}

