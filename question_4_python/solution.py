# =============================================================
# Question 4: GenAI Clinical Data Assistant (LLM & LangChain) 
# Programmer: Xinran Qi
# Date: May 18 2026
# Objective: Develop a Generative AI Assistant that translates natural language questions into structured Pandas queries. 
# =============================================================

import pandas as pd
import json

# ----- Step 1: Load AE dataset -----
# Using mock data since pharmaverseadam is an R package
# In production this would load from adae.csv exported from R
ae = pd.DataFrame({
    "USUBJID": ["SUBJ001","SUBJ002","SUBJ003","SUBJ004",
                "SUBJ005","SUBJ006","SUBJ007","SUBJ008"],
    "AETERM":  ["HEADACHE","NAUSEA","HEADACHE","DIZZINESS",
                "NAUSEA","PRURITUS","HEADACHE","FATIGUE"],
    "AESEV":   ["MILD","MODERATE","SEVERE","MILD",
                "MODERATE","MILD","MILD","MODERATE"],
    "AESOC":   ["NERVOUS SYSTEM DISORDERS",
                "GASTROINTESTINAL DISORDERS",
                "NERVOUS SYSTEM DISORDERS",
                "NERVOUS SYSTEM DISORDERS",
                "GASTROINTESTINAL DISORDERS",
                "SKIN AND SUBCUTANEOUS TISSUE DISORDERS",
                "NERVOUS SYSTEM DISORDERS",
                "GENERAL DISORDERS"]
})

# ----- Step 2: Define dataset schema for LLM -----
# This dictionary describes the dataset to the language model
# so it can intelligently map user questions to the right columns
SCHEMA = {
    "USUBJID": "Unique subject/patient identifier",
    "AETERM":  "Reported adverse event term (e.g. HEADACHE, NAUSEA)",
    "AESEV":   "Severity of adverse event: MILD, MODERATE, or SEVERE",
    "AESOC":   "System organ class / body system category"
}

SCHEMA_STR = "\n".join([f"- {k}: {v}" for k, v in SCHEMA.items()])

# ----- Step 3: Mock LLM parser -----
# In production: replace with real OpenAI/LangChain API call
# Logic flow is identical: Prompt -> Parse -> Execute
def mock_llm_parse(question: str) -> dict:
    """
    Simulates LLM behavior by mapping natural language
    to a structured JSON output with target_column and filter_value.
    A real LLM call would use the same input/output contract.
    """
    q = question.lower()

    # Map severity-related questions to AESEV column
    if any(w in q for w in ["severity", "intense", "mild",
                             "moderate", "severe"]):
        for sev in ["mild", "moderate", "severe"]:
            if sev in q:
                return {"target_column": "AESEV",
                        "filter_value":  sev.upper()}
        return {"target_column": "AESEV", "filter_value": "MODERATE"}

    # Map body system questions to AESOC column
    elif any(w in q for w in ["cardiac", "skin", "nervous",
                               "gastrointestinal", "body system",
                               "organ", "system"]):
        for soc in ae["AESOC"].str.lower().unique():
            if any(word in q for word in soc.split()):
                return {"target_column": "AESOC",
                        "filter_value":  soc.upper()}
        return {"target_column": "AESOC", "filter_value": None}

    # Default: map to specific AE term in AETERM column
    else:
        for term in ae["AETERM"].str.lower().unique():
            if term in q:
                return {"target_column": "AETERM",
                        "filter_value":  term.upper()}
        return {"target_column": "AETERM", "filter_value": None}


# ----- Step 4: Define the Clinical Trial Data Agent -----
class ClinicalTrialDataAgent:
    """
    AI agent that accepts natural language questions about
    clinical AE data and returns filtered results.
    Pipeline: user question -> LLM parse -> Pandas filter -> results
    """

    def __init__(self, dataframe: pd.DataFrame, schema: str):
        self.df     = dataframe
        self.schema = schema

    def parse_question(self, question: str) -> dict:
        """Send question to LLM and receive structured JSON output."""
        print(f"\nQuestion: {question}")
        structured = mock_llm_parse(question)
        print(f"LLM structured output: {json.dumps(structured, indent=2)}")
        return structured

    def execute_query(self, structured_output: dict) -> dict:
        """Apply the structured filter to the AE dataframe."""
        col = structured_output.get("target_column")
        val = structured_output.get("filter_value")

        if col is None or val is None:
            return {
                "count":       0,
                "subject_ids": [],
                "message":     "Could not parse query — no match found"
            }

        # Apply filter and return unique subjects
        filtered         = self.df[self.df[col].str.upper() == val.upper()]
        unique_subjects  = filtered["USUBJID"].unique().tolist()

        return {
            "count":          len(unique_subjects),
            "subject_ids":    unique_subjects,
            "filter_applied": f"{col} == '{val}'"
        }

    def ask(self, question: str) -> dict:
        """Full pipeline: question -> parse -> execute -> return."""
        structured = self.parse_question(question)
        result     = self.execute_query(structured)
        print(f"Subjects matched: {result['count']}")
        print(f"Subject IDs: {result['subject_ids']}")
        return result


# ----- Step 5: Run 3 test queries -----
if __name__ == "__main__":
    agent = ClinicalTrialDataAgent(ae, SCHEMA_STR)

    print("=" * 60)
    print("CLINICAL TRIAL DATA AGENT — TEST QUERIES")
    print("=" * 60)

    # Query 1: Filter by severity
    agent.ask("Give me the subjects who had adverse events of moderate severity.")

    # Query 2: Filter by specific AE term
    agent.ask("Which patients experienced Headache?")

    # Query 3: Filter by body system
    agent.ask("Show me subjects with nervous system adverse events.")

    # Query 3: Filter by body system
    agent.ask("Give me subjects who had adverse events of gastrointestinal disorders.")
  
