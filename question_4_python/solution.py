# ===============================================================================================================================
# Question 4: GenAI Clinical Data Assistant (LLM & LangChain) 
# Programmer: Xinran Qi
# Date: May 18 2026
# Objective: Develop a Generative AI Assistant that translates natural language questions into structured Pandas queries. 
# ===============================================================================================================================

import pandas as pd
import json

# ----- Step 1: Load AE dataset -----
# Using mock data representing pharmaverseadam::adae structure
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
# Schema describes columns to the language model so it can
# intelligently map user questions to the correct variables
SCHEMA = {
    "USUBJID": "Unique subject/patient identifier",
    "AETERM":  "Reported adverse event term (e.g. HEADACHE, NAUSEA)",
    "AESEV":   "Severity of adverse event: MILD, MODERATE, or SEVERE",
    "AESOC":   "System organ class / body system category"
}

SCHEMA_STR = "\n".join([f"- {k}: {v}" for k, v in SCHEMA.items()])

# ----- Step 3: Mock LLM parser -----
# Simulates LLM behavior: Prompt -> Parse -> Structured JSON
# In production: replace with real OpenAI/LangChain API call
# The input/output contract remains identical
def mock_llm_parse(question: str) -> dict:
    """
    Maps natural language question to structured JSON output.
    Returns target_column and filter_value for dataset filtering.
    """
    q = question.lower()

    # Map severity-related questions to AESEV column
    if any(w in q for w in ["severity","mild","moderate",
                             "severe","intensity"]):
        for sev in ["mild","moderate","severe"]:
            if sev in q:
                return {"target_column": "AESEV",
                        "filter_value":  sev.upper()}
        return {"target_column": "AESEV", "filter_value": "MODERATE"}

    # Map body system questions to AESOC column
    # Check specific systems BEFORE generic words to avoid mismatches
    elif any(w in q for w in ["gastrointestinal","cardiac","skin",
                               "nervous","subcutaneous","general",
                               "body system","organ class"]):
        soc_map = {
            "gastrointestinal": "GASTROINTESTINAL DISORDERS",
            "nervous":          "NERVOUS SYSTEM DISORDERS",
            "cardiac":          "CARDIAC DISORDERS",
            "skin":             "SKIN AND SUBCUTANEOUS TISSUE DISORDERS",
            "subcutaneous":     "SKIN AND SUBCUTANEOUS TISSUE DISORDERS",
            "general":          "GENERAL DISORDERS"
        }
        for keyword, soc_value in soc_map.items():
            if keyword in q:
                return {"target_column": "AESOC",
                        "filter_value":  soc_value}
        return {"target_column": "AESOC", "filter_value": None}

    # Default: map to specific AE term in AETERM column
    else:
        for term in ae["AETERM"].str.lower().unique():
            if term in q:
                return {"target_column": "AETERM",
                        "filter_value":  term.upper()}
        return {"target_column": "AETERM", "filter_value": None}


# ------ Step 4: Define the Clinical Trial Data Agent ------
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
        print(f"LLM output: {json.dumps(structured, indent=2)}")
        return structured

    def execute_query(self, structured_output: dict) -> dict:
        """Apply the structured filter to the AE dataframe."""
        col = structured_output.get("target_column")
        val = structured_output.get("filter_value")

        if not col or not val:
            return {
                "count":       0,
                "subject_ids": [],
                "message":     "Could not parse query — no match found"
            }

        filtered        = self.df[self.df[col].str.upper() == val.upper()]
        unique_subjects = filtered["USUBJID"].unique().tolist()

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


# ------ Step 5: Run test queries (one example question in the PDF and 3 testing questions I made ------
if __name__ == "__main__":
    agent = ClinicalTrialDataAgent(ae, SCHEMA_STR)

    print("=" * 60)
    print("CLINICAL TRIAL DATA AGENT — TEST QUERIES")
    print("=" * 60)

    # Query 1: Filter by severity
    agent.ask("Give me the subjects who had adverse events "
              "of moderate severity")

    # Query 2: Filter by specific AE term
    agent.ask("Which patients experienced Headache?")

    # Query 3: Filter by nervous system body system
    agent.ask("Show me subjects with nervous system adverse events")

    # Query 4: Filter by gastrointestinal body system
    agent.ask("Give me subjects who had adverse events "
              "of gastrointestinal disorders")
