import pandas as pd
import json

# -----------------------------
# Load Data
# -----------------------------
def load_data(file_path="adae.csv"):
    df = pd.read_csv(file_path)
    df.columns = df.columns.str.upper()
    df = df.fillna("")
    return df


# -----------------------------
# Schema + Dataset Extraction
# -----------------------------
def build_schema(df):
    ae_terms = df["AETERM"].dropna().unique().tolist()
    ae_soc = df["AESOC"].dropna().unique().tolist()
    ae_sev = df["AESEV"].dropna().unique().tolist()

    ae_terms_sample = ae_terms[:50]
    ae_soc_sample = ae_soc[:50]

    schema = f"""
    You are working with a clinical adverse events dataset.

    Columns:
    - AESEV (Severity): {ae_sev}
    - AETERM (Conditions): {ae_terms_sample}
    - AESOC (Body Systems): {ae_soc_sample}

    Your job:
    Map user questions to:
    1. target_column
    2. filter_value

    Instructions:
    - If user refers to severity or intensity → AESEV
    - If user mentions a condition → match to AETERM
    - If user mentions a body system → match to AESOC
    - Choose the closest matching value from the dataset

    Return ONLY JSON:
    {{
      "target_column": "...",
      "filter_value": "..."
    }}
    """
    return schema, ae_terms, ae_soc, ae_sev


# -----------------------------
# LLM Agent Class
# -----------------------------
class ClinicalTrialDataAgent:
    def __init__(self, df, use_mock=True):
        self.use_mock = use_mock
        self.schema, self.ae_terms, self.ae_soc, self.ae_sev = build_schema(df)

    def parse_query(self, user_query):
        prompt = self.schema + f"\nUser Question: {user_query}"

        if self.use_mock:
            return self.mock_llm_response(user_query)

        # REAL LLM VERSION (optional)
        # response = self.llm(...)
        # return json.loads(response)

    def mock_llm_response(self, query):
        q = query.lower()

        # Severity
        for sev in self.ae_sev:
            if sev.lower() in q:
                return {"target_column": "AESEV", "filter_value": sev}

        # AETERM
        for term in self.ae_terms:
            if term.lower() in q:
                return {"target_column": "AETERM", "filter_value": term}

        # AESOC
        for soc in self.ae_soc:
            if soc.lower().split()[0] in q:
                return {"target_column": "AESOC", "filter_value": soc}

        return {"target_column": None, "filter_value": None}


# -----------------------------
# Execution Function
# -----------------------------
def execute_query(df, parsed_output):
    col = parsed_output["target_column"]
    val = parsed_output["filter_value"]

    if col is None or val is None:
        return {"count": 0, "subjects": []}

    df[col] = df[col].astype(str)

    if col in ["AESEV", "AETERM"]:
        filtered_df = df[df[col].str.upper() == val.upper()]
    else:
        filtered_df = df[df[col].str.upper().str.contains(val.upper())]

    filtered_df = filtered_df.drop_duplicates(subset=["USUBJID", col])
    unique_subjects = filtered_df["USUBJID"].dropna().unique().tolist()

    return {
        "count": len(unique_subjects),
        "subjects": unique_subjects
    }