from clinical_agent import load_data, ClinicalTrialDataAgent, execute_query

# Load dataset
df = load_data("adae.csv")

# Initialize agent
agent = ClinicalTrialDataAgent(df, use_mock=True)

# Test queries (3 required, you can include more)
queries = [
    "Give me the subjects who had Adverse events of Moderate severity?",
    "Show patients who had headache",
    "Which patients had bradycardia?"
]

# Run tests
for q in queries:
    print("\n-----------------------------")
    print("Query:", q)

    parsed = agent.parse_query(q)
    print("Parsed Output:", parsed)

    result = execute_query(df, parsed)
    print("Result:", result)
