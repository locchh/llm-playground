# 🚀 Quickstart: Setting Up Doccano

Doccano is an open-source text annotation tool that supports sequence labeling, text classification, and sequence-to-sequence tasks.

## **1️⃣ Install Doccano**
You can install Doccano using **Docker** (recommended) or **pip**.

### **A. Using Docker (Recommended)**
1. Install **Docker** if you haven’t already: [Docker Install](https://docs.docker.com/get-docker/)
2. Run the following command:
   ```bash
   docker run --rm -d -p 8000:8000 -e "ADMIN_USERNAME=admin" -e "ADMIN_PASSWORD=123@qwe" doccano/doccano
   ```
3. Open your browser and go to:  
   👉 **http://localhost:8000**  
   Log in with:
   - **Username**: `admin`
   - **Password**: `123@qwe`

### **B. Using pip (Alternative)**
If you prefer a non-Docker installation:
```bash
pip install doccano
doccano init
doccano createuser --username admin --password 123@qwe
doccano webserver --port 8000
```
Then, access **http://localhost:8000**.

---

## **🛠 Quickstart: Using Doccano**
1. **Log in** to the Doccano web interface.
2. **Create a Project**:
   - Click **"Create Project"**.
   - Choose **Sequence Labeling, Text Classification, or Seq2Seq** based on your task.
3. **Upload Your Dataset** (if available):
   - Go to **Dataset → Upload Data**.
   - Upload a `.jsonl` or `.txt` file.
4. **Annotate Text**:
   - Select a label.
   - Highlight text and assign labels.
5. **Export Labeled Data**:
   - Go to **Dataset → Export**.
   - Download annotations in **JSONL** format.

---

## **🔥 Next Steps**
- Integrate Doccano with **ANTLR-based parsing**.
- Automate parsing & validation using **Python scripts**.
- Train **machine learning models** using labeled data.

---

# **📜 Script: Import/Export Between Doccano & ANTLR**

### **🔧 Prerequisites**
1. Ensure **Doccano is running** (`http://localhost:8000`).
2. Install required Python packages:
   ```bash
   pip install requests jsonlines antlr4-python3-runtime
   ```
3. Have your **ANTLR grammar ready** (e.g., `MyLang.g4`).

### **📜 Python Script**
```python
import json
import requests
import os
import subprocess
import csv

# Doccano credentials
DOCCANO_URL = "http://localhost:8000"
USERNAME = "admin"
PASSWORD = "password"

# ANTLR Configuration
ANTLR_GRAMMAR = "MyLang.g4"
TEST_CASES_DIR = "antlr_test_cases"
VALIDATION_REPORT = "antlr_validation_report.csv"

# Ensure directories exist
os.makedirs(TEST_CASES_DIR, exist_ok=True)

# ---------------- STEP 1: Authenticate with Doccano ----------------
def get_auth_token():
    response = requests.post(f"{DOCCANO_URL}/v1/auth-token", json={"username": USERNAME, "password": PASSWORD})
    response.raise_for_status()
    return response.json()["token"]

TOKEN = get_auth_token()
HEADERS = {"Authorization": f"Token {TOKEN}"}

# ---------------- STEP 2: Export Annotated Data from Doccano ----------------
def export_doccano_annotations(project_id):
    """Fetches annotated data from Doccano."""
    response = requests.get(f"{DOCCANO_URL}/v1/projects/{project_id}/docs", headers=HEADERS)
    response.raise_for_status()
    return response.json()["results"]

def save_annotations_as_antlr_tests(project_id):
    """Converts Doccano annotations into ANTLR test cases."""
    annotations = export_doccano_annotations(project_id)
    test_files = []
    
    for i, entry in enumerate(annotations):
        text = entry["text"]
        labels = entry.get("annotations", [])

        test_case = f"// Test Case {i+1}\n// Source Code:\n{text}\n\n"
        expected_tokens = []
        
        for label in labels:
            start, end, label_name = label["start_offset"], label["end_offset"], label["label"]
            token_text = text[start:end]
            expected_tokens.append((token_text, label_name))
            test_case += f"{token_text} -> {label_name}\n"

        file_path = os.path.join(TEST_CASES_DIR, f"test_case_{i+1}.txt")
        with open(file_path, "w", encoding="utf-8") as f:
            f.write(test_case)
        print(f"✅ Saved: {file_path}")
        
        test_files.append((file_path, expected_tokens))

    return test_files

# ---------------- STEP 3: Run ANTLR Validation ----------------
def run_antlr_validation(test_files):
    """Runs ANTLR lexer and compares expected vs. actual tokens."""
    report = []

    for test_file, expected_tokens in test_files:
        print(f"🔍 Running ANTLR on {test_file}...")
        cmd = f"grun MyLang tokens {test_file}"
        result = subprocess.run(cmd, shell=True, capture_output=True, text=True)

        actual_tokens = parse_antlr_output(result.stdout)
        errors = compare_tokens(expected_tokens, actual_tokens)

        test_result = "Pass" if not errors else "Fail"
        report.append([test_file, test_result, "; ".join(errors)])

        print(f"✅ Test Completed: {test_file} → {test_result}")

    save_report(report)

def parse_antlr_output(output):
    """Parses ANTLR's token output into a structured format."""
    tokens = []
    for line in output.splitlines():
        parts = line.split()
        if len(parts) >= 2:
            token_text, token_label = parts[0], parts[-1]
            tokens.append((token_text, token_label))
    return tokens

def compare_tokens(expected, actual):
    """Compares expected tokens from Doccano with actual ANTLR tokens."""
    errors = []
    for (exp_text, exp_label), (act_text, act_label) in zip(expected, actual):
        if exp_text != act_text or exp_label != act_label:
            errors.append(f"Expected: {exp_text} ({exp_label}) | Got: {act_text} ({act_label})")
    return errors

def save_report(report):
    """Saves validation results to a CSV file."""
    with open(VALIDATION_REPORT, "w", newline="", encoding="utf-8") as f:
        writer = csv.writer(f)
        writer.writerow(["Test Case", "Result", "Errors"])
        writer.writerows(report)
    print(f"📊 Validation Report Saved: {VALIDATION_REPORT}")

# ---------------- EXECUTE STEPS ----------------
if __name__ == "__main__":
    PROJECT_ID = 1  # Change this to your actual project ID in Doccano
    test_files = save_annotations_as_antlr_tests(PROJECT_ID)
    run_antlr_validation(test_files)
```

---

## **🎯 Next Steps**
- **Integrate real-time feedback** in Doccano UI.
- **Automate batch testing** of ANTLR with Doccano-labeled data.
- **Extend to multi-language support** in ANTLR grammars.
