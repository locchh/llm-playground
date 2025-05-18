## Overview

You can transcode Japanese text from one encoding (EUC-JP, ISO-2022-JP, Shift\_JIS, etc.) into another (UTF-8, Shift\_JIS) without altering the “look” of the characters, so long as you:

1. **Decode** the source bytes using the *correct* source encoding.
2. **Normalize** (if desired) to a consistent Unicode canonical form (NFC or NFD).
3. **Re-encode** into your *target* encoding.

If any step is done incorrectly—e.g. using the wrong source encoding or skipping normalization—you may see replacement marks like `?` or `�`. The recipes below will help you avoid that.

---

## 1. Command-Line Converters

### 1.1 iconv

A lightweight, ubiquitous tool for basic encoding conversions.

```bash
# EUC-JP → UTF-8
iconv -f euc-jp   -t utf-8     input.txt    > output_utf8.txt

# UTF-8 → Shift_JIS
iconv -f utf-8    -t shift_jis input_utf8.txt > output_sjis.txt

# (Optional) Normalize to NFC via ICU’s uconv
iconv -f utf-8 -t utf-8 input.txt | uconv -x any-nfc > normalized.txt
```

* `-f`: source encoding
* `-t`: target encoding

---

### 1.2 nkf (Network Kanji Filter)

Built specifically for Japanese. Auto-detects encodings, fixes newlines, and handles MIME mail.

```bash
# Shift_JIS → UTF-8
nkf -w input.txt > output_utf8.txt

# UTF-8 → Shift_JIS
nkf -s input.txt > output_sjis.txt

# Guess encoding
nkf --guess input.txt
```

**Install**

```bash
# macOS
brew install nkf

# Ubuntu
sudo apt install nkf
```

---

### 1.3 uconv (ICU)

Part of the International Components for Unicode—great for conversions plus normalization.

```bash
# Shift_JIS → UTF-8
uconv -f shift_jis -t utf-8 input.txt > output.txt

# Normalize (NFC)
uconv -x any-nfc input.txt > normalized.txt
```

**Install**

```bash
sudo apt install icu-devtools
```

---

### 1.4 enca (Extremely Naive Charset Analyser)

Detects unknown encodings; pair with another tool for conversion.

```bash
enca -L japanese input.txt
# (use its output as the `-f` argument to iconv/uconv/nkf)
```

---

## 2. Programmable Pipelines in Python

### 2.1 Direct Conversion + Normalization

```python
import io
import unicodedata

# 1) Read raw bytes, decode with source encoding
with open("input_sjis.txt", "rb") as f:
    raw = f.read()
text = raw.decode("shift_jis")

# 2) Normalize to NFC (optional, but recommended)
text = unicodedata.normalize("NFC", text)

# 3a) Write as UTF-8
with io.open("output_utf8.txt", "w", encoding="utf-8") as f:
    f.write(text)

# 3b) Or write back to Shift_JIS
with io.open("output_sjis.txt", "w", encoding="shift_jis") as f:
    f.write(text)
```

### 2.2 Auto-Detection + Conversion

```python
import chardet
import unicodedata
import io

# 1) Detect
with open("input.txt", "rb") as f:
    raw = f.read()
enc = chardet.detect(raw)['encoding']

# 2) Decode → normalize
text = raw.decode(enc)
text = unicodedata.normalize("NFC", text)

# 3) Re-encode to UTF-8
with io.open("output_utf8.txt", "w", encoding="utf-8") as f:
    f.write(text)
```

> **Tip:** you can swap `"utf-8"` for `"shift_jis"` (or any supported codec) in step 3 to target a different encoding.

---

## 3. Quick-Reference Summary

| Tool       | Detection                      | Conversion                      | Normalization             | Best for…                         |
| ---------- | ------------------------------ | ------------------------------- | ------------------------- | --------------------------------- |
| **nkf**    | ✔️ auto                        | SJIS/EUC/ISO/UTF8               | —                         | Japanese-specific pipelines       |
| **uconv**  | —                              | 🌐 many encodings               | NFC, NFD, etc.            | Unicode-cleanup + transforms      |
| **iconv**  | —                              | 🌐 many encodings               | — (use uconv next)        | Lightweight, everywhere available |
| **enca**   | ✔️                             | (none)                          | —                         | Identifying unknown source enc.   |
| **Python** | via chardet/charset-normalizer | any codec via `decode`/`encode` | NFC/NFD via `unicodedata` | Custom scripts & batch jobs       |
