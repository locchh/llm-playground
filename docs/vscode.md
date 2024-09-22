To install Visual Studio Code (VSCode) on Ubuntu, follow these steps:

1. **Update the package index:**
   ```bash
   sudo apt update
   ```

2. **Install the required dependencies:**
   ```bash
   sudo apt install software-properties-common apt-transport-https wget
   ```

3. **Import the Microsoft GPG key:**
   ```bash
   wget -q https://packages.microsoft.com/keys/microsoft.asc -O- | sudo apt-key add -
   ```

4. **Enable the VSCode repository:**
   ```bash
   sudo add-apt-repository "deb [arch=amd64] https://packages.microsoft.com/repos/vscode stable main"
   ```

5. **Install VSCode:**
   ```bash
   sudo apt update
   sudo apt install code
   ```

6. **Launch VSCode:**
   After installation, you can launch it from the applications menu or by running:
   ```bash
   code
   ```