The error message `Failed to start sshd.service: Unit sshd.service not found.` means that the SSH server (`sshd`) is not installed on the system or the service is named differently. Here’s how to resolve this:

### Steps to Fix

---

### 1. **Install the OpenSSH Server**

If SSH is not installed, you need to install the `openssh-server` package. Here's how you can do it depending on your Linux distribution:

#### **For Ubuntu/Debian-based systems**:
   ```bash
   sudo apt update
   sudo apt install openssh-server
   ```

#### **For CentOS/RHEL-based systems**:
   ```bash
   sudo yum install openssh-server
   ```

#### **For Fedora**:
   ```bash
   sudo dnf install openssh-server
   ```

#### **For Arch Linux**:
   ```bash
   sudo pacman -S openssh
   ```

---

### 2. **Start and Enable the SSH Service**

Once the installation is complete, you need to start the SSH service and ensure it starts on boot:

   ```bash
   sudo systemctl start ssh
   sudo systemctl enable ssh
   ```

- **Note**: Some systems may refer to the SSH service as `ssh.service` instead of `sshd.service`. The command above (`systemctl start ssh`) will work for both cases.

---

### 3. **Verify the SSH Service is Running**

After starting the service, check if SSH is running properly:

   ```bash
   sudo systemctl status ssh
   ```

You should see output indicating that the service is active and running.

---

### 4. **Check if SSH is Listening on Port 22**

To ensure that SSH is listening for connections on port 22, run:

   ```bash
   sudo netstat -tuln | grep 22
   ```

or, using `ss` (a newer command):

   ```bash
   sudo ss -tuln | grep 22
   ```

This will show whether the SSH service is listening on the default port (22).

---

### 5. **Firewall Settings**

If SSH is still not accessible, make sure your firewall allows SSH connections:

#### **For Ubuntu/Debian with `ufw`**:
   ```bash
   sudo ufw allow ssh
   sudo ufw enable
   ```

#### **For CentOS/RHEL with `firewalld`**:
   ```bash
   sudo firewall-cmd --permanent --add-service=ssh
   sudo firewall-cmd --reload
   ```

---

### Summary

1. Install the `openssh-server` package using your package manager.
2. Start and enable the SSH service.
3. Check that the SSH service is running and listening on port 22.
4. Ensure that the firewall allows SSH connections.

Once installed and running, you should be able to access the server via SSH.