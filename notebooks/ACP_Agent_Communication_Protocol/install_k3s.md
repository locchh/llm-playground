Here’s a **quick and practical guide** to run **K3s** (Lightweight Kubernetes) on **Linux**:

---

### ✅ 1. **Prepare Your Linux Machine**

* Any modern Linux distro (Ubuntu, Debian, CentOS, Rocky, etc.)
* 1 CPU, 512 MB RAM (bare minimum), more for production
* `curl` and `bash` installed

---

### ✅ 2. **Install K3s (Single Node)**

Run this command as **root** or with `sudo`:

```bash
curl -sfL https://get.k3s.io | sh -
```

👉 This installs:

* Kubernetes (k3s binary)
* `k3s` service (Systemd)

---

### ✅ 3. **Check the Service**

```bash
sudo systemctl status k3s
```

To see logs:

```bash
sudo journalctl -u k3s -f
```

---

### ✅ 4. **Verify K3s Cluster**

```bash
sudo k3s kubectl get nodes
```

Optional: Create an alias so you can just type `kubectl`:

```bash
alias kubectl='sudo k3s kubectl'
```

---

### ✅ 5. **Deploy Something (Example: NGINX)**

```bash
kubectl create deployment nginx --image=nginx
kubectl expose deployment nginx --port=80 --type=NodePort
kubectl get svc
```

Access via:

```
http://<your-server-ip>:<NodePort>
```

---

### ✅ 6. **Uninstall (If Needed)**

```bash
/usr/local/bin/k3s-uninstall.sh
```

---

### 🔑 Notes:

* For **multi-node clusters**, you need to set a **server** (control plane) and **agents** (workers) with `K3S_URL` and `K3S_TOKEN`.
* Example:

```bash
# On server
curl -sfL https://get.k3s.io | sh -

# On agent
curl -sfL https://get.k3s.io | K3S_URL=https://<server-ip>:6443 K3S_TOKEN=<token> sh -
```

Find the token on the server:

```bash
sudo cat /var/lib/rancher/k3s/server/node-token
```

---

✅ K3s is excellent for:

* **Lightweight Kubernetes clusters**
* **Edge computing**
* **Dev/Test environments**

If you tell me your use case (single node, dev, multi-node, production), I can help tailor the commands.
