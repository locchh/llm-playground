To train large language models (LLMs) using policy gradient methods, particularly with techniques like **KL penalty gradients** and **Proximal Policy Optimization (PPO)**, we use mathematical formulations to guide updates to the model's parameters. Let me break down each method with the respective formulas and their explanations.

---

### 1. **Policy Gradient with KL Penalty**
The objective with a KL penalty balances exploration (new predictions) and staying close to the current policy (to prevent drastic changes). The loss function is:

\[
L(\theta) = \mathbb{E}_{x \sim \text{data}, a \sim \pi_\theta} \left[ \log \pi_\theta(a|x) R(a, x) - \beta \text{KL}(\pi_\theta || \pi_{\text{old}}) \right]
\]

#### Explanation of terms:
- \( \theta \): The parameters of the policy (e.g., weights of the model).
- \( x \): Context or input (e.g., a sentence prefix).
- \( a \): Action (e.g., the token chosen by the model).
- \( \pi_\theta(a|x) \): The probability of taking action \( a \) under policy \( \pi_\theta \).
- \( R(a, x) \): The reward for taking action \( a \) in context \( x \) (e.g., based on human feedback or task-specific metrics).
- \( \beta \): A hyperparameter controlling the strength of the KL penalty.
- \( \text{KL}(\pi_\theta || \pi_{\text{old}}) \): The Kullback-Leibler divergence between the current policy (\( \pi_\theta \)) and the old policy (\( \pi_{\text{old}} \)).

#### Steps:
1. The term \( \log \pi_\theta(a|x) R(a, x) \) encourages actions with higher rewards.
2. The KL term \( \beta \text{KL}(\pi_\theta || \pi_{\text{old}}) \) penalizes large deviations from the previous policy.

---

### 2. **Proximal Policy Optimization (PPO)**

PPO is designed to optimize policies while ensuring updates are constrained to prevent overly aggressive changes. The clipped objective is:

\[
L(\theta) = \mathbb{E}_{x, a} \left[ \min \left( r_\theta(a|x) \hat{A}(x, a), \text{clip}(r_\theta(a|x), 1 - \epsilon, 1 + \epsilon) \hat{A}(x, a) \right) \right]
\]

#### Explanation of terms:
- \( r_\theta(a|x) = \frac{\pi_\theta(a|x)}{\pi_{\text{old}}(a|x)} \): The ratio of probabilities under the new and old policies.
- \( \hat{A}(x, a) \): The advantage function estimating how much better action \( a \) is compared to the average action at \( x \).
- \( \epsilon \): A small hyperparameter controlling the clipping range.
- \( \text{clip}(r_\theta(a|x), 1 - \epsilon, 1 + \epsilon) \): Ensures that the probability ratio remains within a specified range, preventing overly large updates.

#### Steps:
1. \( r_\theta(a|x) \hat{A}(x, a) \): Encourages actions that have a high advantage.
2. The clipping \( \text{clip}(r_\theta(a|x), 1 - \epsilon, 1 + \epsilon) \) ensures the update to the policy stays within a safe region to avoid destabilization.
3. The objective is the minimum of the unclipped and clipped terms, which avoids the risk of overly optimistic updates.

---

### 3. **KL Penalty Gradient in PPO**

Some variations of PPO incorporate an explicit KL penalty instead of clipping. The objective in such cases is:

\[
L(\theta) = \mathbb{E}_{x, a} \left[ r_\theta(a|x) \hat{A}(x, a) - \beta \text{KL}(\pi_\theta || \pi_{\text{old}}) \right]
\]

#### Explanation:
This combines the clipping-free PPO update with the KL penalty term to ensure controlled policy updates.

- The **advantage term** encourages exploration based on observed rewards.
- The **KL term** penalizes divergence from the previous policy, similar to the earlier KL penalty method.

---

### Summary of Differences:
- **KL Penalty**: Explicitly penalizes divergence from the previous policy with a hyperparameter \( \beta \).
- **PPO**: Uses clipping to constrain updates instead of an explicit KL penalty. It ensures stability while allowing flexibility.
- **KL in PPO**: A hybrid approach that combines advantage-driven updates with a KL penalty for more explicit control.

Each approach has trade-offs and can be tuned based on the model size, task complexity, and stability requirements. Let me know if you want a deeper dive into any of these!