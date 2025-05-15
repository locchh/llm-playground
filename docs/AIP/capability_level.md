# Capability Level

## 1. Core Logic (Internal Computing)

* **Definition**: Pure computation and algorithms without interaction with the outside world.
* **Scope**:

  * Business logic
  * Data processing
  * Algorithm implementation
* **Examples**:

  * Sorting algorithms
  * Math utilities
  * In-memory data structures
* **Traits**:

  * Testable in isolation
  * Deterministic
  * No side effects

---

## 2. IO Interaction

* **Definition**: Deals with reading and writing from/to external systems or interfaces.
* **Scope**:

  * File system
  * User input/output
  * Logging
* **Examples**:

  * Reading files
  * Writing console output
  * Accepting user input via CLI
* **Traits**:

  * Side-effecting operations
  * Needs proper error handling

---

## 3. Dependency Use

* **Definition**: Utilization of external libraries, frameworks, and APIs.
* **Scope**:

  * SDKs, third-party services
  * Framework hooks
  * Middleware
* **Examples**:

  * Using NumPy for computation
  * HTTP client libraries
* **Traits**:

  * Reuse and abstraction
  * Dependency management and updates

---

## 4. Application Layer

* **Definition**: Coordinates features and workflows for the end-user or external clients.
* **Scope**:

  * Web interfaces
  * API endpoints
  * CLI/GUI apps
* **Examples**:

  * Flask/Django routes
  * UI forms and validation
* **Traits**:

  * User-facing
  * Ties together internal logic and dependencies

---

## 5. System Integration

* **Definition**: Integration of multiple subsystems into a coherent system.
* **Scope**:

  * Service orchestration
  * Message queues
  * Auth, caching, logging, monitoring
* **Examples**:

  * RESTful microservices
  * Kafka consumers/producers
  * CI/CD pipelines
* **Traits**:

  * Coordination across modules
  * Requires knowledge of infrastructure and protocols

---

## 6. Distributed / Platform-Level Programming

* **Definition**: Design and development of scalable, resilient, and fault-tolerant systems across multiple machines or environments.
* **Scope**:

  * Distributed computing
  * Cloud-native design
  * Container orchestration (e.g., Kubernetes)
  * Platform SDKs (e.g., AWS Lambda, Azure Functions)
* **Examples**:

  * MapReduce, Spark jobs
  * Federated services
  * Serverless architecture
* **Traits**:

  * Network reliability, data consistency, performance trade-offs
  * High complexity and scalability