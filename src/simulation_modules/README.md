# Simulation Modules

This directory contains the Fortran implementations of active simulation modules that can be configured and toggled in the simulation framework.

---

## Permanent Modules (Internal)
These modules are always active and handle core agent and simulation updates.

### 0.1 Update Agent Age
* **Source:** `mod_technical_modules.f95` (Subroutine: `update_agent_age`)
* **Description:** Increments the age (in ticks) of all alive agents, increases pregnancy timers, and manages gestation progress.

### 0.2 Realise Births
* **Source:** `mod_technical_modules.f95` (Subroutine: `realise_births`)
* **Description:** Handles the birth of new agents from pregnant females.
* **Logic:** Spawns a child agent at the mother's position when gestation is complete, then resets her pregnancy status.

---

## Configurable Simulation Modules
These modules are toggled on/off dynamically via the simulation configuration (`active_module_ids`).

### 12. Reviewed Death
* **Source:** `mod_reviewed_modules.f95` (Subroutine: `reviewed_death`)
* **Description:** Reviewed agent-centric death module.
* **Logic:** Computes age-dependent and environmental mortality risk using reviewed parameters. Kills agents using `call agent_ptr%agent_dies(reason)`.

### 13. Reviewed Birth
* **Source:** `mod_reviewed_modules.f95` (Subroutine: `reviewed_birth`)
* **Description:** Reviewed agent-centric birth module.
* **Logic:** Determines reproductive eligibility and handles spawning of offspring using reviewed fertility curves and parameters.

### 14. Move Children to Mothers
* **Source:** `mod_reviewed_modules.f95` (Subroutine: `move_children_to_mothers`)
* **Description:** Ensures dependent children move alongside their mother.
* **Logic:** Displaces child agents directly to their mother's spatial location each tick.

### 21. Reviewed Agent Motion
* **Source:** `mod_reviewed_modules.f95` (Subroutine: `reviewed_agent_motion`)
* **Description:** Standard reviewed agent-centric movement model.
* **Logic:** Implements random walk and drift movement based on environmental potential (HEP) and reviewed movement parameters.

### 22. Cluster Death (No Interaction)
* **Source:** `mod_birth_death_new.f95` (Subroutine: `new_death`)
* **Description:** Cluster-aware death module where each population operates with its **own independent** `MC_cl_AV` carrying capacity constraint.
* **Logic:** Evaluates age-based (Gompertz-Makeham) mortality. The Eq.26 fertility controller (`K_fertility(jp)`) is computed per-population using each population's individual `MC_cl_AV(jp)`.

### 23. Cluster Birth (No Interaction)
* **Source:** `mod_birth_death_new.f95` (Subroutine: `new_birth`)
* **Description:** Cluster-aware birth module where each population operates with its **own independent** `MC_cl_AV` carrying capacity constraint.
* **Logic:** Evaluates reproduction using `K_fertility(jp)` derived from the per-population `MC_cl_AV`. Interbreeding across populations controlled by `config%allow_across_populations`.

### 28. Cluster Death (Shared MC)
* **Source:** `mod_birth_death_new.f95` (Subroutine: `new_death_shared_mc`)
* **Description:** Cluster-aware death module where all populations **share a single carrying capacity** constraint computed as the population-count-weighted average of `MC_cl_AV` across populations.
* **Logic:** Delegates to the same age-based mortality as module 22. Death probability is not directly affected by MC; the shared constraint governs the fertility scaler `K_fertility_shared` used by module 29.

### 29. Cluster Birth (Shared MC)
* **Source:** `mod_birth_death_new.f95` (Subroutine: `new_birth_shared_mc`)
* **Description:** Cluster-aware birth module where all populations **share a single carrying capacity** constraint.
* **Logic:** Uses `K_fertility_shared` (a single scalar per cluster, computed in `update_cluster_macroscopic_fertility_scale_shared_mc`) instead of the per-population `K_fertility(jp)`. The shared MC is the agent-count-weighted average of `MC_cl_AV(jp)` for populations with living agents in that cluster. Populations with 0 alive agents contribute weight 0. Interbreeding controlled by `config%allow_across_populations`.

---

## C3 Creativity Modules

### 24. Creativity (C3)
* **Source:** `mod_creativity.f95` (Subroutine: `update_creativity`)
* **Description:** Individual creativity evolution module (throttled by `c3_individual_update_interval`).
* **Logic:** Simulates individual creativity growth using neighbor scanning.

### 25. Cluster Creativity (C3)
* **Source:** `mod_creativity.f95` (Subroutine: `accumulate_cluster_creativity`)
* **Description:** Cheap per-tick accumulation of individual agent creativity into cluster sums.
* **Logic:** Sums agent creativity scores into their respective spatial cluster structures.

### 26. Creativity Simple (C3)
* **Source:** `mod_creativity_simple.f95` (Subroutine: `update_creativity_simple`)
* **Description:** Cell-aggregated individual creativity evolution module using an efficient $O(N)$ grid sweep.
* **Logic:** Aggregates creativity scores within grid cells to update agent parameters.

### 27. Creativity Fast (C3)
* **Source:** `mod_creativity_fast.f95` (Subroutine: `update_creativity_fast`)
* **Description:** High-performance top individuals creativity module utilizing an optimized $O(N)$ sweep.
