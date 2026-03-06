# Simulation Modules

This directory contains the Fortran implementations for the various simulation modules that can be toggled in the `SpawnPointEditor`.

Below is a description of each module:

## 1. Natural Deaths
**Source:** `mod_birth_death_agb.f95` (Subroutine: `realise_natural_deaths`)
**Description:**
Implements natural mortality based on age. The probability of death is calculated using a Gompertz-Makeham-like law where the probability increases with age.
- **Input:** Agent's age.
- **Logic:** Calculates a probability `prob` based on age (in years). If a random number `r < prob`, the agent dies (Reason: 1).

## 2. Births
**Source:** `mod_birth_technical.f95` (Subroutine: `realise_births`)
**Description:**
Handles the birth of new agents from pregnant females.
- **Conditions:** Agent must be pregnant for at least `pregnancy_minimum_length` ticks.
- **Logic:**
    - Checks if pregnancy duration is sufficient.
    - Checks a random probability `birth_prob_after_min_length`.
    - If successful, a new agent is spawned.
    - The new agent inherits position from the mother and has `age=0`.
    - The father is identified (if alive) to link lineage.

## 3. Move
**Source:** `mod_move.f95` (Subroutine: `agent_move`)
**Description:**
Standard random walk movement with a drift component.
- **Logic:**
    - Calculates a new position based on current velocity and random diffusion (`sigma`).
    - Checks boundary conditions (map edges).
    - Checks for water (agents die if they move into water, Reason: 3).
    - Updates the agent's position if valid.

## 4. Update Age
**Source:** `mod_birth_technical.f95` (Subroutine: `update_age_pregnancy`)
**Description:**
Simple incrementer for agent age and pregnancy duration.
- **Logic:**
    - `age = age + 1`
    - If `is_pregnant > 0`, `is_pregnant = is_pregnant + 1`.

## 5. Find Mate
**Source:** `mod_birth_death_agb.f95` (Subroutine: `find_mate`)
**Description:**
Enables reproduction by allowing agents to find partners.
- **Logic:**
    - Only active for Males (Females are passive in this model, or vice-versa depending on exact logic; here code says `if gender == 'M' return`, implying Females search?). *Correction: Code says `if gender == 'M' return`, so FEMALES scan for partners.*
    - Checks for potential partners in the same grid cell.
    - Conditions: Age (fertility), resources (if enabled), and random probability.
    - If successful, the agent becomes pregnant (`is_pregnant = 1`) and records the father's ID.

## 6. Distribute Ressources
**Source:** `mod_birth_death_agb.f95` (Subroutine: `distribute_ressources`)
**Description:**
Allocates resources from grid cells to agents.
- **Logic:**
    - Resets agent resources to 0 at start of tick.
    - Cells produce resources proportional to HEP (Human Environment Potential).
    - Agents in a cell compete for resources in rounds (e.g., take up to 5, then take up to 2).
    - Updates the agent's `avg_resources` history (running average over 12 ticks).

## 7. Resource Mortality
**Source:** `mod_birth_death_agb.f95` (Subroutine: `resource_mortality`)
**Description:**
Kills agents that fall below a resource threshold.
- **Logic:**
    - Checks `avg_resources`.
    - If `avg_resources < min_avg_resources_for_survival`, the agent dies (Reason: 2).

## 8. Langevin Move
**Source:** `mod_move.f95` (Subroutine: `agent_move_langevin`)
**Description:**
Advanced movement model using Langevin dynamics.
- **Logic:**
    - Calculates a gradient of HEP (attraction to better environments).
    - Updates velocity based on:
        - Gradient attraction (force)
        - Friction/Damping (drag)
        - Random diffusion (noise)
    - Updates position based on usage of velocity.

## 9. Birth Death
**Source:** `mod_birth_death_strict.f95` (Subroutine: `apply_birth_death_all_cells`)
**Description:**
A "God-mode" controller that forces the agent count in each cell to match a target capacity derived from HEP.
- **Logic:**
    - Calculates `target_count` = f(HEP).
    - If `current > target`: Randomly kills excess agents.
    - If `current < target`: Spawns new agents to fill the deficit.
- **Use Case:** Initializing stable populations or forcing equilibrium.

## 10. Verhulst Pressure
**Source:** `mod_birth_death_probabilistic.f95` (Subroutine: `apply_verhulst_pressure_all_cells`)
**Description:**
Applies density-dependent pressure (Verhulst logistic equation principle) as a mortality risk.
- **Logic:**
    - Calculates density `D = N / K` (Current / Capacity).
    - Increases mortality probability as `D` exceeds 1.0.
    - Dampens birth rates as `D` approaches 1.0.
    - More natural/stochastic than the "Birth Death" module.

## 11. Clustering
**Source:** `src/data_structures/mod_clustering.f95` (via `mod_python_interface`)
**Description:**
Runs a watershed clustering algorithm on the HEP surface to group agents into spatial clusters.
- **Logic:**
    - Analyzes the HEP map topology.
    - Assigns a Cluster ID to every grid cell.
    - Agents in those cells are logically grouped into that cluster.
    - Tracks migration of agents between clusters.

## 12. New Death
**Source:** `mod_birth_death_new.f95` (Subroutine: `new_death`)
**Description:**
Custom death module for workshop implementation. Runs on each alive agent.
- **Config Parameters:** `d1` through `d10` (all `real(8)`, default 0.0)
- **Logic:** User-defined. Skeleton ready for implementing custom death probability based on age, resources, location, etc.
- **Usage:** Use `agent_ptr%agent_dies(reason=6)` to kill agents with a custom reason code.

## 13. New Birth
**Source:** `mod_birth_death_new.f95` (Subroutine: `new_birth`)
**Description:**
Custom birth module for workshop implementation. Runs on each alive agent.
- **Config Parameters:** `b1` through `b10` (all `real(8)`, default 0.0)
- **Logic:** User-defined. Skeleton ready for implementing custom birth logic (fertility checks, probability gates, child spawning).
- **Usage:** Use `spawn_agent_hash` + `add_agent_to_array_hash` to create new agents.

## 14. New Preparation
**Source:** `mod_birth_death_new.f95` (Subroutine: `new_preparation`)
**Description:**
Custom grid preparation module for workshop implementation. Runs once per tick on the entire grid.
- **Config Parameters:** `p1` through `p10` (all `real(8)`, default 0.0)
- **Logic:** User-defined. Has access to the full grid structure including cell agent counts, HEP values, and all grid cell properties.
- **Usage:** Typically used to pre-compute carrying capacity, distribute resources, or update cell-level state before death/birth modules run.

## 15. Reviewed Move
**Source:** `mod_reviewed_modules.f95` (Subroutine: `reviewed_move`)
**Description:**
Reviewed movement module. Runs on each alive agent.
- **Config Parameters:** `r1` through `r10` (all `real(8)`, default 0.0)
- **Logic:** User-defined. Skeleton ready for implementing custom movement logic.
- **Usage:** Use `call agent_ptr%update_pos(new_x, new_y)` to move agents. Current tick available as argument `t`.

## 16. Reviewed Birth Grid
**Source:** `mod_reviewed_modules.f95` (Subroutine: `reviewed_birth_grid`)
**Description:**
Reviewed grid-centric birth module. Runs once per tick on the entire grid.
- **Config Parameters:** `r1` through `r10` (all `real(8)`, default 0.0)
- **Logic:** User-defined. Has full grid access for spatial birth logic.
- **Usage:** Use `spawn_agent_hash` + `add_agent_to_array_hash` to create new agents. Current tick available as argument `t`.

## 17. Reviewed Death AGB
**Source:** `mod_reviewed_modules.f95` (Subroutine: `reviewed_death_agb`)
**Description:**
Reviewed agent-centric death module. Runs on each alive agent.
- **Config Parameters:** `r1` through `r10` (all `real(8)`, default 0.0)
- **Logic:** User-defined. Skeleton ready for implementing custom death logic.
- **Usage:** Use `call agent_ptr%agent_dies(reason=6)` to kill agents. Current tick available as argument `t`.

## 18. Reviewed Death Grid
**Source:** `mod_reviewed_modules.f95` (Subroutine: `reviewed_death_grid`)
**Description:**
Reviewed grid-centric death module. Runs once per tick on the entire grid.
- **Config Parameters:** `r1` through `r10` (all `real(8)`, default 0.0)
- **Logic:** User-defined. Has full grid access for spatial death logic (e.g., overcrowding, resource depletion).
- **Usage:** Current tick available as argument `t`.
