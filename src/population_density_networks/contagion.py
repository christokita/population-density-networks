import numpy as np
import pandas as pd
from abc import ABC, abstractmethod
from typing import Any, Dict, Union

from .model import run_single_simulation


class ContagionModel(ABC):
    """Base class for contagion models."""
    
    def __init__(
        self,
        network: pd.DataFrame,
        initial_infected: Union[int, float],
    ) -> None:
        """
        Initialize base contagion model.

        Parameters
        ----------
        network :           Network adjacency matrix.
        initial_infected :  Initial infected number of individuals or fraction of population.
        """
        self.network = network
        self.initial_infected = initial_infected

        # Set up initially infected individuals
        if isinstance(initial_infected, float):
            self.individual_states = np.random.choice([0, 1], size=network.shape[0], p=[1 - initial_infected, initial_infected])
        else:
            infected_indices = np.random.choice(network.shape[0], size=initial_infected, replace=False)
            self.individual_states = np.zeros(network.shape[0], dtype=int)
            self.individual_states[infected_indices] = 1

        # Set up time series of states
        self.state_time_series = pd.DataFrame({
            'time': [0],
            'susceptible': [np.sum(self.individual_states == 0)],
            'infected': [np.sum(self.individual_states == 1)],
            'pct_infected': [np.sum(self.individual_states == 1) / network.shape[0]],
        })

    @abstractmethod
    def run_simulation(self, time_steps: int) -> None:
        """
        Run simulation of contagion model for a given number of time steps.

        Parameters
        ----------
        time_steps :    Number of time steps to run the simulation for.
        """
        pass


class SimpleContagionModel(ContagionModel):
    def __init__(
        self,
        network: pd.DataFrame,
        initial_infected: Union[int, float],
        infection_probability: float,
    ) -> None:
        """
        Initialize simple contagion model.

        Parameters
        ----------
        network :                   Network adjacency matrix.
        initial_infected :          Initial infected fraction of population or number of individuals.
        infection_probability :     Probability of infection.
        """
        super().__init__(network, initial_infected)
        self.infection_probability = infection_probability


    def run_simulation(self, time_steps: int) -> None:
        """
        Run simulation of simple contagion model for a given number of time steps.

        Parameters
        ----------
        time_steps :    Number of time steps to run the simulation for.
        """
        # Pre calculate 1 - beta
        one_minus_beta = 1 - self.infection_probability

        # Run simulation
        for t in range(1, time_steps + 1):

            # Get current and next states
            current_states = self.individual_states.copy()

            # Count infected neighbors for all nodes
            infected_neighbors = self.network @ self.individual_states

            # Infection probability for all nodes: p_i = 1 - (1-beta) ** m_i
            p = 1.0 - np.power(one_minus_beta, infected_neighbors)

            # Draw infections in one shot
            infected_draw = np.random.random(size=self.individual_states.size) < p
            susceptible = (self.individual_states == 0)
            new_infections = susceptible & infected_draw

            # Update states
            self.individual_states[new_infections] = 1
            time_step_states = pd.DataFrame({
                'time': [t],
                'susceptible': [np.sum(self.individual_states == 0)],
                'infected': [np.sum(self.individual_states == 1)],
                'pct_infected': [np.sum(self.individual_states == 1) / self.network.shape[0]],
            })
            self.state_time_series = pd.concat([self.state_time_series, time_step_states], ignore_index=True)
        
        return None


class ComplexContagionModel(ContagionModel):
    def __init__(
        self,
        network: pd.DataFrame,
        initial_infected: Union[int, float],
        thresholds: list[float] = None,
    ) -> None:
        """
        Initialize complex contagion model.

        Parameters
        ----------
        network :           Network adjacency matrix.
        initial_infected :  Initial infected fraction of population or number of individuals.
        thresholds :        (Optional) List of thresholds for each individual. If none, we will initialize using uniform distribution.
        """
        super().__init__(network, initial_infected)
        
        # Set up thresholds
        if thresholds is None:
            lower = 1e-6
            upper = 1 - lower
            self.thresholds = np.random.uniform(lower, upper, size=network.shape[0])  # ensure thresholds are not 0 or 1
        else:
            self.thresholds = np.asarray(thresholds, dtype=float)  # ensure thresholds are a numpy array

    
    def run_simulation(self, time_steps: int) -> None:
        """
        Run simulation of complex contagion model for a given number of time steps.

        Parameters
        ----------
        time_steps :    Number of time steps to run the simulation for.
        """
        for t in range(1, time_steps + 1):

            # Grab information on suseptible individuals and their neighbors
            suseptible_indices = np.where(self.individual_states == 0)[0]
            sub_network = self.network.iloc[suseptible_indices, :]
            active_neighbors = np.dot(sub_network, self.individual_states)
            total_neighbors = sub_network.sum(axis=1)
            p_active_neighbors = active_neighbors / total_neighbors

            # Determine if new individuals are activated/infected
            activation_indices = np.where(p_active_neighbors > self.thresholds[suseptible_indices])[0]

            # Update states
            self.individual_states[suseptible_indices[activation_indices]] = 1
            time_step_states = pd.DataFrame({
                'time': [t],
                'susceptible': [np.sum(self.individual_states == 0)],
                'infected': [np.sum(self.individual_states == 1)],
                'pct_infected': [np.sum(self.individual_states == 1) / self.network.shape[0]],
            })
            self.state_time_series = pd.concat([self.state_time_series, time_step_states], ignore_index=True)

        return None


def analyze_contagion_results(contagion_model: ContagionModel) -> Dict[str, Any]:
    """
    Analyze contagion spread dynamics in a single simulation.

    Args:
    ----
    contagion_model:  Simulated contagion model

    Returns:
    -------
    Statistics describing the contagion spread dynamics:
    - Final fraction of infected population
    - Time to reach majority (>50%) infected
    - Whether the network reached majority spread
    - Max slope in infected fraction over time
    """
    # Calculate additional time series metrics
    contagion_model.state_time_series['infected_fraction'] = contagion_model.state_time_series['infected'] / (contagion_model.state_time_series['susceptible'] + contagion_model.state_time_series['infected'])
    contagion_model.state_time_series['change_in_infected_fraction'] = contagion_model.state_time_series['infected_fraction'].diff()

    # Get final fraction of infected population
    final_infected_fraction = contagion_model.state_time_series.iloc[-1]['infected_fraction']

    # Get time to reach majority (>50%) infected
    try:
        majority_time = contagion_model.state_time_series[contagion_model.state_time_series['infected_fraction'] > 0.5]['time'].iloc[0]
    except IndexError:
        majority_time = None

    # Get whether the network reached majority spread
    reached_majority_spread = majority_time is not None

    # Get biggest slope in infected fraction over time
    max_slope = contagion_model.state_time_series['change_in_infected_fraction'].max()

    return {
        'final_infected_fraction': final_infected_fraction,
        'time_to_majority': majority_time,
        'reached_majority_spread': reached_majority_spread,
        'max_slope': max_slope,
    }