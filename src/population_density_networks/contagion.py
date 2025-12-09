import numpy as np
import pandas as pd
from typing import Union

from .model import run_single_simulation


class SimpleContagionModel:
    def __init__(
        self,
        network: pd.DataFrame,
        initial_infected: Union[int, float],
        infection_rate: float,
    ) -> None:
        """
        Initialize contagion model.

        Parameters
        ----------
        network :           Network adjacency matrix.
        initial_infected :  Initial infected fraction of poulation or number of individuals.
        infection_rate :    Infection rate.
        """
        self.network = network
        self.initial_infected = initial_infected
        self.infection_rate = infection_rate

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
        })


    def run_simulation(self, time_steps: int) -> None:
        """
        Run simulation of simple contagion model for a given number of time steps.

        Parameters
        ----------
        time_steps :    Number of time steps to run the simulation for.
        """
        for t in range(1, time_steps + 1):

            # Get current and next states
            current_states = self.individual_states.copy()
            next_states = current_states.copy()

            # Loop through suseptible individuals
            suseptible_indices = np.where(current_states == 0)[0]
            for i in suseptible_indices:

                # Get neighbors
                neighbors = self.network.iloc[i, :]
                total_infected_neighbors = np.dot(neighbors, current_states)

                # Calculate infection probability
                p_not_infected = (1 - self.infection_rate) ** total_infected_neighbors
                p_infected = 1 - p_not_infected
                
                # Determine if individual is infected
                next_states[i] = np.random.choice([0, 1], p=[p_not_infected, p_infected])

            # Update states
            self.individual_states = next_states.copy()
            time_step_states = pd.DataFrame({
                'time': [t],
                'susceptible': [np.sum(self.individual_states == 0)],
                'infected': [np.sum(self.individual_states == 1)],
            })
            self.state_time_series = pd.concat([self.state_time_series, time_step_states], ignore_index=True)
        
        return None


class ComplexContagionModel:
    def __init__(
        self,
        network: pd.DataFrame,
        initial_infected: Union[int, float],
        thresholds: list[float] = None,
    ) -> None:
        """
        Initialize contagion model.

        Parameters
        ----------
        network :           Network adjacency matrix.
        initial_infected :  Initial infected fraction of poulation or number of individuals.
        thresholds :        (Optional) List of thresholds for each indvidual. If none, we will initialize using uniform distribution.
        """
        self.network = network
        self.initial_infected = initial_infected
        
        # Set up thresholds
        if thresholds is None:
            lower = 1e-6
            upper = 1 - lower
            self.thresholds = np.random.uniform(lower, upper, size=network.shape[0])  # ensure thresholds are not 0 or 1
        else:
            self.thresholds = np.asarray(thresholds, dtype=float)  # ensure thresholds are a numpy array

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
        })

    
    def run_simulation(self, time_steps: int) -> None:
        """
        Run simulation of complex contagion model for a given number of time steps.

        Parameters
        ----------
        time_steps :    Number of time steps to run the simulation for.
        """
        for t in range(1, time_steps + 1):

            # Get current and next states
            current_states = self.individual_states.copy()
            next_states = current_states.copy()

            # Grab information on suseptible individuals and their neighbors
            suseptible_indices = np.where(current_states == 0)[0]
            sub_network = self.network.iloc[suseptible_indices, :]
            active_neighbors = np.dot(sub_network, current_states)
            total_neighbors = sub_network.sum(axis=1)
            p_active_neighbors = active_neighbors / total_neighbors

            # Determine if new individuals are activated/infected
            acvivation_indices = np.where(p_active_neighbors > self.thresholds[suseptible_indices])[0]
            next_states[suseptible_indices[acvivation_indices]] = 1

            # Update states
            self.individual_states = next_states.copy()
            time_step_states = pd.DataFrame({
                'time': [t],
                'susceptible': [np.sum(self.individual_states == 0)],
                'infected': [np.sum(self.individual_states == 1)],
            })
            self.state_time_series = pd.concat([self.state_time_series, time_step_states], ignore_index=True)

        return None
