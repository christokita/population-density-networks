import math
import networkx as nx
import numpy as np
import pandas as pd
from scipy import spatial
from tqdm import trange


class NetworkFormationModel:
    
    def __init__(
        self,
        n: int,
        k_cap_mean: float,
        k_cap_sd: float,
        epsilon: float = 0.1,
        radius: float = 1.0,
    ) -> None:
        """
        Initiate network formation model with specific parameters.
        
        
        Parameters
        ----------
        n :             number of individuals.
        k_cap_mean :    average maximum degree limit.
        k_cap_sd :      standard deviation of maximum degree limit.
        epsilon :       constant added to degree during network formation process.
        radius :        meximum distance for social connection formation
        """
        # Hyperparameters
        self.n = n
        self.k_cap_mean = k_cap_mean
        self.k_cap_sd = k_cap_sd
        self.epsilon = epsilon
        self.r = radius
        
        # Set individuals variables
        self.individuals = pd.DataFrame({'id': np.arange(n)})
        self.social_network = pd.DataFrame(0, index=range(n), columns=range(n))
        
        # Calculate and set degree caps
        k_sigma2 = np.log(1.0 + (k_cap_sd**2) / (k_cap_mean**2))
        k_sigma = np.sqrt(k_sigma2)
        k_mu = np.log(k_cap_mean) - 0.5 * k_sigma2
        self.k_limit = np.random.lognormal(mean=k_mu, sigma=k_sigma, size=self.n)


    def set_up_world(self, density: float) -> None:
        """
        Given desired density, place individuals on grid.
        
        Parameters
        ----------
        density :   desired population density.
        """
        # Calculate grid size
        D = math.sqrt(self.n / density)
        
        
        # Place individual on (torus) grid
        self.individuals['x'] = np.random.uniform(low=0, high=D, size=self.n)
        self.individuals['y'] = np.random.uniform(low=0, high=D, size=self.n)
        
        
    def create_social_network(self, rounds: int, show_progress: bool = True):
        """
        """
        # Calculate distances between all individuals
        distance_matrix = spatial.distance_matrix(
            self.individuals[['x', 'y']].values,
            self.individuals[['x', 'y']].values,
        )
        
        # Loop through individuals, creating edges
        for _ in trange(rounds, disable=not show_progress, desc="Forming social network"):
            
            # Pick focal individual
            i = np.random.choice(np.arange(self.n))
            
            # Only consider connection formation if agent has capacity
            if self.social_network.iloc[i, :].sum() < self.k_limit[i]:
                
                # Get degree of all individuals and distance from focal individual
                k = self.social_network.sum(axis=0).values
                d = distance_matrix[i, :]

                # Weighting function
                w = (k + self.epsilon) * ((d + self.r) ** -2)
                w[i] = 0.0  # Zero out self

                # Skip if no eligible candidates
                if w.sum() <= 0:
                    continue
            
                # Normalize probabilities
                p = w / w.sum()
                
                # Draw partner
                j = np.random.choice(np.arange(self.n), p=p)
                
                # Add social tie if possible
                if (
                        self.social_network.iloc[i, j] == 0 and
                        self.social_network.iloc[j, :].sum() < self.k_limit[j]
                ):
                    self.social_network.loc[i, j] = 1
                    self.social_network.loc[j, i] = 1
            
    # def create_social_network(self, rounds: int, show_progress: bool = True):
    #     """
    #     """
    #     # Calculate distances between all individuals
    #     distance_matrix = spatial.distance_matrix(
    #         self.individuals[['x', 'y']].values,
    #         self.individuals[['x', 'y']].values,
    #     )
        
    #     # Loop through individuals, creating edges
    #     for _ in trange(rounds, disable=not show_progress, desc="Forming social network"):
            
    #         # Pick focal individual
    #         i = np.random.choice(np.arange(self.n))
            
    #         # Only consider connection formation if agent has capacity
    #         if self.social_network.iloc[i, :].sum() < self.k_limit[i]:
                
    #             # Get degree of all individuals and neighborhood
    #             k = self.social_network.sum(axis=0).values
    #             neighborhood = self.social_network.iloc[i, :].values
                
    #             # Build local candidate set individuals in radius
    #             in_radius= distance_matrix[i, :] < self.r
    #             in_radius[i] = False  # exclude self
    #             candidates = np.where(
    #                 in_radius & 
    #                 (neighborhood == 0) & 
    #                 (k < self.k_limit)
    #             )[0]
                
    #             # Skip if no eligible candidates
    #             if candidates.size == 0:
    #                 continue 
                
    #             # Preferential attachment probability
    #             w = k[candidates] + self.epsilon
    #             p = w / w.sum()
                
    #             # Draw partner
    #             j = np.random.choice(candidates, p=p)
                
    #             # Add social tie if possible
    #             if (
    #                     self.social_network.iloc[i, j] == 0 and
    #                     self.social_network.iloc[j, :].sum() < self.k_limit[j]
    #             ):
    #                 self.social_network.loc[i, j] = 1
    #                 self.social_network.loc[j, i] = 1


def analyze_network_structure(network: pd.DataFrame) -> pd.Series:
    """
    Analyze the structure of the final social network.

    Returns
    -------
    Series containing key network metrics.

    """
    # Convert to netwokx graph
    g = nx.from_pandas_adjacency(network)
    
    # Check connectivity
    is_connected = nx.is_connected(g)
    num_components = nx.number_connected_components(g)
    
    # For disconnected graphs, calculate path metrics on largest component
    if is_connected:
        avg_shortest_path = nx.average_shortest_path_length(g)
        diameter = nx.diameter(g)
        largest_component_size = len(g)
    else:
        largest_cc = max(nx.connected_components(g), key=len)
        g_largest = g.subgraph(largest_cc)
        avg_shortest_path = nx.average_shortest_path_length(g_largest)
        diameter = nx.diameter(g_largest)
        largest_component_size = len(largest_cc)
    
    return pd.Series({
        'network_density': nx.density(g),
        'network_is_connected': is_connected,
        'network_num_components': num_components,
        'network_largest_component_size': largest_component_size,
        'network_avg_shortest_path': avg_shortest_path,  # On largest component if disconnected
        'network_diameter': diameter,  # On largest component if disconnected
        'network_clustering_coef': nx.average_clustering(g),
        'network_modularity': nx.community.modularity(g, communities=nx.community.greedy_modularity_communities(g), weight=None),
        'network_assortativity': nx.degree_assortativity_coefficient(g)
    })
    
