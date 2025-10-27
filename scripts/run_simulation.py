#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sun Aug 17 14:01:17 2025

@author: ChrisTokita
"""

from src.population_density_networks import model


# Paramters
N = 1000
K_CAP_MEAN = 100
K_CAP_SD = 50

# Run simulation
model_run = model.NetworkFormationModel(
    n=N,
    k_cap_mean=K_CAP_MEAN,
    k_cap_sd=K_CAP_SD
)

model_run.set_up_world(density=1000.0)
model_run.create_social_network(rounds=10000)

