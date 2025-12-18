#!/usr/bin/env python3

import argparse
import json
import sys
import os
sys.path.append(os.path.dirname(__file__))

from my_torch.Neuron import Neuron

def load_config(config_file):
    """Load network configuration from JSON file"""
    try:
        with open(config_file, 'r') as f:
            config = json.load(f)

        required_fields = ['name', 'architecture', 'hyperparameters']
        for field in required_fields:
            if field not in config:
                raise ValueError(f"Missing required field '{field}' in config file")

        if not isinstance(config['architecture'], list) or len(config['architecture']) == 0:
            raise ValueError("Architecture must be a non-empty list of layers")

        for i, layer in enumerate(config['architecture']):
            required_layer_fields = ['input_size', 'output_size', 'activation']
            for field in required_layer_fields:
                if field not in layer:
                    raise ValueError(f"Layer {i}: missing required field '{field}'")

        hyperparams = config['hyperparameters']
        if 'loss' not in hyperparams:
            raise ValueError("Missing 'loss' in hyperparameters")
        if 'learning_rate' not in hyperparams:
            raise ValueError("Missing 'learning_rate' in hyperparameters")

        return config

    except FileNotFoundError:
        print(f"Error: Configuration file '{config_file}' not found", file=sys.stderr)
        sys.exit(84)
    except json.JSONDecodeError as e:
        print(f"Error: Invalid JSON in '{config_file}': {e}", file=sys.stderr)
        sys.exit(84)
    except ValueError as e:
        print(f"Error: {e}", file=sys.stderr)
        sys.exit(84)

def create_network_from_config(config):
    """Create a neural network from configuration"""
    hyperparams = config['hyperparameters']

    network = Neuron(
        loss=hyperparams['loss'],
        learning_rate=hyperparams['learning_rate']
    )

    for layer_config in config['architecture']:
        network.add_layer(
            input_size=layer_config['input_size'],
            output_size=layer_config['output_size'],
            activation=layer_config['activation']
        )

    return network

def generate_networks_from_config(config_file, count):
    """Generate multiple networks from a single configuration file"""
    config = load_config(config_file)
    base_name = config['name']

    print(f"Generating {count} networks from configuration '{config_file}'")
    print(f"Network name: {base_name}")
    print(f"Architecture: {len(config['architecture'])} layers")

    for i in range(1, count + 1):
        network = create_network_from_config(config)
        filename = f"{base_name}_{i}.nn"
        network.save(filename)
        print(f"Generated {filename}")

def main():
    parser = argparse.ArgumentParser(
        description='Generate MY_TORCH neural networks from configuration files',
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog='''Example usage:
  python3 my_torch_generator.py config1.json 5 config2.json 3
This generates 5 networks from config1.json and 3 from config2.json.'''
    )

    parser.add_argument(
        'configs',
        nargs='+',
        help='Configuration file and number pairs: config_file count [config_file count ...]'
    )

    if len(sys.argv) == 1:
        parser.print_help()
        sys.exit(0)

    if len(sys.argv) % 2 == 0:
        print("Error: Invalid number of arguments. Expected pairs of config_file and count.", file=sys.stderr)
        parser.print_help()
        sys.exit(84)

    try:
        config_pairs = []
        for i in range(1, len(sys.argv), 2):
            config_file = sys.argv[i]
            count = int(sys.argv[i + 1])
            if count <= 0:
                raise ValueError(f"Count must be positive, got {count}")
            config_pairs.append((config_file, count))

    except (IndexError, ValueError) as e:
        print(f"Error: {e}", file=sys.stderr)
        parser.print_help()
        sys.exit(84)

    for config_file, count in config_pairs:
        generate_networks_from_config(config_file, count)

    print(f"\nSuccessfully generated networks from {len(config_pairs)} configuration(s)")

if __name__ == '__main__':
    main()