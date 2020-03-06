SERVER_NAMES = ['Hill', 'Jaquez', 'Smith', 'Campbell', 'Singleton']

# 11980 11981 11982 11983 11984
SERVER_PORTS = {
    'Hill': 11980,
    'Jaquez': 11981,
    'Smith': 11982,
    'Campbell': 11983,
    'Singleton': 11984
}

SERVER_PORTS_LOCAL = {
    'Hill': 8000,
    'Jaquez': 8001,
    'Smith': 8002,
    'Campbell': 8003,
    'Singleton': 8004
}

NEIGHBORS = {
    'Hill': ['Jaquez', 'Smith'],
    'Jaquez': ['Hill', 'Singleton'],
    'Smith': ['Hill', 'Campbell', 'Singleton'],
    'Campbell': ['Smith', 'Singleton'],
    'Singleton': ['Jaquez', 'Smith', 'Campbell']
}
