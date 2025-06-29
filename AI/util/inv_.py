
def parse_inventory(inv_str):

    inv_str = inv_str.strip("[]\n ")
    items = inv_str.split(',')
    inventory = {}
    for item in items:
        if item:
            name, value = item.strip().split()
            inventory[name] = int(value)
    return inventory
