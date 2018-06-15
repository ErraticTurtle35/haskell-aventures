missing_products_with_lots = [{
    'lot_id': 49,
    'product_id': 117,
    'split_qty': 10.0
}, {
    'lot_id': 50,
    'product_id': 117,
    'split_qty': 2.0
}, {
    'lot_id': 102,
    'product_id': 53,
    'split_qty': 1.0
}, {
    'lot_id': 103,
    'product_id': 53,
    'split_qty': 2.0
}, {
    'lot_id': 105,
    'product_id': 53,
    'split_qty': 1.0
}]

reservation_details = {
    17: [{
        'product_id': 17,
        'product_uom_id': 23,
        'qty_done': 10.0,
        'reserved_lot_id': False,
        'expiration_date': False
    }],
    53: [{
        'product_id': 53,
        'product_uom_id': 23,
        'qty_done': 2.0,
        'reserved_lot_id': 101,
        'expiration_date': False
    }, {
        'product_id': 53,
        'product_uom_id': 23,
        'qty_done': 2.0,
        'reserved_lot_id': 102,
        'expiration_date': '2018-06-12 20:37:20'
    }, {
        'product_id': 53,
        'product_uom_id': 23,
        'qty_done': 2.0,
        'reserved_lot_id': 103,
        'expiration_date': '2018-06-12 20:37:32'
    }, {
        'product_id': 53,
        'product_uom_id': 23,
        'qty_done': 2.0,
        'reserved_lot_id': 104,
        'expiration_date': '2018-06-12 20:37:41'
    }, {
        'product_id': 53,
        'product_uom_id': 23,
        'qty_done': 1.0,
        'reserved_lot_id': 105,
        'expiration_date': '2018-06-12 20:37:55'
    }],
    47: [{
        'product_id': 47,
        'product_uom_id': 23,
        'qty_done': 10.0,
        'reserved_lot_id': False,
        'expiration_date': False
    }],
    44: [{
        'product_id': 44,
        'product_uom_id': 23,
        'qty_done': 1.0,
        'reserved_lot_id': 36,
        'expiration_date': False
    }, {
        'product_id': 44,
        'product_uom_id': 23,
        'qty_done': 1.0,
        'reserved_lot_id': 37,
        'expiration_date': False
    }, {
        'product_id': 44,
        'product_uom_id': 23,
        'qty_done': 1.0,
        'reserved_lot_id': 38,
        'expiration_date': False
    }, {
        'product_id': 44,
        'product_uom_id': 23,
        'qty_done': 1.0,
        'reserved_lot_id': 39,
        'expiration_date': False
    }, {
        'product_id': 44,
        'product_uom_id': 23,
        'qty_done': 1.0,
        'reserved_lot_id': 40,
        'expiration_date': False
    }],
    46: [{
        'product_id': 46,
        'product_uom_id': 23,
        'qty_done': 1.0,
        'reserved_lot_id': 81,
        'expiration_date': False
    }, {
        'product_id': 46,
        'product_uom_id': 23,
        'qty_done': 1.0,
        'reserved_lot_id': 82,
        'expiration_date': False
    }, {
        'product_id': 46,
        'product_uom_id': 23,
        'qty_done': 1.0,
        'reserved_lot_id': 83,
        'expiration_date': False
    }, {
        'product_id': 46,
        'product_uom_id': 23,
        'qty_done': 1.0,
        'reserved_lot_id': 84,
        'expiration_date': False
    }, {
        'product_id': 46,
        'product_uom_id': 23,
        'qty_done': 1.0,
        'reserved_lot_id': 85,
        'expiration_date': False
    }],
    117: [{
        'product_id': 117,
        'product_uom_id': 23,
        'qty_done': 10.0,
        'reserved_lot_id': 48,
        'expiration_date': '2018-06-11 21:46:15'
    }, {
        'product_id': 117,
        'product_uom_id': 23,
        'qty_done': 10.0,
        'reserved_lot_id': 49,
        'expiration_date': '2018-06-11 21:46:19'
    }, {
        'product_id': 117,
        'product_uom_id': 23,
        'qty_done': 5.0,
        'reserved_lot_id': 50,
        'expiration_date': '2018-06-11 21:46:27'
    }]
}

reservation_details_product_with_lots = reservation_details[53] + reservation_details[117]
missing_lot_id_for_products_with_lots = list(map(lambda p: p['lot_id'], missing_products_with_lots))

def get_picked(missing_lots, product_details):
    picked = []
    for p in product_details:
        if p['reserved_lot_id'] not in missing_lots:
            picked.append(p)
    return picked

def get_partial_picked(missing_lots, product_details):
    partial_picked = []
    for p in product_details:
        if p['reserved_lot_id'] in missing_lots:
            partial_picked.append(p)
    return partial_picked


all_product_with_lot_picked = get_picked(missing_lot_id_for_products_with_lots, reservation_details_product_with_lots)

partial_picked_product_details = get_partial_picked(missing_lot_id_for_products_with_lots, reservation_details_product_with_lots)





shipment_quantity_before = [10.0, 9.0, 10.0, 5.0, 5.0, 25.0]
shipment_quantity_after  = [10.0, 5.0, 10.0, 4.0, 5.0, 13.000000000000002]


current_picking_ordered_qty_before = [10.0, 9.0, 10.0, 5.0, 5.0, 25.0]
current_picking_ordered_qty_after  = [10.0, 5.0, 10.0, 4.0, 5.0, 13.0]


current_move_lines_qty_before = [10.0, 2.0, 2.0, 2.0, 2.0, 1.0, 10.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 10.0, 10.0, 5.0] | 20
current_move_lines_qty_after  = [10.0, 10.0, 2.0, 1.0, 2.0, 10.0, 3.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0]  | 16



shipment_quantity_before = [10.0, 5.0, 10.0, 4.0, 5.0, 13.0]
shipment_quantity_after =  [5.0, 5.0, 0.0, 3.0000000000000004, 5.0, 13.0]

current_picking_ordered_qty_before = [10.0, 5.0, 10.0, 4.0, 5.0, 13.0]
current_picking_ordered_qty_after  = [5.0, 5.0, 0.0, 3.0, 5.0, 13.0]


current_move_lines_qty_before = [10.0, 2.0, 1.0, 2.0, 10.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 10.0, 3.0]
current_move_lines_qty_after  = [5.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 2.0, 1.0, 2.0, 10.0, 3.0]