-- Load the dp_api_demo2 table specifying OIDs.


load data
infile *

into table dp_api_demo2
replace
oid (name_oid)
(
name_oid FILLER position(1:32),
warehouse_id    position(33:35),
warehouse_name  position(36:70),
location_id     position(71:74)
)


BEGINDATA
78686CC3AEE15A4DE034080020A3EC15123plainville warehouse parts_one     1234
78686CC3AEE25A4DE034080020A3EC15222plainville warehouse parts_two     1212
78686CC3AEE35A4DE034080020A3EC15333plainville warehouse pieces_one    2323
