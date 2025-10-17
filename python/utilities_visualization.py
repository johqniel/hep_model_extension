def calc_rows_cols(num_plots):
    num_rows = 0
    num_cols = 0
    available_spots = 0
    i = 0 

    while i < num_plots:
        if i < available_spots: 
            i += 1
            continue
        else:
            if num_rows + num_cols % 2 == 0:
                num_cols += 1
                available_spots += num_cols + 1
            else: 
                num_rows += 1
                available_spots += num_rows + 2


    return num_cols, num_rows#