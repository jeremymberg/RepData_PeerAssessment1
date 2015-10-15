get_mean_number_of_steps <- function(missing_interval, Average_df = Activity_averages_df) {
        index_mean_number_of_steps <- match(missing_interval, Average_df$interval)
        print(index_mean_number_of_steps)
        print(head(Average_df$Average))
        mean_number_of_steps <- Average_df$Average[index_mean_number_of_steps]
}
