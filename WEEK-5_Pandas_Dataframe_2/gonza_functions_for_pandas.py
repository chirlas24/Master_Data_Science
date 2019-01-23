def remove_columns_with_few_data(df, low_limit_data_by_column):
    "Remove columns of dataframe with a low limit data number for columns"
    
    print('Number of columns before removing: %i' % df.count().size)
    columns_after_removing = df.count()[df.count() > low_limit_data_by_column].size
    print('Number of columns after removing: %i' % columns_after_removing)
    columns_removed = df.count().size - columns_after_removing
    print('Number of columns removed: %i' % columns_removed)

    df_modified = df[df.columns[df.count() > low_limit_data_by_column]]

    return df_modified
