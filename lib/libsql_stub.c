// libsql_stub.c
#include "libsql.h"

// This function ensures all symbols from the static library are linked
// It won't be called at runtime, but prevents the linker from stripping symbols
void libsql_ensure_linking(void) {
    // Use volatile void* to prevent compiler optimization
    volatile void* dummy;
    
    // Core library functions
    dummy = (void*)libsql_enable_internal_tracing;
    
    // Sync functions
    dummy = (void*)libsql_sync;
    dummy = (void*)libsql_sync2;
    
    // Database opening functions
    dummy = (void*)libsql_open_sync;
    dummy = (void*)libsql_open_sync_with_webpki;
    dummy = (void*)libsql_open_sync_with_config;
    dummy = (void*)libsql_open_ext;
    dummy = (void*)libsql_open_file;
    dummy = (void*)libsql_open_remote;
    dummy = (void*)libsql_open_remote_with_webpki;
    
    // Database management
    dummy = (void*)libsql_close;
    
    // Connection functions
    dummy = (void*)libsql_connect;
    dummy = (void*)libsql_load_extension;
    dummy = (void*)libsql_reset;
    dummy = (void*)libsql_disconnect;
    
    // Statement preparation
    dummy = (void*)libsql_prepare;
    
    // Parameter binding functions
    dummy = (void*)libsql_bind_int;
    dummy = (void*)libsql_bind_float;
    dummy = (void*)libsql_bind_null;
    dummy = (void*)libsql_bind_string;
    dummy = (void*)libsql_bind_blob;
    
    // Statement execution
    dummy = (void*)libsql_query_stmt;
    dummy = (void*)libsql_execute_stmt;
    dummy = (void*)libsql_reset_stmt;
    dummy = (void*)libsql_free_stmt;
    
    // Direct query/execute functions
    dummy = (void*)libsql_query;
    dummy = (void*)libsql_execute;
    
    // Results management
    dummy = (void*)libsql_free_rows;
    dummy = (void*)libsql_free_rows_future;
    dummy = (void*)libsql_wait_result;
    
    // Column information
    dummy = (void*)libsql_column_count;
    dummy = (void*)libsql_column_name;
    dummy = (void*)libsql_column_type;
    
    // Connection state functions
    dummy = (void*)libsql_changes;
    dummy = (void*)libsql_last_insert_rowid;
    
    // Row iteration
    dummy = (void*)libsql_next_row;
    dummy = (void*)libsql_free_row;
    
    // Value extraction functions
    dummy = (void*)libsql_get_string;
    dummy = (void*)libsql_free_string;
    dummy = (void*)libsql_get_int;
    dummy = (void*)libsql_get_float;
    dummy = (void*)libsql_get_blob;
    dummy = (void*)libsql_free_blob;
    
    // Prevent "unused variable" warnings
    (void)dummy;
}