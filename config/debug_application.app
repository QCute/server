{
    application, record_application,
    [
        {description, "This is record application server."},
        {vsn, "1.0.0"},
        {modules, [debug_application]},
        {registered, [debug_server]},
        {applications, [kernel, stdlib, sasl]},
        {mod, {debug_application, []}},
        {start_phases, []}
    ]
}.