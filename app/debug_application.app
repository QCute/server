{
    application, record_application,
    [
        {description, "This is debug application."},
        {vsn, "1.0.0"},
        {mod, {debug_application, []}},
        {modules, [debug_application]},
        {registered, [debug_application]},
        {applications, [kernel, stdlib, sasl]},
        {env, []}
    ]
}.