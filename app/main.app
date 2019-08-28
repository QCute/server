{
    application, main,
    [
        {description, "This is main application server."},
        {vsn, "1.0.0"},
        {modules, [main]},
        {registered, [main]},
        {applications, [kernel, stdlib, sasl]},
        {mod, {main, []}},
        {start_phases, []},
        {env, []}
    ]
}.
