-module(node_data).
-compile(nowarn_export_all).
-compile(export_all).


center_node(center) ->
    [];
center_node(dev) ->
    [];
center_node(main) ->
    center;
center_node(stable) ->
    center;
center_node(test) ->
    center;
center_node(world) ->
    [];
center_node(_) -> 
    [].

center_ip(center) ->
    [];
center_ip(dev) ->
    [];
center_ip(main) ->
    [];
center_ip(stable) ->
    [];
center_ip(test) ->
    [];
center_ip(world) ->
    [];
center_ip(_) -> 
    [].

server_node(center) ->
    [center];
server_node(local) ->
    [dev, main, stable, test];
server_node(world) ->
    [world];
server_node(_) -> 
    [].

server_ip(center) ->
    [[]];
server_ip(local) ->
    [[], [], [], []];
server_ip(world) ->
    [[]];
server_ip(_) -> 
    [].

