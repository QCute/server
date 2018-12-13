-module(data_param).
-compile(nowarn_export_all).
-compile(export_all).


get(activity_recharge_mail_content) ->
    <<"这是您在充值活动中未领取的奖励">>;
get(activity_recharge_mail_title) ->
    <<"充值活动奖励发放">>;
get(beauty_largess) ->
    [{13010006,1,0},{13010007,2,0},{13010002,0,1},{13010004,0,2}];
get(beauty_tease) ->
    [{13010001,1},{13010003,2}];
get(chat_expire_days) ->
    30;
get(chat_num_limit) ->
    200;
get(chat_world_timespan) ->
    3;
get(chuyou_add_seat_cost) ->
    [{1, 300}, {2, 600}, {3, 1000}, {4, 2000}, {5, 3000}, {6, 4000}, {7, 5000}, {8, 5000}, {9, 5000}];
get(chuyou_duration) ->
    10800;
get(chuyou_max_batai_seat) ->
    4;
get(chuyou_max_jiaban_seat) ->
    6;
get(crusade_auto_limit) ->
    50;
get(crusade_guanka_limit) ->
    10801;
get(feast_attend_limit) ->
    3;
get(feast_exchange_limit) ->
    9;
get(feast_exchange_refresh_limit) ->
    10;
get(feast_exchange_refresh_time) ->
    7200;
get(feast_message_limit) ->
    50;
get(feast_rank_limit) ->
    300;
get(first_recharge_reward) ->
    [100043,[{11010002,4},{12010001,10},{12010005,10}]];
get(fuben_boss_first_wave) ->
    701;
get(fuben_boss_open_time) ->
    {20,0};
get(fuben_boss_over_time) ->
    {21,0};
get(fuben_boss_stage_list) ->
    [{8000,[{3333,13055001,1},{3333,13055002,1},{3334,13055003,1}]}, {6000,[{3333,13055001,1},{3333,13055002,1},{3334,13055003,1}]}, {4000,[{3333,13055001,1},{3333,13055002,1},{3334,13055003,1}]}, {2000,[{3333,13055001,1},{3333,13055002,1},{3334,13055003,1}]}];
get(fuben_clear_time) ->
    {23,0};
get(fuben_drop_list_limit) ->
    50;
get(fuben_first_wave) ->
    101;
get(fuben_hurt_rank_award) ->
    [{1,1,150,150,60},{2,2,120,120,50},{3,3,100,100,40},{4,5,80,80,30},{6,10,70,70,25},{11,20,60,60,20},{21,50,50,50,15},{51,100,40,40,10}];
get(fuben_hurt_rank_limit) ->
    100;
get(fuben_kill_rank_limit) ->
    100;
get(fuben_open_time) ->
    {12,0};
get(fuben_over_time) ->
    {14,0};
get(guild_create_gold) ->
    2000;
get(guild_elite_merit_limit) ->
    1000;
get(guild_join_timespan) ->
    86400;
get(guild_name_gold) ->
    500;
get(guild_rank_timespan) ->
    3600;
get(guild_vice_merit_limit) ->
    2000;
get(heir_energy_cd_time) ->
    10800;
get(heir_loc) ->
    2;
get(heir_loc_gold) ->
    [{3,300},{4,600},{5,1000},{6,2000},{7,3000},{8,4000},{9,5000},{10,5000}];
get(heir_loc_limit) ->
    10;
get(heir_num_limit) ->
    9999;
get(heir_pet_id_ratio) ->
    [{0, 1}, {10000, 2}];
get(heir_quality_ratio) ->
    [{1,20,70,1},{1,20,30,2},{21,100,50,1},{21,100,30,2},{21,100,20,3},{101,200,30,1},{101,200,30,2},{101,200,20,3},{101,200,20,4},{201,500,40,2},{201,500,30,3},{201,500,20,4},{201,500,10,5},{501,9999999999,30,2},{501,9999999999,30,3},{501,9999999999,20,4},{501,9999999999,20,5}];
get(heir_rand_icon_list) ->
    [{1, [2001,2002,2003,2004]},{2, [2101,2102,2103,2104]}];
get(heir_ratio) ->
    [{1,10},{2,80}];
get(heir_train_exp) ->
    10;
get(lifetime_card_award) ->
    [0, []];
get(lifetime_card_miracle_count) ->
    0;
get(luxury_one) ->
    56;
get(luxury_ten) ->
    560;
get(mail_attachment_limit) ->
    10;
get(mail_expire_days) ->
    30;
get(marriage_tiqin_expire_timespan) ->
    259200;
get(marriage_zhaoqin_num_limit) ->
    5;
get(marriage_zhaoqin_refresh_gold) ->
    100;
get(marriage_zhaoqin_refresh_timespan) ->
    3600;
get(menke_car_quality_limit) ->
    [{6,4},{5,4},{4,4},{3,10},{2,10},{1,10}];
get(menke_expatriate_add_seat_cost) ->
    [{1, 300}, {2, 600}, {3, 1000}, {4, 2000}, {5, 3000}, {6, 4000}, {7, 5000}, {8, 5000}, {9, 5000}];
get(menke_expatriate_max_seat) ->
    10;
get(menke_expatriate_menke_limit) ->
    30;
get(menke_expatriate_valid_day) ->
    100;
get(month_card_award) ->
    [0, []];
get(month_card_miracle_count) ->
    0;
get(penalty_box_award) ->
    11010007;
get(penalty_message_num) ->
    200;
get(penalty_rank_cd_time) ->
    3600;
get(penalty_rank_num) ->
    10;
get(prison_prisoner_limit) ->
    10;
get(prison_punish_take_hp) ->
    1;
get(rank_mobai_award) ->
    [{60,10},{30,20},{10,50}];
get(sign_award_cycle) ->
    21;
get(study_add_seat) ->
    [{1,300},{2,600},{3,1000},{4,2000},{5,3000},{6,4000},{7,5000},{8,5000},{9,5000}];
get(study_max_seat) ->
    10;
get(trade_auto_limit) ->
    50;
get(trade_job_limit) ->
    10;
get(visit_limit) ->
    {3,2};
get(visit_recover_cd) ->
    {3600,900};
get(visit_recover_num) ->
    {1,1};
get(yamen_attribute_first) ->
    [63,64,65];
get(yamen_battle_limit) ->
    4;
get(yamen_defend_message_limit) ->
    30;
get(yamen_foe_kill_limit) ->
    5;
get(yamen_foe_list_limit) ->
    30;
get(yamen_global_kill_limit) ->
    20;
get(yamen_global_message_limit) ->
    200;
get(yamen_menke_lv_limit) ->
    60;
get(year_card_award) ->
    100;
get(yuehui_accept_limit) ->
    3;
get(yuehui_act_cd) ->
    10800;
get(yuehui_act_cd_gold) ->
    10;
get(yuehui_auto_limit) ->
    3;
get(yuehui_job_limit) ->
    2;
get(yuehui_num_ratio_list) ->
    [{2,500},{3,200}];
get(yuehui_timespan) ->
    900;
get(zhengwu_cd_time) ->
    1800;
get(zhengwu_finish_award) ->
    [{5000,12025017,1},{5000,13016001,1}];
get(_) -> 
    [].

