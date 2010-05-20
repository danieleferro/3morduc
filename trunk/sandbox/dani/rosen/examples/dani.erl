%%%%%%%%%%%%%%%%%%%%%%
%% ADDED BY DANIELE %%
%%%%%%%%%%%%%%%%%%%%%%

-module(dani).
-export([go/0]).

-include ("geometry.hrl").
-include ("robot.hrl").

-define(ROBOT_SIZE, ?CM(30)).


go () ->
    WPid = create_world(),
    create_robot(WPid),
    sensors().

create_world () ->
    rosen:start_link(),
    %%  rosen:start_link(?ROSEN_OPTIONS),
    rosen:zoom (-?CM(300)),
    rosen:up (-?CM(50)),


    %% the playing area
    {ok, WPid} = rosen_world:start_link (#world {width = ?CM(300),
						 height = ?CM(210),
						 color = ?RGB(0.25,0.25,0.25)}),

    %% blue starting area
    rosen_world:paint_floor (WPid,
			     ?CM(125), -?CM(80),
			     ?CM(50), ?CM(50),
			     ?RGB(0.0, 0.0, 1.0)),

    %% red starting area
    rosen_world:paint_floor (WPid,
			     -?CM(125), -?CM(80),
			     ?CM(50), ?CM(50),
			     ?RGB(1.0, 0.0, 0.0)),

    %% -----------------------------------------------------------------

    rosen_world:wall (WPid,
		      -?CM(75), ?CM(105),
		      ?CM(150), ?CM(7.5), ?CM(1.0),
		      0.0,
		      ?RGB(0.0, 0.0, 1.0)),

    rosen_world:wall (WPid,
		      -?CM(75), -?CM(105),
		      ?CM(150), ?CM(7.5), ?CM(1.0),
		      0.0,
		      ?RGB(0.0, 0.0, 1.0)),

    rosen_world:wall (WPid,
		      -?CM(150.5), 0.0,
		      ?CM(210.0), ?CM(7.5), ?CM(1.0),
		      90.0,
		      ?RGB(0.0, 0.0, 1.0)),


    %% -----------------------------------------------------------------

    rosen_world:wall (WPid,
		      ?CM(75), ?CM(105),
		      ?CM(150), ?CM(7.5), ?CM(1.0),
		      0.0,
		      ?RGB(1.0, 0.0, 0.0)),

    rosen_world:wall (WPid,
		      ?CM(75), -?CM(105),
		      ?CM(150), ?CM(7.5), ?CM(1.0),
		      0.0,
		      ?RGB(1.0, 0.0, 0.0)),

    rosen_world:wall (WPid,
		      ?CM(150.5), 0.0,
		      ?CM(210.0), ?CM(7.5), ?CM(1.0),
		      90.0,
		      ?RGB(1.0, 0.0, 0.0)),
    WPid.

create_robot(WPid) ->   
    
    

    Base = #object3d {  type = cylinder,
			radius = ?ROBOT_SIZE/2,
			axis = ?VECTOR (0.0, 1.0, 0.0),
			size = ?ROBOT_SIZE,
			color = ?RGBA(0, 1.0, 0, 0.75),
			position = ?VECTOR (0, 0.1, ?CM(5)) },
    

    Sphere = #object3d { type = sphere,
			 position = ?VECTOR (0.0, 0.0, ?ROBOT_SIZE - ?CM(16)),
			 radius = ?CM(4)},

    Connector = #object3d { type = compound,
			    name = connector,
			    position = ?VECTOR (0.0,
						?CM(15),
						?ROBOT_SIZE - ?CM(4))},
    

    {ok, RobotPid} =
	robot:start_link (#robot { world = WPid,
				   type = two_wheels_driving,
				   name = robot1,
				   wheel_radius = ?CM(4),
				   wheel_distance = ?ROBOT_SIZE,
				   wheel_ticks = 250,
				   structure = [Sphere, Connector,  Base]} ),

    %%% ADDED BY ME
    rosen:set_main_robot(robot1),
    

    robot:command(robot1, {set_position, 0.0, 0, 0}),
    robot:command(robot1, {set_speed, 0.0, 0.0}),
    ok.


sensors () ->
  robot:add_sensor (robot1,
                    #sensor { type = contact_point,
                              sensor_name = front_contact,
                              position = ?VECTOR (?ROBOT_SIZE - ?CM(4), 0, ?CM(0)),
                              color = ?RGB (0.0, 0.0, 0.0)}),

  robot:add_sensor (
    robot1,
    distance_left,
    #sensor { type = gp2d12,
              sensor_name = distance_left,
              position = ?VECTOR (?ROBOT_SIZE - ?CM(4), -?ROBOT_SIZE / 2 , ?CM(0)),
              parameters = 0.0,
              color = ?RGB (1.0, 0.0, 0.0)}),

  robot:add_sensor (
    robot1,
    distance_right,
    #sensor { type = gp2d12,
              sensor_name = distance_right,
              position = ?VECTOR (?ROBOT_SIZE - ?CM(4), ?ROBOT_SIZE / 2 , ?CM(0)),
              parameters = 0.0,
              color = ?RGB (0.0, 1.0, 0.0)}),

%%   Pid = robot:get_motion_pid (robot1),

%%   gen_activity:set_property (Pid, v, 5.0),
%%   gen_activity:set_property (Pid, omega, 189.076),

%%   {ok, ActivityPid} =
%%     object3d:add_activity ( robot1_object, robot_path,
%%                             [{robot, RobotPid},
%%                              {rotation_tolerance, 2},
%%                              {rotation_speed, ?DEG_S(30)},
%%                              {motion_tolerance, ?CM(5)},
%%                              {motion_speed, ?CM_S(40)},
%%                              {path, [{forward, ?CM(80)},
%%                                      {rotate, 90}]},
%%                              {restart, false}]),

  %% object3d:add_activity ( robot1_object, obstacle_avoid, [{robot, robot1}]),

 %%  test_path (ActivityPid),

  ok.


test_path (Pid) ->
  %%timer:sleep (1000),
  case gen_activity:get_property (Pid, path_done) of
    true ->
      io:format ("Done~n"),
      test_path (Pid);
    _ ->
      test_path (Pid)
  end.
