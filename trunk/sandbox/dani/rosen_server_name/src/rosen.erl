%%
%% rosen.erl
%%
%% ----------------------------------------------------------------------
%%
%%  ROSEN, a RObotic Simulation Erlang eNgine
%%  Copyright (C) 2007 Corrado Santoro (csanto@diit.unict.it)
%%
%%  This program is free software: you can redistribute it and/or modify
%%  it under the terms of the GNU General Public License as published by
%%  the Free Software Foundation, either version 3 of the License, or
%%  (at your option) any later version.
%%
%%  This program is distributed in the hope that it will be useful,
%%  but WITHOUT ANY WARRANTY; without even the implied warranty of
%%  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%  GNU General Public License for more details.
%%
%%  You should have received a copy of the GNU General Public License
%%  along with this program.  If not, see <http://www.gnu.org/licenses/>
%%
%% $Id: rosen.erl,v 1.16 2008/10/21 21:36:35 aristidena Exp $
%%
%%
%% @doc The module implementing the 3D engine.
%%
%% <p>This module represents the 3D engine. It handles the collection
%% of the objects of the world, providing the functions to add new objects
%% and to change the view of the world. It also has the responsibility
%% of triggering redrawing and activity execution.</p>
%%

-module (rosen).
-behaviour(gen_fsm).

-include("sdl.hrl").
-include("sdl_events.hrl").
-include("sdl_video.hrl").
-include("sdl_keyboard.hrl").
-include("gl.hrl").
-include("geometry.hrl").

-define (DEFAULT_PERIOD, 10).
-define (DEFAULT_ROTATION_INCREMENT, 5).
-define (DEFAULT_ZOOM_INCREMENT, 5).
-define (DEFAULT_ROBOT_STEP, 5).
-define (DEFAULT_ROBOT_ROTATION, 5).
-define (SIMULATED_DELTATIME, 0.1).


-record (engine_state, {object_list = [],
                        world_list = [],
                        period = ?DEFAULT_PERIOD,
                        screen_h,
                        screen_w,
                        rotation = ?VECTOR (0.0, 0.0, 0.0),
                        rotation_increment = ?VECTOR (0.0, 0.0, 0.0),
                        zoom = 0.0,
                        zoom_increment = 0.0,
                        collision_options,
			collision_enable = false,
			time_type, 
			time,
			delta,
			last_time,
			%% ADDED BY DANIELE %%
			main_robot = undefined,
			r_motion = stop,
			r_rotation = 0
		       }).


-export ([start_link/1,
          start_link/2,
          zoom/2,
          up/2,
          left/2,
          add_object/2,
          color/1,
          stop/1,
          add_world/2,
	  set_collision/2]).
-export ([init/1, idle_draw/2, terminate/3, handle_event/3]).

%%% my function %%%
-export([set_main_robot/2]).
set_main_robot(Name, Pid) ->
    gen_fsm:send_all_state_event(Name, {set_main_robot, Pid}).



%%====================================================================
%% Func: start_link/0
%%====================================================================
%% @spec start_link() -> {ok, Pid}
%%
%% @doc Starts the engine.
%%
start_link (Name) ->
    start_link (Name, []).


%%====================================================================
%% Func: start_link/1
%%====================================================================
%% @spec start_link(Config) -> ok
%%       Config = [term()]
%%
%% @doc Starts the engine with the options specified.
%%
%% <p>Turns on full screen graphics. Example:</p>
%% <pre>
%%  rosen:start_link ([fullscreen]).
%% </pre>
%%
%% <p>Turns on collision, with the specified modules for the layers of
%% collision handling.
%% Example:</p>
%% <pre>
%%  LayerModules = [ object_not_in_a_compound_list,
%%                   to_object_dict,
%%                   add_mesh,
%%                   add_aabb,
%%                   to_object_couples,
%%                   broad_phase_aabbs_intersect,
%%                   narrow_phase_trimeshes_intersect,
%%                   from_collision_dict_to_object_list,
%%                   response_set_previous ]
%%  rosen:start_link ([{ collision, LayerModules }]).
%% </pre>
%%
%% <p>For details in writing a module for a layer of collision handling,
%% see the module <code>collision</code>.</p>
%%
start_link (Name, Config) ->
    gen_fsm:start_link ({local, Name}, ?MODULE, Config, []).


%%====================================================================
%% Func: add_object/1
%%====================================================================
%% @spec add_object(ObjectPid) -> ok
%%
%% @doc Adds a new object (represented by its pid) to the engine.
%%
add_object (Name, Object) ->

    io:format("chiamo add_object con: ~p, ~p~n", [Name, Object]),
    gen_fsm:send_event (Name, {add_object, Object}).


%%====================================================================
%% Func: zoom/1
%%====================================================================
%% @spec zoom(Zoom::number()) -> ok
%%
%% @doc Changes the zoom of the camera.
%%
zoom (Name, Zoom) ->
    gen_fsm:send_event (Name, {zoom, Zoom}).


%%====================================================================
%% Func: up/1
%%====================================================================
%% @spec up(PanUp::number()) -> ok
%%
%% @doc Changes the height of the camera.
%%
up (Name, X) ->
    gen_fsm:send_event (Name, {up_down, X}).


%%====================================================================
%% Func: left/1
%%====================================================================
%% @spec left(PanLeft::number()) -> ok
%%
%% @doc Changes the horizontal position of the camera.
%%
left (Name, X) ->
    gen_fsm:send_event (Name, {left_right, X}).



%%====================================================================
%% Func: color/1
%%====================================================================
color (X) when size (X) == 3 ->
    gl:color3fv (X);
color (X) when size (X) == 4 ->
    gl:color4fv (X).


%%====================================================================
%% Func: stop/0
%%====================================================================
%%
%% @spec stop() -> ok
%%
%% @doc Destroys the engine.
%%
stop (Name) ->
    gen_fsm:send_all_state_event (Name, stop).


%%====================================================================
%% Func: add_world/1
%%====================================================================
%% @spec add_world(WorldPid) -> ok
%%
%% @doc Adds a new world (represented by its pid) to the engine.
%%
add_world (Name, WorldPid) ->
    gen_fsm:send_all_state_event (Name, {add_world, WorldPid}).


%%====================================================================
%% Func: set_collision/1
%%====================================================================
%% @spec set_collision(Value) -> ok
%%
%% @doc Enable/Disable Collision Handler.
%%
set_collision (Name, Value) ->
    gen_fsm:send_all_state_event (Name, {set_collision, Value}).


%%====================================================================
%% Callback functions
%%====================================================================
%%====================================================================
%% Func: idle_draw/2
%%====================================================================
%% @private
%%
idle_draw ({zoom, ZoomValue}, StateData) ->

    %% set zoom value
    %% state name is always "idle_draw"
    NewStateData =
	StateData#engine_state {zoom = ZoomValue },
    {next_state, idle_draw, NewStateData, NewStateData#engine_state.period};

%%
idle_draw ({up_down, NewX}, StateData) ->

    %% set only X value in rotation vector
    %% state name is always "idle_draw"

    Y = (StateData#engine_state.rotation)#vector.y,
    Z = (StateData#engine_state.rotation)#vector.z,
    NewStateData =
	StateData#engine_state {rotation = ?VECTOR (NewX, Y, Z) },

    gl:translatef (0.0, NewX, 0.0),

    {next_state, idle_draw, NewStateData, NewStateData#engine_state.period};



%%
idle_draw ({left_right, NewY}, StateData) ->

    %% set only Y value in rotation vector
    %% state name is always "idle_draw"

    X = (StateData#engine_state.rotation)#vector.x,
    Z = (StateData#engine_state.rotation)#vector.z,
    NewStateData =
	StateData#engine_state {rotation = ?VECTOR (X, NewY, Z) },
    {next_state, idle_draw, NewStateData, NewStateData#engine_state.period};

%%
idle_draw ({add_object, Pid}, StateData) ->

    %% add new object to object_list
    %% state name is always "idle_draw"

    {next_state, idle_draw,
     StateData#engine_state {object_list = [Pid | StateData#engine_state.object_list]},
     StateData#engine_state.period};

%% Funzione da richimare allo scadere del timeout,
%% se lo stato è idle_draw.
%%
%% 1. transla l'oggetto in modo da far passare l'asse di rotazione dall'origine
%% 2. ruota l'oggetto in modo che l'asse di rotazione coincida con uno degli assi coordinati
%% 3. ruotare a piacere
%% 4. applica la rotazioneinversa alla 2
%% 5. transla in modo inverso alla 1.
idle_draw (timeout, StateData) ->
    %%gl:rotatef (0.5, 0.0, 1.0, 0.0),
    gl:clear(?GL_COLOR_BUFFER_BIT bor ?GL_DEPTH_BUFFER_BIT),
    gl:loadIdentity (),

    gl:translatef (0.0, 0.0, StateData#engine_state.zoom),

    R = StateData#engine_state.rotation,
    IncR = StateData#engine_state.rotation_increment,


    %% OLD CODE
   
%%     gl:rotatef (R#vector.x, 1.0, 0.0, 0.0),
%%     gl:rotatef (R#vector.y, 0.0, 1.0, 0.0),
%%     gl:rotatef (R#vector.z, 0.0, 0.0, 1.0),


%%     %% NEW CODE

    gl:translatef (R#vector.y, 0.0, 0.0),
    gl:translatef (0.0, R#vector.x, 0.0),
    gl:translatef (0.0, 0.0, R#vector.z),
    
    
%%     %% END NEW CODE

    NewR = R#vector { x = R#vector.x + IncR#vector.x ,
		      y = R#vector.y + IncR#vector.y ,
		      z = R#vector.z + IncR#vector.z},

    %% CODE ADDED BY DANIELE %%
    

    R_motion = StateData#engine_state.r_motion,
    
    R_rotation = StateData#engine_state.r_rotation,
    

    case StateData#engine_state.main_robot of
	
	undefined ->
	    io:format("undefined case for main_robot~n", []);

	RobotId ->
	    %% call motion on robot
	    %%io:format("set robot position~n", []),

	    { X_old, Y_old, Angle_old } = robot:command(RobotId, {get_position}),


	    if
		( R_rotation /= ( round(Angle_old) rem 180 ) ) and ( R_motion == stop ) ->

		    io:format("~w~n", [ robot:command(RobotId, {get_position}) ]),  

		    %% rotate the robot
		    robot:command(RobotId, {set_position,
					    X_old, Y_old, R_rotation}),

		    io:format("Robot rotation Xpos: ~p  Ypos: ~p  Angle: ~p~n", [X_old, Y_old, R_rotation]);
		    

		 R_motion == forward ->
		    %% move the robot

		    io:format("~w~n", [ robot:command(RobotId, {get_position}) ]),  


		    X_new = ?DEFAULT_ROBOT_STEP * math:cos(R_rotation * math:pi() / 180),
		    Y_new = ?DEFAULT_ROBOT_STEP * math:sin(R_rotation * math:pi() / 180),

		    robot:command(RobotId, {set_position,
					    X_new + X_old,
					    Y_new + Y_old,
					    R_rotation}),
		    
		    io:format("Robot forward motion~nXpos: ~p  Ypos: ~p  Angle: ~p~n", [X_new + X_old, Y_new + Y_old, R_rotation]);

		 R_motion == reward ->
		    %% move the robot

		    io:format("~w~n", [ robot:command(RobotId, {get_position}) ]),  


		    X_new = ?DEFAULT_ROBOT_STEP * math:cos(R_rotation * math:pi() / 180),
		    Y_new = ?DEFAULT_ROBOT_STEP * math:sin(R_rotation * math:pi() / 180),

		    robot:command(RobotId, {set_position,
					    X_old - X_new,
					    Y_old - Y_new,
					    R_rotation}),
		    
		    io:format("Robot forward motion~nXpos: ~p  Ypos: ~p  Angle: ~p~n", [X_old - X_new, Y_old - Y_new, R_rotation]);
		

		true ->
		    ok

	    end
		    
	    	    
    end,


    NewStateData =
	StateData#engine_state
	  { rotation = NewR,
	    zoom = StateData#engine_state.zoom + StateData#engine_state.zoom_increment,
	    r_motion = stop
	   },

    %% END

    %% if collision is enabled, handle collisions
    ok = if
	     %% NewStateData#engine_state.collision_options =/= undefined ->
	     NewStateData#engine_state.collision_enable == true ->
		 collision:handle (
		   NewStateData#engine_state.collision_options,
		   NewStateData#engine_state.object_list);
	     true -> ok
	 end,

    %%   io:format ("--------\n"),


    %% Update Time Parameters
    NewStateTime = time_handler(NewStateData),
    Time = NewStateTime#engine_state.time,
    Delta =  NewStateTime#engine_state.delta,


    lists:foreach (fun (Pid) ->
			   O = object3d:obj (Pid),
			   gl:pushMatrix (),
			   draw_object (Pid, O,O#object3d.visible),
			   gl:popMatrix (),

			   %% before executing activities, set previous
			   %% position, axis and up to current values
			   %% because doing so can be useful for collision
			   %% response
			   object3d:prev_position (Pid, O#object3d.position),
			   object3d:prev_axis (Pid, O#object3d.axis),
			   object3d:prev_up (Pid, O#object3d.up),

			   Acts = object3d:activities (Pid),
			   lists:foreach (fun (Act) ->

						  gen_activity:step (Act,
								     O,
								     {Time, Delta }
								    )

					  end,
					  Acts)
		   end,
		   NewStateTime#engine_state.object_list),



    %%gl:flush(),
    %%   io:format ("Mouse ~p~n", [sdl_keyboard:getKeyState()]),
    gl:swapBuffers (),
    case Evt = sdl_events:pollEvent() of
	#quit{} ->
	    {stop, normal, NewStateTime};
	#resize{} ->
	    %%io:format("Maximized: ~p,~p~n", [sdl_video:wm_isMaximized(), Evt]),
	    W = Evt#resize.w,
	    H = Evt#resize.h,
	    set_viewport(W, H,
			 NewStateTime#engine_state.screen_w,
			 NewStateTime#engine_state.screen_h),
	    gl:matrixMode(?GL_PROJECTION),
	    gl:loadIdentity(),
	    %%gl:ortho( -2.0, 2.0, -2.0, 2.0, -20.0, 20.0),
	    glu:perspective( 50.0, (W * 1.0) / (H * 1.0), 1.0, 1000.0),
	    gl:matrixMode(?GL_MODELVIEW),
	    gl:loadIdentity(),
	    {next_state, idle_draw, NewStateTime, NewStateTime#engine_state.period};
	no_event ->
	    {next_state, idle_draw, NewStateTime, NewStateTime#engine_state.period};
	#keyboard{sym=$f} ->
	    Surface = sdl_video:getVideoSurface(),
	    %%io:format("~p\n", [sdl_video:wm_toggleFullScreen(Surface)]),
	    {next_state, idle_draw, NewStateTime, NewStateTime#engine_state.period};
	#keyboard{sym=?SDLK_q} ->
	    {stop, normal, NewStateTime};
	#keyboard{sym=?SDLK_ESCAPE} ->
	    {stop, normal, NewStateTime};
	#keyboard{sym=?SDLK_l} ->
	    gl:enable (?GL_LIGHTING),
	    {next_state, idle_draw, NewStateTime, NewStateTime#engine_state.period};
	#keyboard{sym=?SDLK_x} ->
	    gl:disable (?GL_LIGHTING),
	    {next_state, idle_draw, NewStateTime, NewStateTime#engine_state.period};
	#keyboard{} = K ->
	    processKey (K, NewStateTime);
	Event ->
	    %%io:format("Got event ~p~n", [Event]),
	    {next_state, idle_draw, NewStateTime, NewStateTime#engine_state.period}
    end.


%%
draw_object(Pid, Obj = #object3d {},false)->
    ok;
%% an object of a compound object
draw_object(Pid, Obj = #object3d {},_)
  when Obj#object3d.parent_object =/= noname ->
    %%   io:format ("NoDraw object ~p, ~p, ~p~n",
    %%              [Pid, Obj#object3d.type,
    %%               Obj#object3d.name]),
    ok;
%%

%% not a compound object
draw_object(Pid, Obj = #object3d { objects = [] },_) ->
    %%   io:format ("Drawing non-compound object ~p, ~p, ~p~n",
    %%              [Pid, Obj#object3d.type,
    %%               Obj#object3d.name]),
    Pos = Obj#object3d.position,
    Axis = Obj#object3d.axis,
    DefaultAxis = Obj#object3d.default_axis,
    Angle = geometry:angle (DefaultAxis, Axis),
    RotAxis = geometry:cross (DefaultAxis, Axis),
    gl:translatef (Pos#vector.x,
		   Pos#vector.y,
		   Pos#vector.z),
    gl:rotatef (Angle,
		RotAxis#vector.x,
		RotAxis#vector.y,
		RotAxis#vector.z),
    if
	Obj#object3d.up =/= undefined ->
	    Up = Obj#object3d.up,
	    DefaultUp = Obj#object3d.default_up,
	    Angle2 = geometry:angle (DefaultUp, Up),
	    RotAxis2 = geometry:cross (DefaultUp, Up),
	    gl:rotatef (Angle2,
			RotAxis2#vector.x,
			RotAxis2#vector.y,
			RotAxis2#vector.z);
	true ->
	    ok
    end,
    object3d:draw (Pid),
    ok;
%%
draw_object(Pid, Obj = #object3d {},_ ) -> %% compound object
    %%   io:format ("Drawing compound object ~p, ~p, ~p~n",
    %%              [Pid, Obj#object3d.type,
    %%               Obj#object3d.name]),
    Pos = Obj#object3d.position,
    Axis = Obj#object3d.axis,
    DefaultAxis = Obj#object3d.default_axis,
    Angle = geometry:angle (DefaultAxis, Axis),
    RotAxis = geometry:cross (DefaultAxis, Axis),
    if
	Obj#object3d.up =/= undefined ->
	    Up = Obj#object3d.up,
	    DefaultUp = Obj#object3d.default_up,
	    Angle2 = geometry:angle (DefaultUp, Up),
	    RotAxis2 = geometry:cross (DefaultUp, Up);
	true ->
	    Angle2 = undefined,
	    RotAxis2 = undefined
    end,
    lists:foreach (fun (P) ->
			   O = object3d:obj (P),
			   ChildDefaultAxis = O#object3d.default_axis,
			   ChildAxis = O#object3d.axis,
			   ChildAngle = geometry:angle (ChildDefaultAxis,
							ChildAxis),
			   ChildRotAxis = geometry:cross (ChildDefaultAxis,
							  ChildAxis),

			   ChildPos = O#object3d.position,

			   if
			       O#object3d.type == compound ->

				   %%                          io:format ("[~p,~p] ~p~n", [Obj#object3d.name,
				   %%                                                      O#object3d.name,
				   %%                                                      Obj#object3d.position]),

				   gl:pushMatrix (),
				   gl:translatef (Pos#vector.x,
						  Pos#vector.y,
						  Pos#vector.z),
				   gl:rotatef (Angle,
					       RotAxis#vector.x,
					       RotAxis#vector.y,
					       RotAxis#vector.z),

				   if
				       Angle2 =/= undefined ->
					   gl:rotatef (Angle2,
						       RotAxis2#vector.x,
						       RotAxis2#vector.y,
						       RotAxis2#vector.z);
				       true ->
					   ok
				   end,

				   %%                          gl:translatef (ChildPos#vector.x,
				   %%                                         ChildPos#vector.y,
				   %%                                         ChildPos#vector.z),

				   %%                          gl:rotatef (ChildAngle,
				   %%                                      ChildRotAxis#vector.x,
				   %%                                      ChildRotAxis#vector.y,
				   %%                                      ChildRotAxis#vector.z),

				   %%                          if
				   %%                            O#object3d.up =/= undefined ->
				   %%                              Up3 = O#object3d.up,
				   %%                              DefaultUp3 = O#object3d.default_up,
				   %%                              Angle3 = geometry:angle (DefaultUp3, Up3),
				   %%                              RotAxis3 = geometry:cross (DefaultUp3, Up3),
				   %%                              gl:rotatef (Angle3,
				   %%                                          RotAxis3#vector.x,
				   %%                                          RotAxis3#vector.y,
				   %%                                          RotAxis3#vector.z);
				   %%                            true ->
				   %%                              ok
				   %%                          end,

				   draw_object (P,
						O#object3d { parent_object = noname },O#object3d.visible),
				   gl:popMatrix (),
				   ok;
			       true ->

				   gl:pushMatrix (),

				   %%                          io:format ("[~p, ~p] ~p, ~p~n",
				   %%                                     [Obj#object3d.name,
				   %%                                      O#object3d.name,
				   %%                                      Obj#object3d.position,
				   %%                                      O#object3d.position]),
				   gl:translatef (Pos#vector.x,
						  Pos#vector.y,
						  Pos#vector.z),

				   gl:rotatef (Angle,
					       RotAxis#vector.x,
					       RotAxis#vector.y,
					       RotAxis#vector.z),

				   if
				       Angle2 =/= undefined ->
					   gl:rotatef (Angle2,
						       RotAxis2#vector.x,
						       RotAxis2#vector.y,
						       RotAxis2#vector.z);
				       true ->
					   ok
				   end,

				   gl:translatef (ChildPos#vector.x,
						  ChildPos#vector.y,
						  ChildPos#vector.z),

				   gl:rotatef (ChildAngle,
					       ChildRotAxis#vector.x,
					       ChildRotAxis#vector.y,
					       ChildRotAxis#vector.z),

				   if
				       O#object3d.up =/= undefined ->
					   Up3 = O#object3d.up,
					   DefaultUp3 = O#object3d.default_up,
					   Angle3 = geometry:angle (DefaultUp3, Up3),
					   RotAxis3 = geometry:cross (DefaultUp3, Up3),
					   gl:rotatef (Angle3,
						       RotAxis3#vector.x,
						       RotAxis3#vector.y,
						       RotAxis3#vector.z);
				       true ->
					   ok
				   end,

				   object3d:draw (P),
				   gl:popMatrix ()
			   end
		   end,
		   Obj#object3d.pids),
    ok.
%%


gather_compound_objects (O) ->
    gather_compound_objects (O, []).
%%
gather_compound_objects ([], Acc) ->
    lists:flatten (Acc);
gather_compound_objects ([P | T], Acc) ->
    O = object3d:obj (P),
    if
	O#object3d.type == compound ->
	    gather_compound_objects (T,
				     [gather_compound_objects (O#object3d.pids)
				      | Acc]);
	true ->
	    gather_compound_objects (T,
				     [{P, O} | Acc])
    end.




%%
processKey (#keyboard{sym=?SDLK_q}, StateData) ->
    {stop, normal, StateData};
%%
processKey (#keyboard{sym=?SDLK_ESCAPE}, StateData) ->
    {stop, normal, StateData};
%%
processKey (#keyboard{sym=$f}, StateData) ->
    Surface = sdl_video:getVideoSurface(),
    %%io:format("~p\n", [sdl_video:wm_toggleFullScreen(Surface)]),
    {next_state, idle_draw, StateData, StateData#engine_state.period};
%%
processKey (#keyboard{sym=$c}, StateData) ->
    io:format ("Camera positon: X = ~p, Y = ~p, Zoom = ~p~n",
	       [(StateData#engine_state.rotation)#vector.x,
		(StateData#engine_state.rotation)#vector.y,
		StateData#engine_state.zoom]),
    {next_state, idle_draw, StateData, StateData#engine_state.period};
%%

processKey (#keyboard{sym=?SDLK_LEFT}, StateData) ->

    %% OLD CODE

    %%  IncR = StateData#engine_state.rotation_increment,
    %%  NewIncR = IncR#vector { y = IncR#vector.y + ?DEFAULT_ROTATION_INCREMENT },
    %%  NewStateData =
    %% 	StateData#engine_state {rotation_increment = NewIncR },
    %%     {next_state, idle_draw, NewStateData, NewStateData#engine_state.period};

    R_rotation = StateData#engine_state.r_rotation,
    New_R_rotation = R_rotation + ?DEFAULT_ROBOT_ROTATION,
    

%%     Rot    = StateData#engine_state.rotation,
%%     NewRot = Rot#vector { y = Rot#vector.y + ?DEFAULT_ROTATION_INCREMENT },
    
    NewStateData =
    	StateData#engine_state {
	  %% rotation = NewRot,
	  r_rotation = New_R_rotation
	 },

    {next_state, idle_draw, NewStateData, NewStateData#engine_state.period};
    


%%
processKey (#keyboard{sym=?SDLK_RIGHT}, StateData) ->

    %% OLD CODE

    %%  IncR = StateData#engine_state.rotation_increment,
    %%  NewIncR = IncR#vector { y = IncR#vector.y - ?DEFAULT_ROTATION_INCREMENT },
    %%  NewStateData =
    %% 	StateData#engine_state {rotation_increment = NewIncR },
    %%     {next_state, idle_draw, NewStateData, NewStateData#engine_state.period};

    %% NEW CODE

    R_rotation = StateData#engine_state.r_rotation,
    New_R_rotation = R_rotation - ?DEFAULT_ROBOT_ROTATION,
    

%%     Rot    = StateData#engine_state.rotation,
%%     NewRot = Rot#vector { y = Rot#vector.y - ?DEFAULT_ROTATION_INCREMENT },
    
    NewStateData =
    	StateData#engine_state {
	  %% rotation = NewRot,
	  r_rotation = New_R_rotation
	 },

    {next_state, idle_draw, NewStateData, NewStateData#engine_state.period};




%%
processKey (#keyboard{sym=?SDLK_UP}, StateData) ->

    %% OLD CODE

    %%  IncR = StateData#engine_state.rotation_increment,
    %%  NewIncR = IncR#vector { x = IncR#vector.x + ?DEFAULT_ROTATION_INCREMENT },
    %%  NewStateData =
    %% 	StateData#engine_state {rotation_increment = NewIncR },
    %%     {next_state, idle_draw, NewStateData, NewStateData#engine_state.period};

    %% NEW CODE    

%%     Rot    = StateData#engine_state.rotation,
%%     NewRot = Rot#vector { x = Rot#vector.x - ?DEFAULT_ROTATION_INCREMENT },
    
    NewStateData =
    	StateData#engine_state {
	  %% rotation = NewRot,
	  r_motion = forward
	 },

    {next_state, idle_draw, NewStateData, NewStateData#engine_state.period};


%%
processKey (#keyboard{sym=?SDLK_DOWN}, StateData) ->
    
    %% OLD CODE
    
    %%  IncR = StateData#engine_state.rotation_increment,
    %%  NewIncR = IncR#vector { x = IncR#vector.x - ?DEFAULT_ROTATION_INCREMENT },
    %%  NewStateData =
    %% 	StateData#engine_state {rotation_increment = NewIncR },
    %%     {next_state, idle_draw, NewStateData, NewStateData#engine_state.period};

    %% NEW CODE

    %%     Rot    = StateData#engine_state.rotation,
%%     NewRot = Rot#vector { x = Rot#vector.x + ?DEFAULT_ROTATION_INCREMENT },
    
    NewStateData =
    	StateData#engine_state {
	  %% rotation = NewRot,
	  r_motion = reward
	 },

    {next_state, idle_draw, NewStateData, NewStateData#engine_state.period};


%%
processKey (#keyboard{sym=?SDLK_1}, StateData) ->

    %% OLD CODE

    %%  ZI = StateData#engine_state.zoom_increment,
    %%  NewStateData =
    %% 	StateData#engine_state {zoom_increment = ZI - ?DEFAULT_ZOOM_INCREMENT },
    %%     {next_state, idle_draw, NewStateData, NewStateData#engine_state.period};

    %% NEW CODE

%%     ZI = StateData#engine_state.zoom,
%%     NewStateData =
%%     	StateData#engine_state {zoom = ZI - ?DEFAULT_ZOOM_INCREMENT },
%%     {next_state, idle_draw, NewStateData, NewStateData#engine_state.period};

    Rot    = StateData#engine_state.rotation,
    NewRot = Rot#vector { z = Rot#vector.z - ?DEFAULT_ZOOM_INCREMENT },
    
    NewStateData =
    	StateData#engine_state {rotation = NewRot },

    {next_state, idle_draw, NewStateData, NewStateData#engine_state.period};




%%


processKey (#keyboard{sym=?SDLK_2}, StateData) ->

    %% OLD CODE
    
%%     ZI = StateData#engine_state.zoom_increment,
%%     NewStateData =
%% 	StateData#engine_state {zoom_increment = ZI + ?DEFAULT_ZOOM_INCREMENT },
%%     {next_state, idle_draw, NewStateData, NewStateData#engine_state.period};

    %% NEW CODE

%%     ZI = StateData#engine_state.zoom,
%%     NewStateData =
%%     	StateData#engine_state {zoom = ZI + ?DEFAULT_ZOOM_INCREMENT },
%%     {next_state, idle_draw, NewStateData, NewStateData#engine_state.period};
    

    Rot    = StateData#engine_state.rotation,
    NewRot = Rot#vector { z = Rot#vector.z + ?DEFAULT_ZOOM_INCREMENT },
    
    NewStateData =
    	StateData#engine_state {rotation = NewRot },

    {next_state, idle_draw, NewStateData, NewStateData#engine_state.period};

    

%%
processKey (#keyboard{sym=?SDLK_SPACE}, StateData) ->
    NewStateData =
	StateData#engine_state {zoom_increment = 0,
				rotation_increment = ?VECTOR(0.0, 0.0, 0.0)},
    {next_state, idle_draw, NewStateData, NewStateData#engine_state.period};
%%
processKey (K, StateData) ->
    %%io:format("Got event ~p~n", [K]),
    {next_state, idle_draw, StateData, StateData#engine_state.period}.


%%====================================================================
%% Func: terminate/3
%%====================================================================
%% @private
%%
terminate (_, _, StateData) ->

    %% stop every object3d in StateData
    lists:foreach (fun (Pid) ->
			   object3d:stop (Pid)
		   end,
		   StateData#engine_state.object_list),

    %% stop every world in StateData
    lists:foreach (fun (Pid) ->
			   rosen_world:stop (Pid)
		   end,
		   StateData#engine_state.world_list),
    ok.


%%====================================================================
%% Func: init/1
%%====================================================================
%% @private
%%
init (Config) ->
    %% Init

    sdl:init(?SDL_INIT_VIDEO),
    sdl_util:debug(1),

    %% if atom "fullscreen" is prensent in Config
    %% add option "FULLSCREEN".
    Flags =
	case lists:member(fullscreen, Config) of
	    true ->
		?SDL_OPENGL  bor ?SDL_FULLSCREEN;
	    _ ->
		?SDL_OPENGL  bor ?SDL_RESIZABLE
	end,

    %% 0 or 1, enable or disable double buffering
    sdl_video:gl_setAttribute(?SDL_GL_DOUBLEBUFFER, 1),

    %% Returns a ref to an array of available screen dimensions for the
    %% given format (null in this case) and video flags, or it return
    %% undef if no modes are available

    %% with fullscreen
    AvailableWindowedSzs = sdl_video:listModes(null, Flags bor ?SDL_FULLSCREEN),
    %% without fullscreen
    %% AvailableWindowedSzs = sdl_video:listModes(null, Flags),

    DriverName = sdl_video:videoDriverName(),

    io:format("Driver ~p ~n", [DriverName]),
    io:format("Available WindowSizes ~p ~n", [AvailableWindowedSzs]),

    case AvailableWindowedSzs of
	[{_, 0,0,W,H}|_] ->
	    %% put in Test all value of [32, 24, 16, 15] that return
	    %% true in sdl_video:videoModeOK(W,H,Test,Flags)
	    %% Test is the value of bit per pixel (bpp)
	    %% Res NEVER USED !!
	    Res = [Test || Test <- [32,24,16,15],
			   true == sdl_video:videoModeOK(W,H,Test,Flags)];
	%% hd(List) return the head of a list.
	%%       io:format("A guess at max video res is ~px~p:~p ~n", [W,H, hd(Res)]);
	_ ->
	    %%       io:format("Can't guess max resolution~n", []),
	    W = 800, H = 600
    end,

    %% set width and height as less than half fullscreen

    New_W = erlang:trunc(W/2 - W/10),
    New_H = erlang:trunc(H/2 - H/10),

    SR = sdl_video:setVideoMode(New_W, New_H, 16, Flags),


    %% GET AND PRINT SOME INFORMATION

    Rs= sdl_video:gl_getAttribute(?SDL_GL_RED_SIZE),
    Gs= sdl_video:gl_getAttribute(?SDL_GL_GREEN_SIZE),
    Bs= sdl_video:gl_getAttribute(?SDL_GL_BLUE_SIZE),

    Ds= sdl_video:gl_getAttribute(?SDL_GL_DEPTH_SIZE),
    Db= (1 == sdl_video:gl_getAttribute(?SDL_GL_DOUBLEBUFFER)),
    %%   io:format("OpenGL attributes ~n"),
    %%   io:format("Sizes in bits Red ~p Green ~p Blue ~p Depth ~p Doublebuffered ~p~n",
    %%             [Rs, Gs, Bs, Ds, Db]),
    %%   io:format("Vendor:     ~s~n",  [gl:getString(?GL_VENDOR)]),
    %%   io:format("Renderer:   ~s~n",  [gl:getString(?GL_RENDERER)]),
    %%   io:format("Version:    ~s~n",  [gl:getString(?GL_VERSION)]),
    %%   io:format("GL AUX BUFFERS ~p~n",  [gl:getIntegerv(?GL_AUX_BUFFERS)]),
    %%   io:format("SDL Version ~p~n",  [sdl_video:wm_getInfo()]),

    %%   io:format("Extensions: ~s~n",  [gl:getString(?GL_EXTENSIONS)]),
    %%   io:format("Maximized: ~p~n",   [sdl_video:wm_isMaximized()]),

    %%   io:format("~p", [catch gl:getConvolutionParameterfv(16#8011, 16#801A)]),

    %% END

    %% disable all events, except for
    %% key press, quit, videoresize
    sdl_events:eventState(?SDL_ALLEVENTS ,?SDL_IGNORE),
    sdl_events:eventState(?SDL_KEYDOWN ,?SDL_ENABLE),
    sdl_events:eventState(?SDL_QUIT ,?SDL_ENABLE),
    sdl_events:eventState(?SDL_VIDEORESIZE, ?SDL_ENABLE),
    %%   ?printError(),

    %% proplists:get_value/2 returns the value of a simple key/value property in List.
    %% If lookup(Key, List) would yield {Key, Value}, this function returns the
    %% corresponding Value, otherwise Default is returned.

    %% get term "CollisionOptions" from element {collision, CollisionOptions} in Config list. 
    CollisionOptions = proplists:get_value (collision, Config),

    %% get term "TimeType" from element {timeType, TimeType} in Config list.
    %% {timeType, simulated} or {timeType, _ }
    TimeType = proplists:get_value (timeType, Config),


    EngineState = #engine_state { zoom = 0.0,
				  screen_h = New_H,
				  screen_w = New_W,

				  %% collision_options = proplists:get_value (collision, Config),
				  collision_options = CollisionOptions,

				  %% valueCall return true if CollisionOptions is not "undefined"
				  collision_enable = valueColl(CollisionOptions),
				  time_type = TimeType,
				  time = 0.0,
				  delta = ?SIMULATED_DELTATIME,

				  %% last_time is the init time
				  %% {0.0, 0.0, 0.0} or now()
				  last_time = setInitTime(TimeType)

				 },


    %% RECOR FOR engine_state
    %%
    %%     -record (engine_state, {object_list = [],
    %%              world_list = [],
    %%              period = ?DEFAULT_PERIOD,
    %%              screen_h,
    %%              screen_w,
    %%              rotation = ?VECTOR (0.0, 0.0, 0.0),
    %%              rotation_increment = ?VECTOR (0.0, 0.0, 0.0),
    %%              zoom = 0.0,
    %%              zoom_increment = 0.0,
    %%              collision_options,
    %% 		    collision_enable = false,
    %% 		    time_type, 
    %% 		    time,
    %% 		    delta,
    %% 		    last_time
    %% 		    }).


    initWin(EngineState),
    
    sdl_util:debug(00),

    %% { ok, StateName, StateDate, Timeout}
    {ok, idle_draw, EngineState, ?DEFAULT_PERIOD}.


						
valueColl(undefined) -> false;
valueColl(Coll) -> true.
						



initWin (EngineState) ->
    W = EngineState#engine_state.screen_w,
    H = EngineState#engine_state.screen_h,

    %% see set_viewport/4 funcion
    %%
%%     set_viewport (W, H,
%%                   EngineState#engine_state.screen_w,
%%                   EngineState#engine_state.screen_h),


    %% OPENGL section
    %% more at http://www.opengl.org/sdk/docs/man/xhtml/


    %% Specifies which matrix stack is the target
    %% for subsequent matrix operations.
    %% GL_PROJECTION ->
    %% Applies subsequent matrix operations to the projection matrixs.
    gl:matrixMode(?GL_PROJECTION),

    %% replace the current matrix with the identity matrix.
    gl:loadIdentity(),

    %% multiply the current matrix with an orthographic matrix
    %% (see http://www.opengl.org/sdk/docs/man/xhtml/glOrtho.xml
    %%  for further details)
    %% gl:ortho( -2.0, 2.0, -2.0, 2.0, -20.0, 20.0),


    %% set up a perspective projection matrix
    %% Parameters:
    %%
    %% fovy ->
    %% Specifies the field of view angle, in degrees, in the y direction.
    %%
    %% aspect ->
    %% Specifies the aspect ratio that determines the field of view in the x direction.
    %% The aspect ratio is the ratio of x (width) to y (height).
    %%
    %% zNear ->
    %% Specifies the distance from the viewer to the near clipping plane
    %% (always positive).
    %%
    %% zFar ->
    %% Specifies the distance from the viewer to the far clipping plane
    %% (always positive).
    glu:perspective( 50.0, (W * 1.0) / (H * 1.0), 1.0, 1000.0),


    %% Specifies which matrix stack is the target
    %% for subsequent matrix operations.
    %% GL_MODELVIEW ->
    %% Applies subsequent matrix operations to the projection matrixs.
    gl:matrixMode(?GL_MODELVIEW),

    %% replace the current matrix with the identity matrix.
    gl:loadIdentity(),

    %% specify clear values for the color buffers
    gl:clearColor(0.0,0.0,0.0,1.0),

    %% select flat or smooth shading
    %% GL_SMOOTH or GL_FLAT
    gl:shadeModel (?GL_SMOOTH),

    %% enable or disable server-side GL capabilities
    %% GL_DEPTH_TEST ->
    %% If enabled, do depth comparisons and update the depth buffer. Note that even if
    %% the depth buffer exists and the depth mask is non-zero, the
    %% depth buffer is not updated if the depth test is disabled. 
    gl:enable(?GL_DEPTH_TEST),


    %% specify the value used for depth buffer comparisons
    gl:depthFunc(?GL_LEQUAL),

    %% specify implementation-specific hints
    gl:hint(?GL_PERSPECTIVE_CORRECTION_HINT, ?GL_NICEST),

    %% camera, look-at-point, up-vector
    %% glu:lookAt(1.0, 1.5, 15.0, 0.0, 0.0, -50.0, 0.0, 1.0, 0.0),
    %%   glu:lookAt(0.0, 0.0, 10.0,
    %%              0.0, 0.0, 0.0,
    %%              0.0, 1.0, 0.0),

    %% enable or disable server-side GL capabilities
    gl:enable(?GL_DEPTH_TEST),

    %% specify the value used for depth buffer comparisons
    gl:depthFunc(?GL_LESS),

    %% set light source parameters for GL_LIGHT0 (ambient)
    %% params contains four integer or floating-point values that specify
    %% the ambient RGBA intensity of the light. 
    gl:lightfv(?GL_LIGHT0, ?GL_AMBIENT, {0.01, 0.01, 0.01, 1.0}),


    %% set light source parameters for GL_LIGHT0 (diffuse)
    gl:lightfv (?GL_LIGHT0, ?GL_DIFFUSE, {0.7, 0.7, 0.7, 1.0 }),

    %% set light source parameters for GL_LIGHT0 (position)
    %% Params contains four integer or floating-point values that specify
    %% the position of the light in homogeneous object coordinates.
    gl:lightfv (?GL_LIGHT0, ?GL_POSITION, {-10.0, 10.0, 10.0, 0.0 }),


    %% set the lighting model parameters (light model ambient)
    %% params contains four integer or floating-point values that specify
    %% the ambient RGBA intensity of the entire scene.                 
    gl:lightModelfv(?GL_LIGHT_MODEL_AMBIENT, { 0.3, 0.3, 0.3, 1.0 }),

    %% set the lighting model parameters (light model local viewer)
    %% params is a single integer or floating-point value that specifies
    %% how specular reflection angles are computed.
    gl:lightModelfv(?GL_LIGHT_MODEL_LOCAL_VIEWER, { 0.0 }),

    %% turn GL_LIGHT0 on.
    gl:enable(?GL_LIGHT0),

    %% turn GL_LIGHTING on.
    gl:enable (?GL_LIGHTING),

    %% If enabled, have one or more material parameters track the current color
    gl:enable(?GL_COLOR_MATERIAL),

    %% If enabled and no vertex shader is active,
    %% normal vectors are normalized to unit length
    %% after transformation and before lighting.
    gl:enable(?GL_NORMALIZE),

    %% If enabled, blend the computed fragment color values with the values
    %% in the color buffers.
    gl:enable (?GL_BLEND),

    %% specify pixel arithmetic
    gl:blendFunc (?GL_SRC_ALPHA, ?GL_ONE_MINUS_SRC_ALPHA),

    %%gl:colorMaterial(?GL_FRONT, ?GL_AMBIENT_AND_DIFFUSE),
    %%gl:materialfv(?GL_FRONT, ?GL_AMBIENT_AND_DIFFUSE, {0.5, 0.5, 0.5, 1.0}),

    %%   gl:enable(?GL_CULL_FACE),
    %%   gl:cullFace(?GL_BACK),

    ok.

set_viewport (W, H, ScreenW, ScreenH) ->

    %%  glViewport specifies the affine transformation of x and y from
    %% 	normalized device coordinates to window coordinates.

    DW = (ScreenW - W) / 2,
    DH = (ScreenH - H) / 2,
    io:format ("set_viewport ~p~n", [{DW, DH, W, H}]),
    gl:viewport (trunc(DW), trunc(DH), W, H).


%%     glViewport(0, 0, w, h);
%%     glMatrixMode(GL_PROJECTION);
%%     glLoadIdentity();
%%     glFrustum(-1, 1, -1, 1, 1, 20); // left, right, bottom, top, near, far
%%     glMatrixMode(GL_MODELVIEW);
%%     glLoadIdentity();
%%     gluLookAt(cx, cy, cz, l1, l2, l3, u1, u2, u3); // camera, look-at-point, up-vector


%%====================================================================
%% Func: handle_event/3
%%====================================================================
%% @private
%%
handle_event ({add_world, WorldPid}, StateName, StateData) ->

    %% add new World in world_list
    %% next state remains the previous
    NewStateData =
	StateData#engine_state {world_list = [WorldPid | StateData#engine_state.world_list]},
    {next_state, StateName, NewStateData};

handle_event ({set_collision, Value}, StateName, StateData) ->

    %% enable or disable collision
    %% next state remains the previous
    NewStateData =
	StateData#engine_state {collision_enable = Value},
    %% io:format("Set delle Collisioni a ~p~n",[Value]),
    {next_state, StateName, NewStateData};


handle_event (stop, _StateName, StateData) ->

    %% call terminate(Reason, NewStateData) to end
    %% as terminate(normal, StateData).
    {stop, normal, StateData};

%%% ADDED BY ME %%%
handle_event ({ set_main_robot, Pid } , StateName, StateData) ->

    %% enable or disable collision
    %% next state remains the previous
    NewStateData =
	StateData#engine_state { main_robot = Pid},
    %% io:format("Set delle Collisioni a ~p~n",[Value]),
    {next_state, StateName, NewStateData}.



%%
%% time_handler 
%% To handle the real or simulated time 

time_handler(State = #engine_state {time_type = simulated}) ->
    Now = State#engine_state.time,
    Delta = ?SIMULATED_DELTATIME,
    NewTime = State#engine_state.time + Delta,
    %% io:format("Tempo Simulato Now  ~p Last Time ~p  Delta ~p NewTime ~p ~n",
    %% [Now,State#activity_state.last_time, Delta, NewTime]),
    NewState = State#engine_state{ last_time = Now,
				   delta = Delta,
				   time = NewTime };
time_handler(State) -> 
    Now = now (),
    Delta = timer:now_diff (Now, State#engine_state.last_time) / 1000000.0,
    %% difference in seconds
    NewTime = State#engine_state.time + Delta,
    %% io:format("Tempo Reale Now  ~p Last Time ~p  Delta ~p NewTime ~p Modulo ~p  ~n",
    %% [Now,State#activity_state.last_time, Delta, NewTime,State#activity_state.module ]),
    NewState = State#engine_state{time_type = real,
				  last_time = Now,
				  delta = Delta,
				  time = NewTime}.

%%
setInitTime(simulated)->    {0.0,0.0,0.0};
setInitTime(_)->    now().

%% setDeltaTime(simulated,undefined)->    Delta;
%% setDeltaTime(_,Delta)->    now().
