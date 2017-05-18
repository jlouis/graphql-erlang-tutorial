%% Database Schema definition files

%% tag::starshipRecord[]
-record(starship,
        {id :: integer(),
         pilots :: [integer()],
         mglt :: integer(),
         starship_class :: binary(),
         hyperdrive_rating :: binary()
}).
%% end::starshipRecord[]

%% tag::transportRecord[]
-record(transport,
        {id :: binary(),
         edited :: binary(),
         consumables :: binary(),
         name :: binary(),
         created :: binary(),
         cargo_capacity :: float(),
         passengers :: binary(),
         max_speed :: integer(),
         crew :: binary(),
         length :: float(),
         model :: binary(),
         cost :: float(),
         manufacturers :: [binary()]
}).
%% end::transportRecord[]

-record(film,
        {id :: binary() | integer(),
         edited :: binary(),
         created :: binary(),
         starships :: [integer()],
         vehicles :: [integer()],
         planets :: [integer()],
         producer :: binary(),
         title :: binary(),
         episode_id :: integer(),
         director :: binary(),
         release_date :: binary(),
         opening_crawl :: binary(),
         characters :: [integer()] }).
         
-record(species,
        {id :: binary() | integer(),
         edited :: binary(),
         created :: binary(),
         classification :: binary(),
         name :: binary(),
         designation :: binary(),
         eye_colors :: [binary()],
         people :: [integer()],
         skin_colors :: [binary()],
         language :: binary(),
         hair_colors :: [binary()],
         homeworld :: binary(),
         average_lifespan :: integer(),
         average_height :: integer()
}).

-record(person,
        {id :: binary(),
         edited :: binary(),
         name :: binary(),
         created :: binary(),
         gender :: binary(),
         skin_color :: binary(),
         hair_color :: binary(),
         height :: integer(),
         eye_color :: binary(),
         mass :: integer(),
         homeworld :: integer(),
         birth_year :: binary()
}).

%% tag::planetRecord[]
-record(planet,
        {id :: binary(),
         edited :: binary(),
         climate :: binary(),
         surface_water :: integer(),
         name :: binary(),
         diameter :: integer(),
         rotation_period :: integer(),
         created :: binary(),
         terrain :: binary(),
         gravity :: binary(),
         orbital_period :: integer(),
         population :: integer()
}).
%% end::planetRecord[]

-record(vehicle,
        {id :: binary(),
         vehicle_class :: binary(),
         pilots :: [integer()]}).

-record(sequences, {key, value}).
