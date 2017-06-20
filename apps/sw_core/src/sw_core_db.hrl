%% Database Schema definition files

%% tag::factionRecord[]
-record(faction,
        {id :: integer(),
         name :: integer()}).
%% end::factionRecord[]

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
         edited :: calendar:datetime(),
         consumables :: binary(),
         name :: binary(),
         created :: calendar:datetime(),
         cargo_capacity :: float(),
         passengers :: binary(),
         crew :: binary(),
         length :: float(),
         model :: binary(),
         cost :: float(),
         max_atmosphering_speed :: integer(),
         manufacturers :: [binary()],
         faction = undefined :: undefined | integer()
}).
%% end::transportRecord[]

-record(film,
        {id :: binary() | integer(),
         edited :: calendar:datetime(),
         created :: calendar:datetime(),
         starships :: [integer()],
         species :: [integer()],
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
         edited :: calendar:datetime(),
         name :: binary(),
         created :: calendar:datetime(),
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
         edited :: calendar:datetime(),
         climate :: binary(),
         surface_water :: integer(),
         name :: binary(),
         diameter :: integer(),
         rotation_period :: integer(),
         created :: calendar:datetime(),
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
