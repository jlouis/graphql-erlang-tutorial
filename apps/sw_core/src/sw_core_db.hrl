%% Database Schema definition files

%% tag::shipRecord[]
-record(starship,
        {id :: binary(),
         cargo_capacity :: float(),
         class :: binary(),
         consumables :: binary(),
         cost :: float(),
         created :: binary(),
         crew :: binary(),
         edited :: binary(),
         hyperdrive_rating :: float(),
         length :: float(),
         manufacturers :: [binary()],
         max_speed :: integer(),
         mglt :: integer(),
         model :: binary(),
         name :: binary(),
         passengers :: binary()
}).
%% end::shipRecord[]

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

