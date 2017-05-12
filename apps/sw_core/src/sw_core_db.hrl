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
