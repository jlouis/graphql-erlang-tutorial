%% Database Schema definition files

%% tag::shipRecord[]
-record(starship,
        {id :: binary(),
         name :: binary(),
         model :: binary(),
         class :: binary(),
         manufacturers :: [binary()],
         cost :: float(),
         length :: float(),
         crew :: binary(),
         passengers :: binary(),
         max_speed :: integer(),
         hyperdrive_rating :: float(),
         mglt :: integer(),
         cargo_capacity :: float(),
         consumables :: binary(),
         created :: binary(),
         edited :: binary()}).
%% end::shipRecord[]
