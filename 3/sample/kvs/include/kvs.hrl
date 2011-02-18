-type(kvs_args()::{integer(), integer(), true | false, self | node()}).

-type(worker_spec()::{term(), {atom(), atom(), [term(), ...]}, permanent, brutal_kill | integer(), worker, [atom(), ...] | []}).
-type(supervisor_spec()::{ok, {{one_for_one, integer(), integer()},[worker_spec(), ...]}} | ignore).
