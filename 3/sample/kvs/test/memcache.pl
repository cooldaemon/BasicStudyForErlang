use Data::Dumper;
use Cache::Memcached::libmemcached;
my $memd = Cache::Memcached::libmemcached->new({
#    servers => ['localhost:11212',],
    servers => ['localhost:11211',],
});

print Dumper($memd), "\n";

$memd->set(foo => 'bar');
print $memd->get('foo'), "\n";

$memd->set(object_key => {complex => ['object', 2, 4]});
print Dumper($memd->get('object_key')), "\n";

