import * as pulumi from '@pulumi/pulumi';
import * as openstack from '@pulumi/openstack';
import { PublicKey } from '.';

export class Selectel extends pulumi.ComponentResource {
  constructor(name: string, args: { sshKey: PublicKey }, opts?: pulumi.ComponentResourceOptions) {
    super('components:index:Selectel', name, args, opts);
    new openstack.compute.Keypair(
      'ssh_key',
      { name: args.sshKey.name, publicKey: args.sshKey.key },
      { parent: this },
    );

    const natNetwork = new openstack.networking.Network(
      'private',
      { name: 'nat' },
      { parent: this },
    );
    const externalNetwork = openstack.networking.getNetworkOutput({ external: true }, opts);

    const router = new openstack.networking.Router(
      'router',
      {
        name: 'router-nat',
        externalNetworkId: externalNetwork.apply((n) => n.id),
      },
      { parent: this },
    );

    const privateSubnet = new openstack.networking.Subnet(
      'private',
      {
        networkId: natNetwork.id,
        cidr: '192.168.0.0/24',
        dnsNameservers: ['188.93.16.19', '188.93.17.19'],
        enableDhcp: false,
      },
      { parent: natNetwork },
    );

    new openstack.networking.RouterInterface(
      'interface',
      { routerId: router.id, subnetId: privateSubnet.id },
      { parent: router },
    );

    new openstack.networking.Port(
      'vpn',
      {
        networkId: natNetwork.id,
        name: 'vpn',
        fixedIps: [{ subnetId: privateSubnet.id }],
      },
      { parent: natNetwork },
    );
  }
}
