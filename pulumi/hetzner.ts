import * as pulumi from '@pulumi/pulumi';
import * as hcloud from '@pulumi/hcloud';
import { PublicKey } from '.';

export class Hetzner extends pulumi.ComponentResource {
  ipv6Ip: hcloud.PrimaryIp;

  constructor(name: string, args: { sshKey: PublicKey }, opts?: pulumi.ComponentResourceOptions) {
    super('components:index:Hetzner', name, args, opts);
    this.ipv6Ip = new hcloud.PrimaryIp(
      `vpn_ipv6`,
      {
        name: 'vpn_ipv6',
        location: 'hel1',
        autoDelete: false,
        type: 'ipv6',
      },
      { parent: this },
    );

    new hcloud.SshKey(
      `ssh_key`,
      {
        name: args.sshKey.name,
        publicKey: args.sshKey.key,
      },
      { parent: this, deleteBeforeReplace: true },
    );
  }
}
