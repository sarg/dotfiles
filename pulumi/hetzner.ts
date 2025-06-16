import * as pulumi from '@pulumi/pulumi';
import * as hcloud from '@pulumi/hcloud';
import { SshKey } from '.';

export class Hetzner extends pulumi.ComponentResource {
  constructor(name: string, args: { sshKey: SshKey }, opts?: pulumi.ComponentResourceOptions) {
    super('components:index:Hetzner', name, args, opts);
    new hcloud.PrimaryIp(
      `vpn_ipv6`,
      {
        name: 'vpn_ipv6',
        datacenter: 'hel1-dc2',
        assigneeType: 'server',
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
