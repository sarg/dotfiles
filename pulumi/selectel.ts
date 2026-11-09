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
  }
}
