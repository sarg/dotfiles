import * as pulumi from '@pulumi/pulumi';
import * as github from '@pulumi/github';
import { PublicKey } from '.';

export class Forges extends pulumi.ComponentResource {
  constructor(
    name: string,
    args: { sshKey: PublicKey; gpgKey: PublicKey },
    opts?: pulumi.ComponentResourceOptions,
  ) {
    super('components:index:Forges', name, args, opts);

    new github.UserSshKey(
      'default',
      {
        // remove key name to match how github saves it
        key: args.sshKey.key.split(' ').slice(0, 2).join(' '),
        title: args.sshKey.name,
      },
      { parent: this },
    );

    new github.UserGpgKey('default', { armoredPublicKey: args.gpgKey.key }, { parent: this });

    // TODO: neither gitea nor forgejo providers support managing of user ssh/gpg keys
  }
}
