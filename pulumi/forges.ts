import * as pulumi from '@pulumi/pulumi';
import * as github from '@pulumi/github';
import * as gitea from '@pulumi/gitea';
import { PublicKey } from '.';

export class Forges extends pulumi.ComponentResource {
  constructor(
    name: string,
    args: { sshKey: PublicKey; gpgKey: PublicKey; codebergToken: string },
    opts?: pulumi.ComponentResourceOptions,
  ) {
    super('components:index:Forges', name, args, opts);

    /* use oauth
    new github.UserSshKey(
      'default',
      {
        // remove key name to match how github saves it
        key: args.sshKey.key.split(' ').slice(0, 2).join(' '),
        title: args.sshKey.name,
      },
      { parent: this },
    );
    */

    new github.UserGpgKey(
      'default',
      { armoredPublicKey: args.gpgKey.key },
      { parent: this, deleteBeforeReplace: true },
    );

    /* use oauth
    new gitea.PublicKey(
      'default',
      { key: args.sshKey.key, title: args.sshKey.name },
      { parent: this },
    );
    */

    new gitea.GpgKey(
      'default',
      { armoredPublicKey: args.gpgKey.key },
      { parent: this, deleteBeforeReplace: true },
    );
  }
}
