import * as telegram from '@pulumi/telegram';
import * as cloudflare from '@pulumi/cloudflare';
import { Cloudflare } from './cloudflare';
import { Google } from './google';
import { Hetzner } from './hetzner';
import { Selectel } from './selectel';
import { Telegram } from './telegram';
import { Convert } from './secrets';
import { execSync } from 'node:child_process';
import * as hcloud from '@pulumi/hcloud';
import * as openstack from '@pulumi/openstack';
import * as gcp from '@pulumi/gcp';

const secretsJson = execSync('sops -d --output-type json ../secrets.yaml');
const secrets = Convert.toSecrets(secretsJson.toString('utf8'));
export interface SshKey {
  name: string;
  key: string;
}

const cloudflareProvider = new cloudflare.Provider('default', {
  apiToken: secrets.cloudflare.API_TOKEN,
});
const telegramProvider = new telegram.Provider('default', {
  botToken: secrets.telegram.BOT_TOKEN,
});
const hetznerProvider = new hcloud.Provider('default', {
  token: secrets.hetzner.API_TOKEN,
});
const selectelProvider = new openstack.Provider('default', {
  cloud: 'selectel',
  region: 'ru-9',
});
const googleProvider = new gcp.Provider('default', {
  userProjectOverride: true,
  // "Pulumi Deployer" service account
  credentials: secrets.google.CREDENTIALS,
  project: 'se-da-sa-1',
});

const sshKey = {
  name: 'thinkpad',
  key: 'ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQDPX3feK7quyEzu/Uq88nCDmcplx7QA4DIGj8gMDuOnW+hs/rTJxJbT3Z2i2f6PdL5dD3v2HJir80Q13Iqp+fcf7OInTwxj8HQohpaY3MK7Wjq4maTRSvpqf8VxNV4jKQl6tacMqbAAtamXxKy1fQriEZMnWdUxjYDBQrYXm8VcGRlWINkUZBvH1YDlV6AJWgNjgAKbzmQzBB7b9MO4DWz7jjbF3Tl7E6Vro32lg0DZcED2wXAqslKp89bW5Y2th1O7DxS8qEI7qQg2E8IUIfaXacLGNsuAV3d/8YTu71sE9k4aUjEfljN/5KoXKzqosVcMe4aebUs+1X/GwIKVZgfjN1h0mugnrsyUesi5MxhnJlEDMdIUL0PGChabQZLO0SnXoOeWORe5HQnuLUBABhqVDg5xHqsIGEvlhGnh3V+CN3twuj7gUuKO2dcJBUxCipE7+ESNc89f7+aHMhDiFmxuMxH5GoTqJjjZx5AtGZCLfmzMB6qH08j2nwVjMwhVPPSNtNQ5iVfkXiOOMSC/W9+nsyzj+bO4xqOpqUl8pBIuJjofea++U49lBDyU4dr5bsriBYmB21BAqu3zFFjf/kydEdPQ5EY+Pt9zAM4dNTLZXo98DV2F/J5pimbbB4VkuipBDPQIWb0DUtqeT/OTV/6qFZd6OOFeJsJxxc71dREhZw== (thinkpad)',
};
const google = new Google('google', { orgId: secrets.google.ORG_ID }, { provider: googleProvider });
new Selectel('selectel', { sshKey }, { provider: selectelProvider });
new Hetzner('hetzner', { sshKey }, { provider: hetznerProvider });
new Cloudflare('cloudflare', { secrets }, { provider: cloudflareProvider });
new Telegram('telegram', {}, { provider: telegramProvider });

export const pulumiKey = google.serviceAccountKey.privateKey;
