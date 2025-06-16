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
  key: execSync('gpg --export-ssh-key 7B83471F7F88DC24').toString('utf8'),
};
const google = new Google('google', { orgId: secrets.google.ORG_ID }, { provider: googleProvider });
new Selectel('selectel', { sshKey }, { provider: selectelProvider });
new Hetzner('hetzner', { sshKey }, { provider: hetznerProvider });
new Cloudflare('cloudflare', { secrets }, { provider: cloudflareProvider });
new Telegram('telegram', {}, { provider: telegramProvider });

export const pulumiKey = google.serviceAccountKey.privateKey;
export const gptelKey = google.gptelKey.keyString;
