import * as pulumi from '@pulumi/pulumi';
import * as cloudflare from '@pulumi/cloudflare';
import { Secrets } from './secrets';

function dnsRecord(
  logicalName: string,
  zone: cloudflare.Zone,
  name: string,
  attrs: Partial<cloudflare.DnsRecordArgs>,
) {
  return new cloudflare.DnsRecord(
    `${attrs.type} ${logicalName}`,
    {
      zoneId: zone.id,
      name: name == '@' ? zone.name : pulumi.concat(name, '.', zone.name),
      type: 'A',
      ttl: 1,
      ...attrs,
    },
    { parent: zone, deleteBeforeReplace: true },
  );
}

function secret(s: string, v: pulumi.Input<string>) {
  return { type: 'secret_text', name: s, text: v };
}

export class Cloudflare extends pulumi.ComponentResource {
  accountId: string;
  private kvNamespace(name: string, opts?: pulumi.CustomResourceOptions) {
    return new cloudflare.WorkersKvNamespace(
      name,
      { accountId: this.accountId, title: name },
      { parent: this, ...opts },
    );
  }
  private worker(name: string, args?: Partial<cloudflare.WorkerScriptArgs>) {
    return new cloudflare.WorkersScript(
      name,
      {
        accountId: this.accountId,
        scriptName: name,
        observability: { enabled: true },
        content: `// stub export default {async fetch(request, env, ctx) {return new Response('Hello World!');},};`,
        mainModule: 'index.js',
        ...args,
      },
      { parent: this, ignoreChanges: ['content'] },
    );
  }
  constructor(name: string, args: { secrets: Secrets }, opts?: pulumi.ComponentResourceOptions) {
    super('components:index:Cloudflare', name, args, opts);
    this.accountId = args.secrets.cloudflare.ACCOUNT_ID;
    const zone = new cloudflare.Zone(
      `sarg.org.ru`,
      { account: { id: this.accountId }, name: 'sarg.org.ru', type: 'full' },
      { parent: this },
    );
    [
      { content: 'aspmx3.googlemail.com', priority: 10 },
      { content: 'alt1.aspmx.l.google.com', priority: 10 },
      { content: 'alt2.aspmx.l.google.com', priority: 5 },
      { content: 'aspmx2.googlemail.com', priority: 10 },
    ].forEach((val, idx) => dnsRecord(`${idx}`, zone, '@', { type: 'MX', ...val }));
    dnsRecord('dmarc', zone, '_dmarc', {
      type: 'TXT',
      content: '"v=DMARC1; p=none"',
    });
    dnsRecord('dkim', zone, 'google._domainkey', {
      type: 'TXT',
      content:
        '"v=DKIM1; k=rsa; p=MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEAy2prlguGQ2S9tKRxBUmgt8ZB76puBdhod705hPnmecmmVdezp7Jo/OMx7Qyl/mq9bcreixGn41fCwHbWmJ0DvGWsMEN2iftvZwllORs1q8vgYVHcZBWBt4Dtg9OBDaoShzuFoOO7ho1XjK5TTjlYSXWkfFvpZR5DTUVETVCezvWExqCLDFJc1aHZOfRU44IVz" "p6YSlLgKjisBQ485XxsAL888BkSn3ocPPpioe5pNGVJphW1KBtiiR8HI3Z8MDmyAoYDknYCOCaYHd9G6ohSbpdG3Y2qubvVxW6nz1JuFdBNbb0qzGkjbCBXeUSJQl7GV3hdX6OjuJFm89gFwIyelQIDAQAB"',
    });
    dnsRecord('spf', zone, '@', {
      type: 'TXT',
      content: '"v=spf1 include:_spf.google.com ~all"',
    });
    dnsRecord('mail', zone, 'mail', {
      type: 'CNAME',
      content: 'ghs.googlehosted.com',
      proxied: false,
    });
    this.worker('blog');
    this.worker('dyndns', {
      bindings: [secret('CLOUDFLARE_ACCOUNT_ID', args.secrets.cloudflare.ACCOUNT_ID)],
    });
    const tgbot = this.worker('tgbot', {
      bindings: [
        secret('BOT_TOKEN', args.secrets.telegram.BOT_TOKEN),
        secret('HETZNER_IPV6_ID', args.secrets.hetzner.PRIMARY_IPV6_ID),
        secret('HCLOUD_TOKEN', args.secrets.hetzner.API_TOKEN),
        secret('SELECTEL_PASS', args.secrets.selectel.PASS),
        secret('SELECTEL_ORG_ID', args.secrets.selectel.ORG_ID),
        secret('WG_PRIVATE', args.secrets.vpn.WG_PRIVATE),
        secret('WG_PSK', args.secrets.vpn.WG_PSK),
        secret('DYNDNS_TOKEN', args.secrets.dyndns.find((t) => t.name === 'vpn')!.token),
      ],
    });
    this.kvNamespace('NOTIFY_TOKENS', { parent: tgbot });
  }
}
