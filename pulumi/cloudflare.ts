import * as pulumi from "@pulumi/pulumi";
import * as cloudflare from "@pulumi/cloudflare";

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
            name:
                name == "@"
                    ? zone.name
                    : pulumi.interpolate`${name}.${zone.name}`,
            type: "A",
            ttl: 1,
            ...attrs,
        },
        { parent: zone, deleteBeforeReplace: true },
    );
}

export class Cloudflare extends pulumi.ComponentResource {
    tgbotDomain: cloudflare.WorkersCustomDomain;

    constructor(
        name: string,
        args?: pulumi.Inputs,
        opts?: pulumi.ComponentResourceOptions,
    ) {
        super("components:index:Cloudflare", name, args, opts);
        const accountId = process.env["CLOUDFLARE_ACCOUNT_ID"]!;
        const zone = new cloudflare.Zone(
            `sarg.org.ru`,
            {
                account: { id: accountId },
                name: "sarg.org.ru",
                type: "full",
            },
            { parent: this },
        );

        [
            { content: "aspmx3.googlemail.com", priority: 10 },
            { content: "alt1.aspmx.l.google.com", priority: 10 },
            { content: "alt2.aspmx.l.google.com", priority: 5 },
            { content: "aspmx2.googlemail.com", priority: 10 },
        ].forEach((val, idx) =>
            dnsRecord(`${idx}`, zone, "@", { type: "MX", ...val }),
        );

        dnsRecord("dmarc", zone, "_dmarc", {
            type: "TXT",
            content: "v=DMARC1; p=none",
        });
        dnsRecord("dkim", zone, "google._domainkey", {
            type: "TXT",
            content:
                "v=DKIM1; k=rsa; p=MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEAy2prlguGQ2S9tKRxBUmgt8ZB76puBdhod705hPnmecmmVdezp7Jo/OMx7Qyl/mq9bcreixGn41fCwHbWmJ0DvGWsMEN2iftvZwllORs1q8vgYVHcZBWBt4Dtg9OBDaoShzuFoOO7ho1XjK5TTjlYSXWkfFvpZR5DTUVETVCezvWExqCLDFJc1aHZOfRU44IVzp6YSlLgKjisBQ485XxsAL888BkSn3ocPPpioe5pNGVJphW1KBtiiR8HI3Z8MDmyAoYDknYCOCaYHd9G6ohSbpdG3Y2qubvVxW6nz1JuFdBNbb0qzGkjbCBXeUSJQl7GV3hdX6OjuJFm89gFwIyelQIDAQAB",
        });
        dnsRecord("spf", zone, "@", {
            type: "TXT",
            content: "v=spf1 include:_spf.google.com ~all",
        });

        dnsRecord("mail", zone, "mail", {
            type: "CNAME",
            content: "ghs.googlehosted.com",
            proxied: false,
        });
        dnsRecord("@", zone, "@", {
            type: "CNAME",
            content: "sarg.github.io",
            proxied: true,
        });

        const tgbotWorkersScript = new cloudflare.WorkersScript(
            `tgbot`,
            {
                accountId,
                scriptName: "tgbot",
                content: `// stub
                    export default {
                        async fetch(request, env, ctx) {
                            return new Response('Hello World!');
                        },
                    };`,
                compatibilityDate: "2025-06-07",
                compatibilityFlags: [],
                mainModule: "index.js",
                observability: { enabled: true },
                bindings: [
                    "BOT_TOKEN",
                    "HCLOUD_TOKEN",
                    "SELECTEL_PASS",
                    "WG_PRIVATE",
                    "WG_PSK",
                    "AFRAID_V4",
                    "AFRAID_V6",
                ].map((s) => ({
                    type: "secret_text",
                    name: s,
                    text: process.env[s],
                })),
            },
            { parent: this, ignoreChanges: ["content"] },
        );

        this.tgbotDomain = new cloudflare.WorkersCustomDomain(
            `tgbot`,
            {
                accountId: accountId,
                environment: "production",
                hostname: pulumi.interpolate`tgbot.${zone.name}`,
                service: tgbotWorkersScript.scriptName,
                zoneId: zone.id,
            },
            { parent: this },
        );

        this.registerOutputs({ tgbotDomain: this.tgbotDomain });
    }
}
