import * as pulumi from "@pulumi/pulumi";
import * as telegram from "@pulumi/telegram";
import { Cloudflare } from "./cloudflare";

export class Telegram extends pulumi.ComponentResource {
    constructor(
        name: string,
        args: { cloudflare: Cloudflare },
        opts?: pulumi.ComponentResourceOptions,
    ) {
        super("components:index:Telegram", name, args, opts);

        new telegram.BotWebhook(
            "sargebutlerbot",
            {
                url: pulumi.interpolate`https://${args.cloudflare.tgbotDomain.hostname}`,
                maxConnections: 1,
            },
            { parent: this },
        );
    }
}
