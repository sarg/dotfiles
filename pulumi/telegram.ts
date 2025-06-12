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
            "diogenisbot",
            {
                url: pulumi.interpolate`https://${args.cloudflare.tgbotDomain.hostname}/hook`,
                maxConnections: 1,
            },
            { parent: this },
        );

        new telegram.BotCommands(
            "diogenisbot",
            {
                commands: [
                    {
                        command: "vpn",
                        description: "start vpn",
                    },
                    {
                        command: "setup",
                        description: "generate notification link",
                    },
                ],
            },
            { parent: this },
        );
    }
}
