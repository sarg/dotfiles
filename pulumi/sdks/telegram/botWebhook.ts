// *** WARNING: this file was generated by pulumi-language-nodejs. ***
// *** Do not edit by hand unless you're certain you know what you are doing! ***

import * as pulumi from "@pulumi/pulumi";
import * as utilities from "./utilities";

export class BotWebhook extends pulumi.CustomResource {
    /**
     * Get an existing BotWebhook resource's state with the given name, ID, and optional extra
     * properties used to qualify the lookup.
     *
     * @param name The _unique_ name of the resulting resource.
     * @param id The _unique_ provider ID of the resource to lookup.
     * @param state Any extra arguments used during the lookup.
     * @param opts Optional settings to control the behavior of the CustomResource.
     */
    public static get(name: string, id: pulumi.Input<pulumi.ID>, state?: BotWebhookState, opts?: pulumi.CustomResourceOptions): BotWebhook {
        return new BotWebhook(name, <any>state, { ...opts, id: id });
    }

    /** @internal */
    public static readonly __pulumiType = 'telegram:index/botWebhook:BotWebhook';

    /**
     * Returns true if the given object is an instance of BotWebhook.  This is designed to work even
     * when multiple copies of the Pulumi SDK have been loaded into the same process.
     */
    public static isInstance(obj: any): obj is BotWebhook {
        if (obj === undefined || obj === null) {
            return false;
        }
        return obj['__pulumiType'] === BotWebhook.__pulumiType;
    }

    public readonly allowedUpdates!: pulumi.Output<string[] | undefined>;
    public readonly botWebhookId!: pulumi.Output<string>;
    public readonly maxConnections!: pulumi.Output<number | undefined>;
    public readonly url!: pulumi.Output<string>;

    /**
     * Create a BotWebhook resource with the given unique name, arguments, and options.
     *
     * @param name The _unique_ name of the resource.
     * @param args The arguments to use to populate this resource's properties.
     * @param opts A bag of options that control this resource's behavior.
     */
    constructor(name: string, args: BotWebhookArgs, opts?: pulumi.CustomResourceOptions)
    constructor(name: string, argsOrState?: BotWebhookArgs | BotWebhookState, opts?: pulumi.CustomResourceOptions) {
        let resourceInputs: pulumi.Inputs = {};
        opts = opts || {};
        if (opts.id) {
            const state = argsOrState as BotWebhookState | undefined;
            resourceInputs["allowedUpdates"] = state ? state.allowedUpdates : undefined;
            resourceInputs["botWebhookId"] = state ? state.botWebhookId : undefined;
            resourceInputs["maxConnections"] = state ? state.maxConnections : undefined;
            resourceInputs["url"] = state ? state.url : undefined;
        } else {
            const args = argsOrState as BotWebhookArgs | undefined;
            if ((!args || args.url === undefined) && !opts.urn) {
                throw new Error("Missing required property 'url'");
            }
            resourceInputs["allowedUpdates"] = args ? args.allowedUpdates : undefined;
            resourceInputs["botWebhookId"] = args ? args.botWebhookId : undefined;
            resourceInputs["maxConnections"] = args ? args.maxConnections : undefined;
            resourceInputs["url"] = args ? args.url : undefined;
        }
        opts = pulumi.mergeOptions(utilities.resourceOptsDefaults(), opts);
        super(BotWebhook.__pulumiType, name, resourceInputs, opts, false /*dependency*/, utilities.getPackage());
    }
}

/**
 * Input properties used for looking up and filtering BotWebhook resources.
 */
export interface BotWebhookState {
    allowedUpdates?: pulumi.Input<pulumi.Input<string>[]>;
    botWebhookId?: pulumi.Input<string>;
    maxConnections?: pulumi.Input<number>;
    url?: pulumi.Input<string>;
}

/**
 * The set of arguments for constructing a BotWebhook resource.
 */
export interface BotWebhookArgs {
    allowedUpdates?: pulumi.Input<pulumi.Input<string>[]>;
    botWebhookId?: pulumi.Input<string>;
    maxConnections?: pulumi.Input<number>;
    url: pulumi.Input<string>;
}
