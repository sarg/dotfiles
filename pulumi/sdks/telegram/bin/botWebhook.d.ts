import * as pulumi from "@pulumi/pulumi";
export declare class BotWebhook extends pulumi.CustomResource {
    /**
     * Get an existing BotWebhook resource's state with the given name, ID, and optional extra
     * properties used to qualify the lookup.
     *
     * @param name The _unique_ name of the resulting resource.
     * @param id The _unique_ provider ID of the resource to lookup.
     * @param state Any extra arguments used during the lookup.
     * @param opts Optional settings to control the behavior of the CustomResource.
     */
    static get(name: string, id: pulumi.Input<pulumi.ID>, state?: BotWebhookState, opts?: pulumi.CustomResourceOptions): BotWebhook;
    /**
     * Returns true if the given object is an instance of BotWebhook.  This is designed to work even
     * when multiple copies of the Pulumi SDK have been loaded into the same process.
     */
    static isInstance(obj: any): obj is BotWebhook;
    readonly allowedUpdates: pulumi.Output<string[] | undefined>;
    readonly botWebhookId: pulumi.Output<string>;
    readonly maxConnections: pulumi.Output<number | undefined>;
    readonly url: pulumi.Output<string>;
    /**
     * Create a BotWebhook resource with the given unique name, arguments, and options.
     *
     * @param name The _unique_ name of the resource.
     * @param args The arguments to use to populate this resource's properties.
     * @param opts A bag of options that control this resource's behavior.
     */
    constructor(name: string, args: BotWebhookArgs, opts?: pulumi.CustomResourceOptions);
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
