import * as pulumi from "@pulumi/pulumi";
export declare class RepositoryWebhook extends pulumi.CustomResource {
    /**
     * Get an existing RepositoryWebhook resource's state with the given name, ID, and optional extra
     * properties used to qualify the lookup.
     *
     * @param name The _unique_ name of the resulting resource.
     * @param id The _unique_ provider ID of the resource to lookup.
     * @param state Any extra arguments used during the lookup.
     * @param opts Optional settings to control the behavior of the CustomResource.
     */
    static get(name: string, id: pulumi.Input<pulumi.ID>, state?: RepositoryWebhookState, opts?: pulumi.CustomResourceOptions): RepositoryWebhook;
    /**
     * Returns true if the given object is an instance of RepositoryWebhook.  This is designed to work even
     * when multiple copies of the Pulumi SDK have been loaded into the same process.
     */
    static isInstance(obj: any): obj is RepositoryWebhook;
    /**
     * Set webhook to active, e.g. `true`
     */
    readonly active: pulumi.Output<boolean>;
    /**
     * Webhook authorization header
     */
    readonly authorizationHeader: pulumi.Output<string | undefined>;
    /**
     * Set branch filter on the webhook, e.g. `"*"`
     */
    readonly branchFilter: pulumi.Output<string>;
    /**
     * The content type of the payload. It can be `json`, or `form`
     */
    readonly contentType: pulumi.Output<string>;
    /**
     * Webhook creation timestamp
     */
    readonly createdAt: pulumi.Output<string>;
    /**
     * A list of events that will trigger the webhool, e.g. `["push"]`
     */
    readonly events: pulumi.Output<string[]>;
    /**
     * Repository name
     */
    readonly name: pulumi.Output<string>;
    readonly repositoryWebhookId: pulumi.Output<string>;
    /**
     * Webhook secret
     */
    readonly secret: pulumi.Output<string | undefined>;
    /**
     * Webhook type, e.g. `gitea`
     */
    readonly type: pulumi.Output<string>;
    /**
     * Target URL of the webhook
     */
    readonly url: pulumi.Output<string>;
    /**
     * User name or organization name
     */
    readonly username: pulumi.Output<string>;
    /**
     * Create a RepositoryWebhook resource with the given unique name, arguments, and options.
     *
     * @param name The _unique_ name of the resource.
     * @param args The arguments to use to populate this resource's properties.
     * @param opts A bag of options that control this resource's behavior.
     */
    constructor(name: string, args: RepositoryWebhookArgs, opts?: pulumi.CustomResourceOptions);
}
/**
 * Input properties used for looking up and filtering RepositoryWebhook resources.
 */
export interface RepositoryWebhookState {
    /**
     * Set webhook to active, e.g. `true`
     */
    active?: pulumi.Input<boolean>;
    /**
     * Webhook authorization header
     */
    authorizationHeader?: pulumi.Input<string>;
    /**
     * Set branch filter on the webhook, e.g. `"*"`
     */
    branchFilter?: pulumi.Input<string>;
    /**
     * The content type of the payload. It can be `json`, or `form`
     */
    contentType?: pulumi.Input<string>;
    /**
     * Webhook creation timestamp
     */
    createdAt?: pulumi.Input<string>;
    /**
     * A list of events that will trigger the webhool, e.g. `["push"]`
     */
    events?: pulumi.Input<pulumi.Input<string>[]>;
    /**
     * Repository name
     */
    name?: pulumi.Input<string>;
    repositoryWebhookId?: pulumi.Input<string>;
    /**
     * Webhook secret
     */
    secret?: pulumi.Input<string>;
    /**
     * Webhook type, e.g. `gitea`
     */
    type?: pulumi.Input<string>;
    /**
     * Target URL of the webhook
     */
    url?: pulumi.Input<string>;
    /**
     * User name or organization name
     */
    username?: pulumi.Input<string>;
}
/**
 * The set of arguments for constructing a RepositoryWebhook resource.
 */
export interface RepositoryWebhookArgs {
    /**
     * Set webhook to active, e.g. `true`
     */
    active: pulumi.Input<boolean>;
    /**
     * Webhook authorization header
     */
    authorizationHeader?: pulumi.Input<string>;
    /**
     * Set branch filter on the webhook, e.g. `"*"`
     */
    branchFilter: pulumi.Input<string>;
    /**
     * The content type of the payload. It can be `json`, or `form`
     */
    contentType: pulumi.Input<string>;
    /**
     * A list of events that will trigger the webhool, e.g. `["push"]`
     */
    events: pulumi.Input<pulumi.Input<string>[]>;
    /**
     * Repository name
     */
    name?: pulumi.Input<string>;
    repositoryWebhookId?: pulumi.Input<string>;
    /**
     * Webhook secret
     */
    secret?: pulumi.Input<string>;
    /**
     * Webhook type, e.g. `gitea`
     */
    type: pulumi.Input<string>;
    /**
     * Target URL of the webhook
     */
    url: pulumi.Input<string>;
    /**
     * User name or organization name
     */
    username: pulumi.Input<string>;
}
