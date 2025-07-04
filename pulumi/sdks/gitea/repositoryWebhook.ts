// *** WARNING: this file was generated by pulumi-language-nodejs. ***
// *** Do not edit by hand unless you're certain you know what you are doing! ***

import * as pulumi from "@pulumi/pulumi";
import * as utilities from "./utilities";

export class RepositoryWebhook extends pulumi.CustomResource {
    /**
     * Get an existing RepositoryWebhook resource's state with the given name, ID, and optional extra
     * properties used to qualify the lookup.
     *
     * @param name The _unique_ name of the resulting resource.
     * @param id The _unique_ provider ID of the resource to lookup.
     * @param state Any extra arguments used during the lookup.
     * @param opts Optional settings to control the behavior of the CustomResource.
     */
    public static get(name: string, id: pulumi.Input<pulumi.ID>, state?: RepositoryWebhookState, opts?: pulumi.CustomResourceOptions): RepositoryWebhook {
        return new RepositoryWebhook(name, <any>state, { ...opts, id: id });
    }

    /** @internal */
    public static readonly __pulumiType = 'gitea:index/repositoryWebhook:RepositoryWebhook';

    /**
     * Returns true if the given object is an instance of RepositoryWebhook.  This is designed to work even
     * when multiple copies of the Pulumi SDK have been loaded into the same process.
     */
    public static isInstance(obj: any): obj is RepositoryWebhook {
        if (obj === undefined || obj === null) {
            return false;
        }
        return obj['__pulumiType'] === RepositoryWebhook.__pulumiType;
    }

    /**
     * Set webhook to active, e.g. `true`
     */
    public readonly active!: pulumi.Output<boolean>;
    /**
     * Webhook authorization header
     */
    public readonly authorizationHeader!: pulumi.Output<string | undefined>;
    /**
     * Set branch filter on the webhook, e.g. `"*"`
     */
    public readonly branchFilter!: pulumi.Output<string>;
    /**
     * The content type of the payload. It can be `json`, or `form`
     */
    public readonly contentType!: pulumi.Output<string>;
    /**
     * Webhook creation timestamp
     */
    public /*out*/ readonly createdAt!: pulumi.Output<string>;
    /**
     * A list of events that will trigger the webhool, e.g. `["push"]`
     */
    public readonly events!: pulumi.Output<string[]>;
    /**
     * Repository name
     */
    public readonly name!: pulumi.Output<string>;
    public readonly repositoryWebhookId!: pulumi.Output<string>;
    /**
     * Webhook secret
     */
    public readonly secret!: pulumi.Output<string | undefined>;
    /**
     * Webhook type, e.g. `gitea`
     */
    public readonly type!: pulumi.Output<string>;
    /**
     * Target URL of the webhook
     */
    public readonly url!: pulumi.Output<string>;
    /**
     * User name or organization name
     */
    public readonly username!: pulumi.Output<string>;

    /**
     * Create a RepositoryWebhook resource with the given unique name, arguments, and options.
     *
     * @param name The _unique_ name of the resource.
     * @param args The arguments to use to populate this resource's properties.
     * @param opts A bag of options that control this resource's behavior.
     */
    constructor(name: string, args: RepositoryWebhookArgs, opts?: pulumi.CustomResourceOptions)
    constructor(name: string, argsOrState?: RepositoryWebhookArgs | RepositoryWebhookState, opts?: pulumi.CustomResourceOptions) {
        let resourceInputs: pulumi.Inputs = {};
        opts = opts || {};
        if (opts.id) {
            const state = argsOrState as RepositoryWebhookState | undefined;
            resourceInputs["active"] = state ? state.active : undefined;
            resourceInputs["authorizationHeader"] = state ? state.authorizationHeader : undefined;
            resourceInputs["branchFilter"] = state ? state.branchFilter : undefined;
            resourceInputs["contentType"] = state ? state.contentType : undefined;
            resourceInputs["createdAt"] = state ? state.createdAt : undefined;
            resourceInputs["events"] = state ? state.events : undefined;
            resourceInputs["name"] = state ? state.name : undefined;
            resourceInputs["repositoryWebhookId"] = state ? state.repositoryWebhookId : undefined;
            resourceInputs["secret"] = state ? state.secret : undefined;
            resourceInputs["type"] = state ? state.type : undefined;
            resourceInputs["url"] = state ? state.url : undefined;
            resourceInputs["username"] = state ? state.username : undefined;
        } else {
            const args = argsOrState as RepositoryWebhookArgs | undefined;
            if ((!args || args.active === undefined) && !opts.urn) {
                throw new Error("Missing required property 'active'");
            }
            if ((!args || args.branchFilter === undefined) && !opts.urn) {
                throw new Error("Missing required property 'branchFilter'");
            }
            if ((!args || args.contentType === undefined) && !opts.urn) {
                throw new Error("Missing required property 'contentType'");
            }
            if ((!args || args.events === undefined) && !opts.urn) {
                throw new Error("Missing required property 'events'");
            }
            if ((!args || args.type === undefined) && !opts.urn) {
                throw new Error("Missing required property 'type'");
            }
            if ((!args || args.url === undefined) && !opts.urn) {
                throw new Error("Missing required property 'url'");
            }
            if ((!args || args.username === undefined) && !opts.urn) {
                throw new Error("Missing required property 'username'");
            }
            resourceInputs["active"] = args ? args.active : undefined;
            resourceInputs["authorizationHeader"] = args ? args.authorizationHeader : undefined;
            resourceInputs["branchFilter"] = args ? args.branchFilter : undefined;
            resourceInputs["contentType"] = args ? args.contentType : undefined;
            resourceInputs["events"] = args ? args.events : undefined;
            resourceInputs["name"] = args ? args.name : undefined;
            resourceInputs["repositoryWebhookId"] = args ? args.repositoryWebhookId : undefined;
            resourceInputs["secret"] = args ? args.secret : undefined;
            resourceInputs["type"] = args ? args.type : undefined;
            resourceInputs["url"] = args ? args.url : undefined;
            resourceInputs["username"] = args ? args.username : undefined;
            resourceInputs["createdAt"] = undefined /*out*/;
        }
        opts = pulumi.mergeOptions(utilities.resourceOptsDefaults(), opts);
        super(RepositoryWebhook.__pulumiType, name, resourceInputs, opts, false /*dependency*/, utilities.getPackage());
    }
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
