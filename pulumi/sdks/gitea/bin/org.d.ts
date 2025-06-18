import * as pulumi from "@pulumi/pulumi";
export declare class Org extends pulumi.CustomResource {
    /**
     * Get an existing Org resource's state with the given name, ID, and optional extra
     * properties used to qualify the lookup.
     *
     * @param name The _unique_ name of the resulting resource.
     * @param id The _unique_ provider ID of the resource to lookup.
     * @param state Any extra arguments used during the lookup.
     * @param opts Optional settings to control the behavior of the CustomResource.
     */
    static get(name: string, id: pulumi.Input<pulumi.ID>, state?: OrgState, opts?: pulumi.CustomResourceOptions): Org;
    /**
     * Returns true if the given object is an instance of Org.  This is designed to work even
     * when multiple copies of the Pulumi SDK have been loaded into the same process.
     */
    static isInstance(obj: any): obj is Org;
    readonly avatarUrl: pulumi.Output<string>;
    /**
     * A description of this organisation.
     */
    readonly description: pulumi.Output<string | undefined>;
    /**
     * The display name of the organisation. Defaults to the value of `name`.
     */
    readonly fullName: pulumi.Output<string | undefined>;
    readonly location: pulumi.Output<string | undefined>;
    /**
     * The name of the organisation without spaces.
     */
    readonly name: pulumi.Output<string>;
    readonly orgId: pulumi.Output<string>;
    readonly repoAdminChangeTeamAccess: pulumi.Output<boolean | undefined>;
    /**
     * List of all Repositories that are part of this organisation
     */
    readonly repos: pulumi.Output<string[]>;
    /**
     * Flag is this organisation should be publicly visible or not.
     */
    readonly visibility: pulumi.Output<string | undefined>;
    /**
     * A link to a website with more information about this organisation.
     */
    readonly website: pulumi.Output<string | undefined>;
    /**
     * Create a Org resource with the given unique name, arguments, and options.
     *
     * @param name The _unique_ name of the resource.
     * @param args The arguments to use to populate this resource's properties.
     * @param opts A bag of options that control this resource's behavior.
     */
    constructor(name: string, args?: OrgArgs, opts?: pulumi.CustomResourceOptions);
}
/**
 * Input properties used for looking up and filtering Org resources.
 */
export interface OrgState {
    avatarUrl?: pulumi.Input<string>;
    /**
     * A description of this organisation.
     */
    description?: pulumi.Input<string>;
    /**
     * The display name of the organisation. Defaults to the value of `name`.
     */
    fullName?: pulumi.Input<string>;
    location?: pulumi.Input<string>;
    /**
     * The name of the organisation without spaces.
     */
    name?: pulumi.Input<string>;
    orgId?: pulumi.Input<string>;
    repoAdminChangeTeamAccess?: pulumi.Input<boolean>;
    /**
     * List of all Repositories that are part of this organisation
     */
    repos?: pulumi.Input<pulumi.Input<string>[]>;
    /**
     * Flag is this organisation should be publicly visible or not.
     */
    visibility?: pulumi.Input<string>;
    /**
     * A link to a website with more information about this organisation.
     */
    website?: pulumi.Input<string>;
}
/**
 * The set of arguments for constructing a Org resource.
 */
export interface OrgArgs {
    /**
     * A description of this organisation.
     */
    description?: pulumi.Input<string>;
    /**
     * The display name of the organisation. Defaults to the value of `name`.
     */
    fullName?: pulumi.Input<string>;
    location?: pulumi.Input<string>;
    /**
     * The name of the organisation without spaces.
     */
    name?: pulumi.Input<string>;
    orgId?: pulumi.Input<string>;
    repoAdminChangeTeamAccess?: pulumi.Input<boolean>;
    /**
     * Flag is this organisation should be publicly visible or not.
     */
    visibility?: pulumi.Input<string>;
    /**
     * A link to a website with more information about this organisation.
     */
    website?: pulumi.Input<string>;
}
