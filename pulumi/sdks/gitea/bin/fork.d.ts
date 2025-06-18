import * as pulumi from "@pulumi/pulumi";
export declare class Fork extends pulumi.CustomResource {
    /**
     * Get an existing Fork resource's state with the given name, ID, and optional extra
     * properties used to qualify the lookup.
     *
     * @param name The _unique_ name of the resulting resource.
     * @param id The _unique_ provider ID of the resource to lookup.
     * @param state Any extra arguments used during the lookup.
     * @param opts Optional settings to control the behavior of the CustomResource.
     */
    static get(name: string, id: pulumi.Input<pulumi.ID>, state?: ForkState, opts?: pulumi.CustomResourceOptions): Fork;
    /**
     * Returns true if the given object is an instance of Fork.  This is designed to work even
     * when multiple copies of the Pulumi SDK have been loaded into the same process.
     */
    static isInstance(obj: any): obj is Fork;
    readonly forkId: pulumi.Output<string>;
    /**
     * The organization that owns the forked repo
     */
    readonly organization: pulumi.Output<string | undefined>;
    /**
     * The owner or owning organization of the repository to fork
     */
    readonly owner: pulumi.Output<string>;
    /**
     * The name of the repository to fork
     */
    readonly repo: pulumi.Output<string>;
    /**
     * Create a Fork resource with the given unique name, arguments, and options.
     *
     * @param name The _unique_ name of the resource.
     * @param args The arguments to use to populate this resource's properties.
     * @param opts A bag of options that control this resource's behavior.
     */
    constructor(name: string, args: ForkArgs, opts?: pulumi.CustomResourceOptions);
}
/**
 * Input properties used for looking up and filtering Fork resources.
 */
export interface ForkState {
    forkId?: pulumi.Input<string>;
    /**
     * The organization that owns the forked repo
     */
    organization?: pulumi.Input<string>;
    /**
     * The owner or owning organization of the repository to fork
     */
    owner?: pulumi.Input<string>;
    /**
     * The name of the repository to fork
     */
    repo?: pulumi.Input<string>;
}
/**
 * The set of arguments for constructing a Fork resource.
 */
export interface ForkArgs {
    forkId?: pulumi.Input<string>;
    /**
     * The organization that owns the forked repo
     */
    organization?: pulumi.Input<string>;
    /**
     * The owner or owning organization of the repository to fork
     */
    owner: pulumi.Input<string>;
    /**
     * The name of the repository to fork
     */
    repo: pulumi.Input<string>;
}
