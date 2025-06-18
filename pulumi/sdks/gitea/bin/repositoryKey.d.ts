import * as pulumi from "@pulumi/pulumi";
export declare class RepositoryKey extends pulumi.CustomResource {
    /**
     * Get an existing RepositoryKey resource's state with the given name, ID, and optional extra
     * properties used to qualify the lookup.
     *
     * @param name The _unique_ name of the resulting resource.
     * @param id The _unique_ provider ID of the resource to lookup.
     * @param state Any extra arguments used during the lookup.
     * @param opts Optional settings to control the behavior of the CustomResource.
     */
    static get(name: string, id: pulumi.Input<pulumi.ID>, state?: RepositoryKeyState, opts?: pulumi.CustomResourceOptions): RepositoryKey;
    /**
     * Returns true if the given object is an instance of RepositoryKey.  This is designed to work even
     * when multiple copies of the Pulumi SDK have been loaded into the same process.
     */
    static isInstance(obj: any): obj is RepositoryKey;
    /**
     * Armored SSH key to add
     */
    readonly key: pulumi.Output<string>;
    /**
     * Whether this key has read or read/write access
     */
    readonly readOnly: pulumi.Output<boolean | undefined>;
    /**
     * The ID of the repository where the deploy key belongs to
     */
    readonly repository: pulumi.Output<number>;
    readonly repositoryKeyId: pulumi.Output<string>;
    /**
     * Name of the deploy key
     */
    readonly title: pulumi.Output<string>;
    /**
     * Create a RepositoryKey resource with the given unique name, arguments, and options.
     *
     * @param name The _unique_ name of the resource.
     * @param args The arguments to use to populate this resource's properties.
     * @param opts A bag of options that control this resource's behavior.
     */
    constructor(name: string, args: RepositoryKeyArgs, opts?: pulumi.CustomResourceOptions);
}
/**
 * Input properties used for looking up and filtering RepositoryKey resources.
 */
export interface RepositoryKeyState {
    /**
     * Armored SSH key to add
     */
    key?: pulumi.Input<string>;
    /**
     * Whether this key has read or read/write access
     */
    readOnly?: pulumi.Input<boolean>;
    /**
     * The ID of the repository where the deploy key belongs to
     */
    repository?: pulumi.Input<number>;
    repositoryKeyId?: pulumi.Input<string>;
    /**
     * Name of the deploy key
     */
    title?: pulumi.Input<string>;
}
/**
 * The set of arguments for constructing a RepositoryKey resource.
 */
export interface RepositoryKeyArgs {
    /**
     * Armored SSH key to add
     */
    key: pulumi.Input<string>;
    /**
     * Whether this key has read or read/write access
     */
    readOnly?: pulumi.Input<boolean>;
    /**
     * The ID of the repository where the deploy key belongs to
     */
    repository: pulumi.Input<number>;
    repositoryKeyId?: pulumi.Input<string>;
    /**
     * Name of the deploy key
     */
    title: pulumi.Input<string>;
}
