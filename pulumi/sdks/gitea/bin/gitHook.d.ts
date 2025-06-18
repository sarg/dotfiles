import * as pulumi from "@pulumi/pulumi";
export declare class GitHook extends pulumi.CustomResource {
    /**
     * Get an existing GitHook resource's state with the given name, ID, and optional extra
     * properties used to qualify the lookup.
     *
     * @param name The _unique_ name of the resulting resource.
     * @param id The _unique_ provider ID of the resource to lookup.
     * @param state Any extra arguments used during the lookup.
     * @param opts Optional settings to control the behavior of the CustomResource.
     */
    static get(name: string, id: pulumi.Input<pulumi.ID>, state?: GitHookState, opts?: pulumi.CustomResourceOptions): GitHook;
    /**
     * Returns true if the given object is an instance of GitHook.  This is designed to work even
     * when multiple copies of the Pulumi SDK have been loaded into the same process.
     */
    static isInstance(obj: any): obj is GitHook;
    /**
     * Content of the git hook
     */
    readonly content: pulumi.Output<string>;
    readonly gitHookId: pulumi.Output<string>;
    /**
     * Name of the git hook to configure
     */
    readonly name: pulumi.Output<string>;
    /**
     * The repository that this hook belongs too.
     */
    readonly repo: pulumi.Output<string>;
    /**
     * The user (or organisation) owning the repo this hook belongs too
     */
    readonly user: pulumi.Output<string>;
    /**
     * Create a GitHook resource with the given unique name, arguments, and options.
     *
     * @param name The _unique_ name of the resource.
     * @param args The arguments to use to populate this resource's properties.
     * @param opts A bag of options that control this resource's behavior.
     */
    constructor(name: string, args: GitHookArgs, opts?: pulumi.CustomResourceOptions);
}
/**
 * Input properties used for looking up and filtering GitHook resources.
 */
export interface GitHookState {
    /**
     * Content of the git hook
     */
    content?: pulumi.Input<string>;
    gitHookId?: pulumi.Input<string>;
    /**
     * Name of the git hook to configure
     */
    name?: pulumi.Input<string>;
    /**
     * The repository that this hook belongs too.
     */
    repo?: pulumi.Input<string>;
    /**
     * The user (or organisation) owning the repo this hook belongs too
     */
    user?: pulumi.Input<string>;
}
/**
 * The set of arguments for constructing a GitHook resource.
 */
export interface GitHookArgs {
    /**
     * Content of the git hook
     */
    content: pulumi.Input<string>;
    gitHookId?: pulumi.Input<string>;
    /**
     * Name of the git hook to configure
     */
    name?: pulumi.Input<string>;
    /**
     * The repository that this hook belongs too.
     */
    repo: pulumi.Input<string>;
    /**
     * The user (or organisation) owning the repo this hook belongs too
     */
    user: pulumi.Input<string>;
}
