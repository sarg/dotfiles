import * as pulumi from "@pulumi/pulumi";
export declare class RepositoryActionsVariable extends pulumi.CustomResource {
    /**
     * Get an existing RepositoryActionsVariable resource's state with the given name, ID, and optional extra
     * properties used to qualify the lookup.
     *
     * @param name The _unique_ name of the resulting resource.
     * @param id The _unique_ provider ID of the resource to lookup.
     * @param state Any extra arguments used during the lookup.
     * @param opts Optional settings to control the behavior of the CustomResource.
     */
    static get(name: string, id: pulumi.Input<pulumi.ID>, state?: RepositoryActionsVariableState, opts?: pulumi.CustomResourceOptions): RepositoryActionsVariable;
    /**
     * Returns true if the given object is an instance of RepositoryActionsVariable.  This is designed to work even
     * when multiple copies of the Pulumi SDK have been loaded into the same process.
     */
    static isInstance(obj: any): obj is RepositoryActionsVariable;
    /**
     * Name of the repository.
     */
    readonly repository: pulumi.Output<string>;
    readonly repositoryActionsVariableId: pulumi.Output<string>;
    /**
     * Owner of the repository.
     */
    readonly repositoryOwner: pulumi.Output<string>;
    /**
     * Value of the variable.
     */
    readonly value: pulumi.Output<string>;
    /**
     * Name of the variable.
     */
    readonly variableName: pulumi.Output<string>;
    /**
     * Create a RepositoryActionsVariable resource with the given unique name, arguments, and options.
     *
     * @param name The _unique_ name of the resource.
     * @param args The arguments to use to populate this resource's properties.
     * @param opts A bag of options that control this resource's behavior.
     */
    constructor(name: string, args: RepositoryActionsVariableArgs, opts?: pulumi.CustomResourceOptions);
}
/**
 * Input properties used for looking up and filtering RepositoryActionsVariable resources.
 */
export interface RepositoryActionsVariableState {
    /**
     * Name of the repository.
     */
    repository?: pulumi.Input<string>;
    repositoryActionsVariableId?: pulumi.Input<string>;
    /**
     * Owner of the repository.
     */
    repositoryOwner?: pulumi.Input<string>;
    /**
     * Value of the variable.
     */
    value?: pulumi.Input<string>;
    /**
     * Name of the variable.
     */
    variableName?: pulumi.Input<string>;
}
/**
 * The set of arguments for constructing a RepositoryActionsVariable resource.
 */
export interface RepositoryActionsVariableArgs {
    /**
     * Name of the repository.
     */
    repository: pulumi.Input<string>;
    repositoryActionsVariableId?: pulumi.Input<string>;
    /**
     * Owner of the repository.
     */
    repositoryOwner: pulumi.Input<string>;
    /**
     * Value of the variable.
     */
    value: pulumi.Input<string>;
    /**
     * Name of the variable.
     */
    variableName: pulumi.Input<string>;
}
