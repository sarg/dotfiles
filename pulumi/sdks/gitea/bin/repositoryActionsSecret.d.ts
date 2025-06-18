import * as pulumi from "@pulumi/pulumi";
export declare class RepositoryActionsSecret extends pulumi.CustomResource {
    /**
     * Get an existing RepositoryActionsSecret resource's state with the given name, ID, and optional extra
     * properties used to qualify the lookup.
     *
     * @param name The _unique_ name of the resulting resource.
     * @param id The _unique_ provider ID of the resource to lookup.
     * @param state Any extra arguments used during the lookup.
     * @param opts Optional settings to control the behavior of the CustomResource.
     */
    static get(name: string, id: pulumi.Input<pulumi.ID>, state?: RepositoryActionsSecretState, opts?: pulumi.CustomResourceOptions): RepositoryActionsSecret;
    /**
     * Returns true if the given object is an instance of RepositoryActionsSecret.  This is designed to work even
     * when multiple copies of the Pulumi SDK have been loaded into the same process.
     */
    static isInstance(obj: any): obj is RepositoryActionsSecret;
    /**
     * Date of 'actions_secret' creation.
     */
    readonly createdAt: pulumi.Output<string>;
    /**
     * Name of the repository.
     */
    readonly repository: pulumi.Output<string>;
    readonly repositoryActionsSecretId: pulumi.Output<string>;
    /**
     * Owner of the repository.
     */
    readonly repositoryOwner: pulumi.Output<string>;
    /**
     * Name of the secret.
     */
    readonly secretName: pulumi.Output<string>;
    /**
     * Value of the secret.
     */
    readonly secretValue: pulumi.Output<string>;
    /**
     * Create a RepositoryActionsSecret resource with the given unique name, arguments, and options.
     *
     * @param name The _unique_ name of the resource.
     * @param args The arguments to use to populate this resource's properties.
     * @param opts A bag of options that control this resource's behavior.
     */
    constructor(name: string, args: RepositoryActionsSecretArgs, opts?: pulumi.CustomResourceOptions);
}
/**
 * Input properties used for looking up and filtering RepositoryActionsSecret resources.
 */
export interface RepositoryActionsSecretState {
    /**
     * Date of 'actions_secret' creation.
     */
    createdAt?: pulumi.Input<string>;
    /**
     * Name of the repository.
     */
    repository?: pulumi.Input<string>;
    repositoryActionsSecretId?: pulumi.Input<string>;
    /**
     * Owner of the repository.
     */
    repositoryOwner?: pulumi.Input<string>;
    /**
     * Name of the secret.
     */
    secretName?: pulumi.Input<string>;
    /**
     * Value of the secret.
     */
    secretValue?: pulumi.Input<string>;
}
/**
 * The set of arguments for constructing a RepositoryActionsSecret resource.
 */
export interface RepositoryActionsSecretArgs {
    /**
     * Name of the repository.
     */
    repository: pulumi.Input<string>;
    repositoryActionsSecretId?: pulumi.Input<string>;
    /**
     * Owner of the repository.
     */
    repositoryOwner: pulumi.Input<string>;
    /**
     * Name of the secret.
     */
    secretName: pulumi.Input<string>;
    /**
     * Value of the secret.
     */
    secretValue: pulumi.Input<string>;
}
