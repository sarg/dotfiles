import * as pulumi from "@pulumi/pulumi";
export declare class Token extends pulumi.CustomResource {
    /**
     * Get an existing Token resource's state with the given name, ID, and optional extra
     * properties used to qualify the lookup.
     *
     * @param name The _unique_ name of the resulting resource.
     * @param id The _unique_ provider ID of the resource to lookup.
     * @param state Any extra arguments used during the lookup.
     * @param opts Optional settings to control the behavior of the CustomResource.
     */
    static get(name: string, id: pulumi.Input<pulumi.ID>, state?: TokenState, opts?: pulumi.CustomResourceOptions): Token;
    /**
     * Returns true if the given object is an instance of Token.  This is designed to work even
     * when multiple copies of the Pulumi SDK have been loaded into the same process.
     */
    static isInstance(obj: any): obj is Token;
    readonly lastEight: pulumi.Output<string>;
    /**
     * The name of the Access Token
     */
    readonly name: pulumi.Output<string>;
    /**
     * List of string representations of scopes for the token
     */
    readonly scopes: pulumi.Output<string[]>;
    /**
     * The actual Access Token
     */
    readonly token: pulumi.Output<string>;
    readonly tokenId: pulumi.Output<string>;
    /**
     * Create a Token resource with the given unique name, arguments, and options.
     *
     * @param name The _unique_ name of the resource.
     * @param args The arguments to use to populate this resource's properties.
     * @param opts A bag of options that control this resource's behavior.
     */
    constructor(name: string, args: TokenArgs, opts?: pulumi.CustomResourceOptions);
}
/**
 * Input properties used for looking up and filtering Token resources.
 */
export interface TokenState {
    lastEight?: pulumi.Input<string>;
    /**
     * The name of the Access Token
     */
    name?: pulumi.Input<string>;
    /**
     * List of string representations of scopes for the token
     */
    scopes?: pulumi.Input<pulumi.Input<string>[]>;
    /**
     * The actual Access Token
     */
    token?: pulumi.Input<string>;
    tokenId?: pulumi.Input<string>;
}
/**
 * The set of arguments for constructing a Token resource.
 */
export interface TokenArgs {
    /**
     * The name of the Access Token
     */
    name?: pulumi.Input<string>;
    /**
     * List of string representations of scopes for the token
     */
    scopes: pulumi.Input<pulumi.Input<string>[]>;
    tokenId?: pulumi.Input<string>;
}
