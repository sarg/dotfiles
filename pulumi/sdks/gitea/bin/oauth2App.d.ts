import * as pulumi from "@pulumi/pulumi";
export declare class Oauth2App extends pulumi.CustomResource {
    /**
     * Get an existing Oauth2App resource's state with the given name, ID, and optional extra
     * properties used to qualify the lookup.
     *
     * @param name The _unique_ name of the resulting resource.
     * @param id The _unique_ provider ID of the resource to lookup.
     * @param state Any extra arguments used during the lookup.
     * @param opts Optional settings to control the behavior of the CustomResource.
     */
    static get(name: string, id: pulumi.Input<pulumi.ID>, state?: Oauth2AppState, opts?: pulumi.CustomResourceOptions): Oauth2App;
    /**
     * Returns true if the given object is an instance of Oauth2App.  This is designed to work even
     * when multiple copies of the Pulumi SDK have been loaded into the same process.
     */
    static isInstance(obj: any): obj is Oauth2App;
    /**
     * OAuth2 Application client id
     */
    readonly clientId: pulumi.Output<string>;
    /**
     * Oauth2 Application client secret
     */
    readonly clientSecret: pulumi.Output<string>;
    /**
     * If set to false, it will be a public client (PKCE will be required)
     */
    readonly confidentialClient: pulumi.Output<boolean | undefined>;
    /**
     * OAuth Application name
     */
    readonly name: pulumi.Output<string>;
    readonly oauth2AppId: pulumi.Output<string>;
    /**
     * Accepted redirect URIs
     */
    readonly redirectUris: pulumi.Output<string[]>;
    /**
     * Create a Oauth2App resource with the given unique name, arguments, and options.
     *
     * @param name The _unique_ name of the resource.
     * @param args The arguments to use to populate this resource's properties.
     * @param opts A bag of options that control this resource's behavior.
     */
    constructor(name: string, args: Oauth2AppArgs, opts?: pulumi.CustomResourceOptions);
}
/**
 * Input properties used for looking up and filtering Oauth2App resources.
 */
export interface Oauth2AppState {
    /**
     * OAuth2 Application client id
     */
    clientId?: pulumi.Input<string>;
    /**
     * Oauth2 Application client secret
     */
    clientSecret?: pulumi.Input<string>;
    /**
     * If set to false, it will be a public client (PKCE will be required)
     */
    confidentialClient?: pulumi.Input<boolean>;
    /**
     * OAuth Application name
     */
    name?: pulumi.Input<string>;
    oauth2AppId?: pulumi.Input<string>;
    /**
     * Accepted redirect URIs
     */
    redirectUris?: pulumi.Input<pulumi.Input<string>[]>;
}
/**
 * The set of arguments for constructing a Oauth2App resource.
 */
export interface Oauth2AppArgs {
    /**
     * If set to false, it will be a public client (PKCE will be required)
     */
    confidentialClient?: pulumi.Input<boolean>;
    /**
     * OAuth Application name
     */
    name?: pulumi.Input<string>;
    oauth2AppId?: pulumi.Input<string>;
    /**
     * Accepted redirect URIs
     */
    redirectUris: pulumi.Input<pulumi.Input<string>[]>;
}
