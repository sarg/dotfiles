import * as pulumi from "@pulumi/pulumi";
/**
 * The provider type for the gitea package. By default, resources use package-wide configuration
 * settings, however an explicit `Provider` instance may be created and passed during resource
 * construction to achieve fine-grained programmatic control over provider settings. See the
 * [documentation](https://www.pulumi.com/docs/reference/programming-model/#providers) for more information.
 */
export declare class Provider extends pulumi.ProviderResource {
    /**
     * Returns true if the given object is an instance of Provider.  This is designed to work even
     * when multiple copies of the Pulumi SDK have been loaded into the same process.
     */
    static isInstance(obj: any): obj is Provider;
    /**
     * The Gitea Base API URL
     */
    readonly baseUrl: pulumi.Output<string | undefined>;
    /**
     * A file containing the ca certificate to use in case ssl certificate is not from a standard chain
     */
    readonly cacertFile: pulumi.Output<string | undefined>;
    /**
     * Password in case of using basic auth
     */
    readonly password: pulumi.Output<string | undefined>;
    /**
     * The application token used to connect to Gitea.
     */
    readonly token: pulumi.Output<string | undefined>;
    /**
     * Username in case of using basic auth
     */
    readonly username: pulumi.Output<string | undefined>;
    /**
     * Create a Provider resource with the given unique name, arguments, and options.
     *
     * @param name The _unique_ name of the resource.
     * @param args The arguments to use to populate this resource's properties.
     * @param opts A bag of options that control this resource's behavior.
     */
    constructor(name: string, args?: ProviderArgs, opts?: pulumi.ResourceOptions);
}
/**
 * The set of arguments for constructing a Provider resource.
 */
export interface ProviderArgs {
    /**
     * The Gitea Base API URL
     */
    baseUrl?: pulumi.Input<string>;
    /**
     * A file containing the ca certificate to use in case ssl certificate is not from a standard chain
     */
    cacertFile?: pulumi.Input<string>;
    /**
     * Disable SSL verification of API calls
     */
    insecure?: pulumi.Input<boolean>;
    /**
     * Password in case of using basic auth
     */
    password?: pulumi.Input<string>;
    /**
     * The application token used to connect to Gitea.
     */
    token?: pulumi.Input<string>;
    /**
     * Username in case of using basic auth
     */
    username?: pulumi.Input<string>;
}
