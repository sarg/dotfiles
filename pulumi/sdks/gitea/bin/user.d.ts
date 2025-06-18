import * as pulumi from "@pulumi/pulumi";
export declare class User extends pulumi.CustomResource {
    /**
     * Get an existing User resource's state with the given name, ID, and optional extra
     * properties used to qualify the lookup.
     *
     * @param name The _unique_ name of the resulting resource.
     * @param id The _unique_ provider ID of the resource to lookup.
     * @param state Any extra arguments used during the lookup.
     * @param opts Optional settings to control the behavior of the CustomResource.
     */
    static get(name: string, id: pulumi.Input<pulumi.ID>, state?: UserState, opts?: pulumi.CustomResourceOptions): User;
    /**
     * Returns true if the given object is an instance of User.  This is designed to work even
     * when multiple copies of the Pulumi SDK have been loaded into the same process.
     */
    static isInstance(obj: any): obj is User;
    /**
     * Flag if this user should be active or not
     */
    readonly active: pulumi.Output<boolean | undefined>;
    /**
     * Flag if this user should be an administrator or not
     */
    readonly admin: pulumi.Output<boolean | undefined>;
    readonly allowCreateOrganization: pulumi.Output<boolean | undefined>;
    readonly allowGitHook: pulumi.Output<boolean | undefined>;
    readonly allowImportLocal: pulumi.Output<boolean | undefined>;
    /**
     * A description of the user
     */
    readonly description: pulumi.Output<string | undefined>;
    /**
     * E-Mail Address of the user
     */
    readonly email: pulumi.Output<string>;
    /**
     * Flag if the user defined password should be overwritten or not
     */
    readonly forcePasswordChange: pulumi.Output<boolean | undefined>;
    /**
     * Full name of the user
     */
    readonly fullName: pulumi.Output<string>;
    readonly location: pulumi.Output<string | undefined>;
    /**
     * The login name can differ from the username
     */
    readonly loginName: pulumi.Output<string>;
    readonly maxRepoCreation: pulumi.Output<number | undefined>;
    /**
     * Flag if the user should change the password after first login
     */
    readonly mustChangePassword: pulumi.Output<boolean | undefined>;
    /**
     * Password to be set for the user
     */
    readonly password: pulumi.Output<string>;
    /**
     * Flag if the user should not be allowed to log in (bot user)
     */
    readonly prohibitLogin: pulumi.Output<boolean | undefined>;
    readonly restricted: pulumi.Output<boolean | undefined>;
    /**
     * Flag to send a notification about the user creation to the defined `email`
     */
    readonly sendNotification: pulumi.Output<boolean | undefined>;
    readonly userId: pulumi.Output<string>;
    /**
     * Username of the user to be created
     */
    readonly username: pulumi.Output<string>;
    /**
     * Visibility of the user. Can be `public`, `limited` or `private`
     */
    readonly visibility: pulumi.Output<string | undefined>;
    /**
     * Create a User resource with the given unique name, arguments, and options.
     *
     * @param name The _unique_ name of the resource.
     * @param args The arguments to use to populate this resource's properties.
     * @param opts A bag of options that control this resource's behavior.
     */
    constructor(name: string, args: UserArgs, opts?: pulumi.CustomResourceOptions);
}
/**
 * Input properties used for looking up and filtering User resources.
 */
export interface UserState {
    /**
     * Flag if this user should be active or not
     */
    active?: pulumi.Input<boolean>;
    /**
     * Flag if this user should be an administrator or not
     */
    admin?: pulumi.Input<boolean>;
    allowCreateOrganization?: pulumi.Input<boolean>;
    allowGitHook?: pulumi.Input<boolean>;
    allowImportLocal?: pulumi.Input<boolean>;
    /**
     * A description of the user
     */
    description?: pulumi.Input<string>;
    /**
     * E-Mail Address of the user
     */
    email?: pulumi.Input<string>;
    /**
     * Flag if the user defined password should be overwritten or not
     */
    forcePasswordChange?: pulumi.Input<boolean>;
    /**
     * Full name of the user
     */
    fullName?: pulumi.Input<string>;
    location?: pulumi.Input<string>;
    /**
     * The login name can differ from the username
     */
    loginName?: pulumi.Input<string>;
    maxRepoCreation?: pulumi.Input<number>;
    /**
     * Flag if the user should change the password after first login
     */
    mustChangePassword?: pulumi.Input<boolean>;
    /**
     * Password to be set for the user
     */
    password?: pulumi.Input<string>;
    /**
     * Flag if the user should not be allowed to log in (bot user)
     */
    prohibitLogin?: pulumi.Input<boolean>;
    restricted?: pulumi.Input<boolean>;
    /**
     * Flag to send a notification about the user creation to the defined `email`
     */
    sendNotification?: pulumi.Input<boolean>;
    userId?: pulumi.Input<string>;
    /**
     * Username of the user to be created
     */
    username?: pulumi.Input<string>;
    /**
     * Visibility of the user. Can be `public`, `limited` or `private`
     */
    visibility?: pulumi.Input<string>;
}
/**
 * The set of arguments for constructing a User resource.
 */
export interface UserArgs {
    /**
     * Flag if this user should be active or not
     */
    active?: pulumi.Input<boolean>;
    /**
     * Flag if this user should be an administrator or not
     */
    admin?: pulumi.Input<boolean>;
    allowCreateOrganization?: pulumi.Input<boolean>;
    allowGitHook?: pulumi.Input<boolean>;
    allowImportLocal?: pulumi.Input<boolean>;
    /**
     * A description of the user
     */
    description?: pulumi.Input<string>;
    /**
     * E-Mail Address of the user
     */
    email: pulumi.Input<string>;
    /**
     * Flag if the user defined password should be overwritten or not
     */
    forcePasswordChange?: pulumi.Input<boolean>;
    /**
     * Full name of the user
     */
    fullName?: pulumi.Input<string>;
    location?: pulumi.Input<string>;
    /**
     * The login name can differ from the username
     */
    loginName: pulumi.Input<string>;
    maxRepoCreation?: pulumi.Input<number>;
    /**
     * Flag if the user should change the password after first login
     */
    mustChangePassword?: pulumi.Input<boolean>;
    /**
     * Password to be set for the user
     */
    password: pulumi.Input<string>;
    /**
     * Flag if the user should not be allowed to log in (bot user)
     */
    prohibitLogin?: pulumi.Input<boolean>;
    restricted?: pulumi.Input<boolean>;
    /**
     * Flag to send a notification about the user creation to the defined `email`
     */
    sendNotification?: pulumi.Input<boolean>;
    userId?: pulumi.Input<string>;
    /**
     * Username of the user to be created
     */
    username: pulumi.Input<string>;
    /**
     * Visibility of the user. Can be `public`, `limited` or `private`
     */
    visibility?: pulumi.Input<string>;
}
