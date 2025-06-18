import * as pulumi from "@pulumi/pulumi";
export declare class PublicKey extends pulumi.CustomResource {
    /**
     * Get an existing PublicKey resource's state with the given name, ID, and optional extra
     * properties used to qualify the lookup.
     *
     * @param name The _unique_ name of the resulting resource.
     * @param id The _unique_ provider ID of the resource to lookup.
     * @param state Any extra arguments used during the lookup.
     * @param opts Optional settings to control the behavior of the CustomResource.
     */
    static get(name: string, id: pulumi.Input<pulumi.ID>, state?: PublicKeyState, opts?: pulumi.CustomResourceOptions): PublicKey;
    /**
     * Returns true if the given object is an instance of PublicKey.  This is designed to work even
     * when multiple copies of the Pulumi SDK have been loaded into the same process.
     */
    static isInstance(obj: any): obj is PublicKey;
    readonly created: pulumi.Output<string>;
    readonly fingerprint: pulumi.Output<string>;
    /**
     * An armored SSH key to add
     */
    readonly key: pulumi.Output<string>;
    readonly publicKeyId: pulumi.Output<string>;
    /**
     * Describe if the key has only read access or read/write
     */
    readonly readOnly: pulumi.Output<boolean | undefined>;
    /**
     * Title of the key to add
     */
    readonly title: pulumi.Output<string>;
    readonly type: pulumi.Output<string>;
    /**
     * User to associate with the added key
     */
    readonly username: pulumi.Output<string>;
    /**
     * Create a PublicKey resource with the given unique name, arguments, and options.
     *
     * @param name The _unique_ name of the resource.
     * @param args The arguments to use to populate this resource's properties.
     * @param opts A bag of options that control this resource's behavior.
     */
    constructor(name: string, args: PublicKeyArgs, opts?: pulumi.CustomResourceOptions);
}
/**
 * Input properties used for looking up and filtering PublicKey resources.
 */
export interface PublicKeyState {
    created?: pulumi.Input<string>;
    fingerprint?: pulumi.Input<string>;
    /**
     * An armored SSH key to add
     */
    key?: pulumi.Input<string>;
    publicKeyId?: pulumi.Input<string>;
    /**
     * Describe if the key has only read access or read/write
     */
    readOnly?: pulumi.Input<boolean>;
    /**
     * Title of the key to add
     */
    title?: pulumi.Input<string>;
    type?: pulumi.Input<string>;
    /**
     * User to associate with the added key
     */
    username?: pulumi.Input<string>;
}
/**
 * The set of arguments for constructing a PublicKey resource.
 */
export interface PublicKeyArgs {
    /**
     * An armored SSH key to add
     */
    key: pulumi.Input<string>;
    publicKeyId?: pulumi.Input<string>;
    /**
     * Describe if the key has only read access or read/write
     */
    readOnly?: pulumi.Input<boolean>;
    /**
     * Title of the key to add
     */
    title: pulumi.Input<string>;
    /**
     * User to associate with the added key
     */
    username: pulumi.Input<string>;
}
