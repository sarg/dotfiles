import * as pulumi from "@pulumi/pulumi";
export declare class GpgKey extends pulumi.CustomResource {
    /**
     * Get an existing GpgKey resource's state with the given name, ID, and optional extra
     * properties used to qualify the lookup.
     *
     * @param name The _unique_ name of the resulting resource.
     * @param id The _unique_ provider ID of the resource to lookup.
     * @param state Any extra arguments used during the lookup.
     * @param opts Optional settings to control the behavior of the CustomResource.
     */
    static get(name: string, id: pulumi.Input<pulumi.ID>, state?: GpgKeyState, opts?: pulumi.CustomResourceOptions): GpgKey;
    /**
     * Returns true if the given object is an instance of GpgKey.  This is designed to work even
     * when multiple copies of the Pulumi SDK have been loaded into the same process.
     */
    static isInstance(obj: any): obj is GpgKey;
    /**
     * An armored GPG public key
     */
    readonly armoredPublicKey: pulumi.Output<string>;
    readonly gpgKeyId: pulumi.Output<string>;
    /**
     * The ID of the GPG key
     */
    readonly keyId: pulumi.Output<string>;
    /**
     * Create a GpgKey resource with the given unique name, arguments, and options.
     *
     * @param name The _unique_ name of the resource.
     * @param args The arguments to use to populate this resource's properties.
     * @param opts A bag of options that control this resource's behavior.
     */
    constructor(name: string, args: GpgKeyArgs, opts?: pulumi.CustomResourceOptions);
}
/**
 * Input properties used for looking up and filtering GpgKey resources.
 */
export interface GpgKeyState {
    /**
     * An armored GPG public key
     */
    armoredPublicKey?: pulumi.Input<string>;
    gpgKeyId?: pulumi.Input<string>;
    /**
     * The ID of the GPG key
     */
    keyId?: pulumi.Input<string>;
}
/**
 * The set of arguments for constructing a GpgKey resource.
 */
export interface GpgKeyArgs {
    /**
     * An armored GPG public key
     */
    armoredPublicKey: pulumi.Input<string>;
    gpgKeyId?: pulumi.Input<string>;
}
