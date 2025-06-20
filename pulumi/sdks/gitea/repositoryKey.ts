// *** WARNING: this file was generated by pulumi-language-nodejs. ***
// *** Do not edit by hand unless you're certain you know what you are doing! ***

import * as pulumi from "@pulumi/pulumi";
import * as utilities from "./utilities";

export class RepositoryKey extends pulumi.CustomResource {
    /**
     * Get an existing RepositoryKey resource's state with the given name, ID, and optional extra
     * properties used to qualify the lookup.
     *
     * @param name The _unique_ name of the resulting resource.
     * @param id The _unique_ provider ID of the resource to lookup.
     * @param state Any extra arguments used during the lookup.
     * @param opts Optional settings to control the behavior of the CustomResource.
     */
    public static get(name: string, id: pulumi.Input<pulumi.ID>, state?: RepositoryKeyState, opts?: pulumi.CustomResourceOptions): RepositoryKey {
        return new RepositoryKey(name, <any>state, { ...opts, id: id });
    }

    /** @internal */
    public static readonly __pulumiType = 'gitea:index/repositoryKey:RepositoryKey';

    /**
     * Returns true if the given object is an instance of RepositoryKey.  This is designed to work even
     * when multiple copies of the Pulumi SDK have been loaded into the same process.
     */
    public static isInstance(obj: any): obj is RepositoryKey {
        if (obj === undefined || obj === null) {
            return false;
        }
        return obj['__pulumiType'] === RepositoryKey.__pulumiType;
    }

    /**
     * Armored SSH key to add
     */
    public readonly key!: pulumi.Output<string>;
    /**
     * Whether this key has read or read/write access
     */
    public readonly readOnly!: pulumi.Output<boolean | undefined>;
    /**
     * The ID of the repository where the deploy key belongs to
     */
    public readonly repository!: pulumi.Output<number>;
    public readonly repositoryKeyId!: pulumi.Output<string>;
    /**
     * Name of the deploy key
     */
    public readonly title!: pulumi.Output<string>;

    /**
     * Create a RepositoryKey resource with the given unique name, arguments, and options.
     *
     * @param name The _unique_ name of the resource.
     * @param args The arguments to use to populate this resource's properties.
     * @param opts A bag of options that control this resource's behavior.
     */
    constructor(name: string, args: RepositoryKeyArgs, opts?: pulumi.CustomResourceOptions)
    constructor(name: string, argsOrState?: RepositoryKeyArgs | RepositoryKeyState, opts?: pulumi.CustomResourceOptions) {
        let resourceInputs: pulumi.Inputs = {};
        opts = opts || {};
        if (opts.id) {
            const state = argsOrState as RepositoryKeyState | undefined;
            resourceInputs["key"] = state ? state.key : undefined;
            resourceInputs["readOnly"] = state ? state.readOnly : undefined;
            resourceInputs["repository"] = state ? state.repository : undefined;
            resourceInputs["repositoryKeyId"] = state ? state.repositoryKeyId : undefined;
            resourceInputs["title"] = state ? state.title : undefined;
        } else {
            const args = argsOrState as RepositoryKeyArgs | undefined;
            if ((!args || args.key === undefined) && !opts.urn) {
                throw new Error("Missing required property 'key'");
            }
            if ((!args || args.repository === undefined) && !opts.urn) {
                throw new Error("Missing required property 'repository'");
            }
            if ((!args || args.title === undefined) && !opts.urn) {
                throw new Error("Missing required property 'title'");
            }
            resourceInputs["key"] = args ? args.key : undefined;
            resourceInputs["readOnly"] = args ? args.readOnly : undefined;
            resourceInputs["repository"] = args ? args.repository : undefined;
            resourceInputs["repositoryKeyId"] = args ? args.repositoryKeyId : undefined;
            resourceInputs["title"] = args ? args.title : undefined;
        }
        opts = pulumi.mergeOptions(utilities.resourceOptsDefaults(), opts);
        super(RepositoryKey.__pulumiType, name, resourceInputs, opts, false /*dependency*/, utilities.getPackage());
    }
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
