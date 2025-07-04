// *** WARNING: this file was generated by pulumi-language-nodejs. ***
// *** Do not edit by hand unless you're certain you know what you are doing! ***

import * as pulumi from "@pulumi/pulumi";
import * as utilities from "./utilities";

export class TeamMembers extends pulumi.CustomResource {
    /**
     * Get an existing TeamMembers resource's state with the given name, ID, and optional extra
     * properties used to qualify the lookup.
     *
     * @param name The _unique_ name of the resulting resource.
     * @param id The _unique_ provider ID of the resource to lookup.
     * @param state Any extra arguments used during the lookup.
     * @param opts Optional settings to control the behavior of the CustomResource.
     */
    public static get(name: string, id: pulumi.Input<pulumi.ID>, state?: TeamMembersState, opts?: pulumi.CustomResourceOptions): TeamMembers {
        return new TeamMembers(name, <any>state, { ...opts, id: id });
    }

    /** @internal */
    public static readonly __pulumiType = 'gitea:index/teamMembers:TeamMembers';

    /**
     * Returns true if the given object is an instance of TeamMembers.  This is designed to work even
     * when multiple copies of the Pulumi SDK have been loaded into the same process.
     */
    public static isInstance(obj: any): obj is TeamMembers {
        if (obj === undefined || obj === null) {
            return false;
        }
        return obj['__pulumiType'] === TeamMembers.__pulumiType;
    }

    /**
     * The user names of the members of the team.
     */
    public readonly members!: pulumi.Output<string[]>;
    /**
     * The ID of the team.
     */
    public readonly teamId!: pulumi.Output<number>;
    public readonly teamMembersId!: pulumi.Output<string>;

    /**
     * Create a TeamMembers resource with the given unique name, arguments, and options.
     *
     * @param name The _unique_ name of the resource.
     * @param args The arguments to use to populate this resource's properties.
     * @param opts A bag of options that control this resource's behavior.
     */
    constructor(name: string, args: TeamMembersArgs, opts?: pulumi.CustomResourceOptions)
    constructor(name: string, argsOrState?: TeamMembersArgs | TeamMembersState, opts?: pulumi.CustomResourceOptions) {
        let resourceInputs: pulumi.Inputs = {};
        opts = opts || {};
        if (opts.id) {
            const state = argsOrState as TeamMembersState | undefined;
            resourceInputs["members"] = state ? state.members : undefined;
            resourceInputs["teamId"] = state ? state.teamId : undefined;
            resourceInputs["teamMembersId"] = state ? state.teamMembersId : undefined;
        } else {
            const args = argsOrState as TeamMembersArgs | undefined;
            if ((!args || args.members === undefined) && !opts.urn) {
                throw new Error("Missing required property 'members'");
            }
            if ((!args || args.teamId === undefined) && !opts.urn) {
                throw new Error("Missing required property 'teamId'");
            }
            resourceInputs["members"] = args ? args.members : undefined;
            resourceInputs["teamId"] = args ? args.teamId : undefined;
            resourceInputs["teamMembersId"] = args ? args.teamMembersId : undefined;
        }
        opts = pulumi.mergeOptions(utilities.resourceOptsDefaults(), opts);
        super(TeamMembers.__pulumiType, name, resourceInputs, opts, false /*dependency*/, utilities.getPackage());
    }
}

/**
 * Input properties used for looking up and filtering TeamMembers resources.
 */
export interface TeamMembersState {
    /**
     * The user names of the members of the team.
     */
    members?: pulumi.Input<pulumi.Input<string>[]>;
    /**
     * The ID of the team.
     */
    teamId?: pulumi.Input<number>;
    teamMembersId?: pulumi.Input<string>;
}

/**
 * The set of arguments for constructing a TeamMembers resource.
 */
export interface TeamMembersArgs {
    /**
     * The user names of the members of the team.
     */
    members: pulumi.Input<pulumi.Input<string>[]>;
    /**
     * The ID of the team.
     */
    teamId: pulumi.Input<number>;
    teamMembersId?: pulumi.Input<string>;
}
