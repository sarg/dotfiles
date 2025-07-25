// *** WARNING: this file was generated by pulumi-language-nodejs. ***
// *** Do not edit by hand unless you're certain you know what you are doing! ***

import * as pulumi from "@pulumi/pulumi";
import * as utilities from "./utilities";

export class TeamMembership extends pulumi.CustomResource {
    /**
     * Get an existing TeamMembership resource's state with the given name, ID, and optional extra
     * properties used to qualify the lookup.
     *
     * @param name The _unique_ name of the resulting resource.
     * @param id The _unique_ provider ID of the resource to lookup.
     * @param state Any extra arguments used during the lookup.
     * @param opts Optional settings to control the behavior of the CustomResource.
     */
    public static get(name: string, id: pulumi.Input<pulumi.ID>, state?: TeamMembershipState, opts?: pulumi.CustomResourceOptions): TeamMembership {
        return new TeamMembership(name, <any>state, { ...opts, id: id });
    }

    /** @internal */
    public static readonly __pulumiType = 'gitea:index/teamMembership:TeamMembership';

    /**
     * Returns true if the given object is an instance of TeamMembership.  This is designed to work even
     * when multiple copies of the Pulumi SDK have been loaded into the same process.
     */
    public static isInstance(obj: any): obj is TeamMembership {
        if (obj === undefined || obj === null) {
            return false;
        }
        return obj['__pulumiType'] === TeamMembership.__pulumiType;
    }

    /**
     * The ID of the team.
     */
    public readonly teamId!: pulumi.Output<number>;
    public readonly teamMembershipId!: pulumi.Output<string>;
    /**
     * The username of the team member.
     */
    public readonly username!: pulumi.Output<string>;

    /**
     * Create a TeamMembership resource with the given unique name, arguments, and options.
     *
     * @param name The _unique_ name of the resource.
     * @param args The arguments to use to populate this resource's properties.
     * @param opts A bag of options that control this resource's behavior.
     */
    constructor(name: string, args: TeamMembershipArgs, opts?: pulumi.CustomResourceOptions)
    constructor(name: string, argsOrState?: TeamMembershipArgs | TeamMembershipState, opts?: pulumi.CustomResourceOptions) {
        let resourceInputs: pulumi.Inputs = {};
        opts = opts || {};
        if (opts.id) {
            const state = argsOrState as TeamMembershipState | undefined;
            resourceInputs["teamId"] = state ? state.teamId : undefined;
            resourceInputs["teamMembershipId"] = state ? state.teamMembershipId : undefined;
            resourceInputs["username"] = state ? state.username : undefined;
        } else {
            const args = argsOrState as TeamMembershipArgs | undefined;
            if ((!args || args.teamId === undefined) && !opts.urn) {
                throw new Error("Missing required property 'teamId'");
            }
            if ((!args || args.username === undefined) && !opts.urn) {
                throw new Error("Missing required property 'username'");
            }
            resourceInputs["teamId"] = args ? args.teamId : undefined;
            resourceInputs["teamMembershipId"] = args ? args.teamMembershipId : undefined;
            resourceInputs["username"] = args ? args.username : undefined;
        }
        opts = pulumi.mergeOptions(utilities.resourceOptsDefaults(), opts);
        super(TeamMembership.__pulumiType, name, resourceInputs, opts, false /*dependency*/, utilities.getPackage());
    }
}

/**
 * Input properties used for looking up and filtering TeamMembership resources.
 */
export interface TeamMembershipState {
    /**
     * The ID of the team.
     */
    teamId?: pulumi.Input<number>;
    teamMembershipId?: pulumi.Input<string>;
    /**
     * The username of the team member.
     */
    username?: pulumi.Input<string>;
}

/**
 * The set of arguments for constructing a TeamMembership resource.
 */
export interface TeamMembershipArgs {
    /**
     * The ID of the team.
     */
    teamId: pulumi.Input<number>;
    teamMembershipId?: pulumi.Input<string>;
    /**
     * The username of the team member.
     */
    username: pulumi.Input<string>;
}
