import * as pulumi from "@pulumi/pulumi";
export declare class TeamMembership extends pulumi.CustomResource {
    /**
     * Get an existing TeamMembership resource's state with the given name, ID, and optional extra
     * properties used to qualify the lookup.
     *
     * @param name The _unique_ name of the resulting resource.
     * @param id The _unique_ provider ID of the resource to lookup.
     * @param state Any extra arguments used during the lookup.
     * @param opts Optional settings to control the behavior of the CustomResource.
     */
    static get(name: string, id: pulumi.Input<pulumi.ID>, state?: TeamMembershipState, opts?: pulumi.CustomResourceOptions): TeamMembership;
    /**
     * Returns true if the given object is an instance of TeamMembership.  This is designed to work even
     * when multiple copies of the Pulumi SDK have been loaded into the same process.
     */
    static isInstance(obj: any): obj is TeamMembership;
    /**
     * The ID of the team.
     */
    readonly teamId: pulumi.Output<number>;
    readonly teamMembershipId: pulumi.Output<string>;
    /**
     * The username of the team member.
     */
    readonly username: pulumi.Output<string>;
    /**
     * Create a TeamMembership resource with the given unique name, arguments, and options.
     *
     * @param name The _unique_ name of the resource.
     * @param args The arguments to use to populate this resource's properties.
     * @param opts A bag of options that control this resource's behavior.
     */
    constructor(name: string, args: TeamMembershipArgs, opts?: pulumi.CustomResourceOptions);
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
