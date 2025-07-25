import * as pulumi from "@pulumi/pulumi";
export declare class TeamMembers extends pulumi.CustomResource {
    /**
     * Get an existing TeamMembers resource's state with the given name, ID, and optional extra
     * properties used to qualify the lookup.
     *
     * @param name The _unique_ name of the resulting resource.
     * @param id The _unique_ provider ID of the resource to lookup.
     * @param state Any extra arguments used during the lookup.
     * @param opts Optional settings to control the behavior of the CustomResource.
     */
    static get(name: string, id: pulumi.Input<pulumi.ID>, state?: TeamMembersState, opts?: pulumi.CustomResourceOptions): TeamMembers;
    /**
     * Returns true if the given object is an instance of TeamMembers.  This is designed to work even
     * when multiple copies of the Pulumi SDK have been loaded into the same process.
     */
    static isInstance(obj: any): obj is TeamMembers;
    /**
     * The user names of the members of the team.
     */
    readonly members: pulumi.Output<string[]>;
    /**
     * The ID of the team.
     */
    readonly teamId: pulumi.Output<number>;
    readonly teamMembersId: pulumi.Output<string>;
    /**
     * Create a TeamMembers resource with the given unique name, arguments, and options.
     *
     * @param name The _unique_ name of the resource.
     * @param args The arguments to use to populate this resource's properties.
     * @param opts A bag of options that control this resource's behavior.
     */
    constructor(name: string, args: TeamMembersArgs, opts?: pulumi.CustomResourceOptions);
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
