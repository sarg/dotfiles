import * as pulumi from "@pulumi/pulumi";
export declare class Team extends pulumi.CustomResource {
    /**
     * Get an existing Team resource's state with the given name, ID, and optional extra
     * properties used to qualify the lookup.
     *
     * @param name The _unique_ name of the resulting resource.
     * @param id The _unique_ provider ID of the resource to lookup.
     * @param state Any extra arguments used during the lookup.
     * @param opts Optional settings to control the behavior of the CustomResource.
     */
    static get(name: string, id: pulumi.Input<pulumi.ID>, state?: TeamState, opts?: pulumi.CustomResourceOptions): Team;
    /**
     * Returns true if the given object is an instance of Team.  This is designed to work even
     * when multiple copies of the Pulumi SDK have been loaded into the same process.
     */
    static isInstance(obj: any): obj is Team;
    /**
     * Flag if the Teams members should be able to create Rpositories in the Organisation
     */
    readonly canCreateRepos: pulumi.Output<boolean | undefined>;
    /**
     * Description of the Team
     */
    readonly description: pulumi.Output<string | undefined>;
    /**
     * Flag if the Teams members should have access to all Repositories in the Organisation
     */
    readonly includeAllRepositories: pulumi.Output<boolean | undefined>;
    /**
     * Name of the Team
     */
    readonly name: pulumi.Output<string>;
    /**
     * The organisation which this Team is part of.
     */
    readonly organisation: pulumi.Output<string>;
    /**
     * Permissions associated with this Team Can be `none`, `read`, `write`, `admin` or `owner`
     */
    readonly permission: pulumi.Output<string | undefined>;
    /**
     * List of Repositories that should be part of this team
     */
    readonly repositories: pulumi.Output<string[]>;
    readonly teamId: pulumi.Output<string>;
    /**
     * List of types of Repositories that should be allowed to be created from Team members. Can be `repo.code`, `repo.issues`,
     * `repo.ext_issues`, `repo.wiki`, `repo.pulls`, `repo.releases`, `repo.projects` and/or `repo.ext_wiki`
     */
    readonly units: pulumi.Output<string | undefined>;
    /**
     * Create a Team resource with the given unique name, arguments, and options.
     *
     * @param name The _unique_ name of the resource.
     * @param args The arguments to use to populate this resource's properties.
     * @param opts A bag of options that control this resource's behavior.
     */
    constructor(name: string, args: TeamArgs, opts?: pulumi.CustomResourceOptions);
}
/**
 * Input properties used for looking up and filtering Team resources.
 */
export interface TeamState {
    /**
     * Flag if the Teams members should be able to create Rpositories in the Organisation
     */
    canCreateRepos?: pulumi.Input<boolean>;
    /**
     * Description of the Team
     */
    description?: pulumi.Input<string>;
    /**
     * Flag if the Teams members should have access to all Repositories in the Organisation
     */
    includeAllRepositories?: pulumi.Input<boolean>;
    /**
     * Name of the Team
     */
    name?: pulumi.Input<string>;
    /**
     * The organisation which this Team is part of.
     */
    organisation?: pulumi.Input<string>;
    /**
     * Permissions associated with this Team Can be `none`, `read`, `write`, `admin` or `owner`
     */
    permission?: pulumi.Input<string>;
    /**
     * List of Repositories that should be part of this team
     */
    repositories?: pulumi.Input<pulumi.Input<string>[]>;
    teamId?: pulumi.Input<string>;
    /**
     * List of types of Repositories that should be allowed to be created from Team members. Can be `repo.code`, `repo.issues`,
     * `repo.ext_issues`, `repo.wiki`, `repo.pulls`, `repo.releases`, `repo.projects` and/or `repo.ext_wiki`
     */
    units?: pulumi.Input<string>;
}
/**
 * The set of arguments for constructing a Team resource.
 */
export interface TeamArgs {
    /**
     * Flag if the Teams members should be able to create Rpositories in the Organisation
     */
    canCreateRepos?: pulumi.Input<boolean>;
    /**
     * Description of the Team
     */
    description?: pulumi.Input<string>;
    /**
     * Flag if the Teams members should have access to all Repositories in the Organisation
     */
    includeAllRepositories?: pulumi.Input<boolean>;
    /**
     * Name of the Team
     */
    name?: pulumi.Input<string>;
    /**
     * The organisation which this Team is part of.
     */
    organisation: pulumi.Input<string>;
    /**
     * Permissions associated with this Team Can be `none`, `read`, `write`, `admin` or `owner`
     */
    permission?: pulumi.Input<string>;
    /**
     * List of Repositories that should be part of this team
     */
    repositories?: pulumi.Input<pulumi.Input<string>[]>;
    teamId?: pulumi.Input<string>;
    /**
     * List of types of Repositories that should be allowed to be created from Team members. Can be `repo.code`, `repo.issues`,
     * `repo.ext_issues`, `repo.wiki`, `repo.pulls`, `repo.releases`, `repo.projects` and/or `repo.ext_wiki`
     */
    units?: pulumi.Input<string>;
}
