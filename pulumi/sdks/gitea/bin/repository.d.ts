import * as pulumi from "@pulumi/pulumi";
export declare class Repository extends pulumi.CustomResource {
    /**
     * Get an existing Repository resource's state with the given name, ID, and optional extra
     * properties used to qualify the lookup.
     *
     * @param name The _unique_ name of the resulting resource.
     * @param id The _unique_ provider ID of the resource to lookup.
     * @param state Any extra arguments used during the lookup.
     * @param opts Optional settings to control the behavior of the CustomResource.
     */
    static get(name: string, id: pulumi.Input<pulumi.ID>, state?: RepositoryState, opts?: pulumi.CustomResourceOptions): Repository;
    /**
     * Returns true if the given object is an instance of Repository.  This is designed to work even
     * when multiple copies of the Pulumi SDK have been loaded into the same process.
     */
    static isInstance(obj: any): obj is Repository;
    readonly allowManualMerge: pulumi.Output<boolean | undefined>;
    readonly allowMergeCommits: pulumi.Output<boolean | undefined>;
    readonly allowRebase: pulumi.Output<boolean | undefined>;
    readonly allowRebaseExplicit: pulumi.Output<boolean | undefined>;
    readonly allowSquashMerge: pulumi.Output<boolean | undefined>;
    /**
     * Set to 'true' to archive the repository instead of deleting on destroy.
     */
    readonly archiveOnDestroy: pulumi.Output<boolean | undefined>;
    readonly archived: pulumi.Output<boolean | undefined>;
    /**
     * Flag if the repository should be initiated with the configured values
     */
    readonly autoInit: pulumi.Output<boolean | undefined>;
    readonly autodetectManualMerge: pulumi.Output<boolean | undefined>;
    readonly cloneUrl: pulumi.Output<string>;
    readonly created: pulumi.Output<string>;
    /**
     * The default branch of the repository. Defaults to `main`
     */
    readonly defaultBranch: pulumi.Output<string | undefined>;
    /**
     * The description of the repository.
     */
    readonly description: pulumi.Output<string | undefined>;
    /**
     * A specific gitignore that should be commited to the repositoryon creation if `auto_init` is set to `true` Need to exist
     * in the gitea instance
     */
    readonly gitignores: pulumi.Output<string | undefined>;
    /**
     * A flag if the repository should have issue management enabled or not.
     */
    readonly hasIssues: pulumi.Output<boolean | undefined>;
    /**
     * A flag if the repository should have the native project management enabled or not.
     */
    readonly hasProjects: pulumi.Output<boolean | undefined>;
    /**
     * A flag if the repository should acceppt pull requests or not.
     */
    readonly hasPullRequests: pulumi.Output<boolean | undefined>;
    /**
     * A flag if the repository should have the native wiki enabled or not.
     */
    readonly hasWiki: pulumi.Output<boolean | undefined>;
    readonly htmlUrl: pulumi.Output<string>;
    readonly ignoreWhitespaceConflicts: pulumi.Output<boolean | undefined>;
    /**
     * The Issue Label configuration to be used in this repository. Need to exist in the gitea instance
     */
    readonly issueLabels: pulumi.Output<string | undefined>;
    /**
     * The license under which the source code of this repository should be. Need to exist in the gitea instance
     */
    readonly license: pulumi.Output<string | undefined>;
    readonly migrationCloneAddress: pulumi.Output<string | undefined>;
    /**
     * DEPRECATED in favor of `migration_clone_address`
     */
    readonly migrationCloneAddresse: pulumi.Output<string | undefined>;
    readonly migrationIssueLabels: pulumi.Output<boolean | undefined>;
    readonly migrationLfs: pulumi.Output<boolean | undefined>;
    readonly migrationLfsEndpoint: pulumi.Output<string | undefined>;
    readonly migrationMilestones: pulumi.Output<boolean | undefined>;
    /**
     * valid time units are 'h', 'm', 's'. 0 to disable automatic sync
     */
    readonly migrationMirrorInterval: pulumi.Output<string | undefined>;
    readonly migrationReleases: pulumi.Output<boolean | undefined>;
    /**
     * git/github/gitlab/gitea/gogs
     */
    readonly migrationService: pulumi.Output<string | undefined>;
    readonly migrationServiceAuthPassword: pulumi.Output<string | undefined>;
    readonly migrationServiceAuthToken: pulumi.Output<string | undefined>;
    readonly migrationServiceAuthUsername: pulumi.Output<string | undefined>;
    readonly mirror: pulumi.Output<boolean | undefined>;
    /**
     * The Name of the repository
     */
    readonly name: pulumi.Output<string>;
    readonly permissionAdmin: pulumi.Output<boolean>;
    readonly permissionPull: pulumi.Output<boolean>;
    readonly permissionPush: pulumi.Output<boolean>;
    /**
     * Flag if the repository should be private or not.
     */
    readonly private: pulumi.Output<boolean | undefined>;
    readonly readme: pulumi.Output<string | undefined>;
    readonly repoTemplate: pulumi.Output<boolean | undefined>;
    readonly repositoryId: pulumi.Output<string>;
    readonly sshUrl: pulumi.Output<string>;
    readonly updated: pulumi.Output<string>;
    /**
     * The Owner of the repository
     */
    readonly username: pulumi.Output<string>;
    /**
     * A link to a website with more information.
     */
    readonly website: pulumi.Output<string | undefined>;
    /**
     * Create a Repository resource with the given unique name, arguments, and options.
     *
     * @param name The _unique_ name of the resource.
     * @param args The arguments to use to populate this resource's properties.
     * @param opts A bag of options that control this resource's behavior.
     */
    constructor(name: string, args: RepositoryArgs, opts?: pulumi.CustomResourceOptions);
}
/**
 * Input properties used for looking up and filtering Repository resources.
 */
export interface RepositoryState {
    allowManualMerge?: pulumi.Input<boolean>;
    allowMergeCommits?: pulumi.Input<boolean>;
    allowRebase?: pulumi.Input<boolean>;
    allowRebaseExplicit?: pulumi.Input<boolean>;
    allowSquashMerge?: pulumi.Input<boolean>;
    /**
     * Set to 'true' to archive the repository instead of deleting on destroy.
     */
    archiveOnDestroy?: pulumi.Input<boolean>;
    archived?: pulumi.Input<boolean>;
    /**
     * Flag if the repository should be initiated with the configured values
     */
    autoInit?: pulumi.Input<boolean>;
    autodetectManualMerge?: pulumi.Input<boolean>;
    cloneUrl?: pulumi.Input<string>;
    created?: pulumi.Input<string>;
    /**
     * The default branch of the repository. Defaults to `main`
     */
    defaultBranch?: pulumi.Input<string>;
    /**
     * The description of the repository.
     */
    description?: pulumi.Input<string>;
    /**
     * A specific gitignore that should be commited to the repositoryon creation if `auto_init` is set to `true` Need to exist
     * in the gitea instance
     */
    gitignores?: pulumi.Input<string>;
    /**
     * A flag if the repository should have issue management enabled or not.
     */
    hasIssues?: pulumi.Input<boolean>;
    /**
     * A flag if the repository should have the native project management enabled or not.
     */
    hasProjects?: pulumi.Input<boolean>;
    /**
     * A flag if the repository should acceppt pull requests or not.
     */
    hasPullRequests?: pulumi.Input<boolean>;
    /**
     * A flag if the repository should have the native wiki enabled or not.
     */
    hasWiki?: pulumi.Input<boolean>;
    htmlUrl?: pulumi.Input<string>;
    ignoreWhitespaceConflicts?: pulumi.Input<boolean>;
    /**
     * The Issue Label configuration to be used in this repository. Need to exist in the gitea instance
     */
    issueLabels?: pulumi.Input<string>;
    /**
     * The license under which the source code of this repository should be. Need to exist in the gitea instance
     */
    license?: pulumi.Input<string>;
    migrationCloneAddress?: pulumi.Input<string>;
    /**
     * DEPRECATED in favor of `migration_clone_address`
     */
    migrationCloneAddresse?: pulumi.Input<string>;
    migrationIssueLabels?: pulumi.Input<boolean>;
    migrationLfs?: pulumi.Input<boolean>;
    migrationLfsEndpoint?: pulumi.Input<string>;
    migrationMilestones?: pulumi.Input<boolean>;
    /**
     * valid time units are 'h', 'm', 's'. 0 to disable automatic sync
     */
    migrationMirrorInterval?: pulumi.Input<string>;
    migrationReleases?: pulumi.Input<boolean>;
    /**
     * git/github/gitlab/gitea/gogs
     */
    migrationService?: pulumi.Input<string>;
    migrationServiceAuthPassword?: pulumi.Input<string>;
    migrationServiceAuthToken?: pulumi.Input<string>;
    migrationServiceAuthUsername?: pulumi.Input<string>;
    mirror?: pulumi.Input<boolean>;
    /**
     * The Name of the repository
     */
    name?: pulumi.Input<string>;
    permissionAdmin?: pulumi.Input<boolean>;
    permissionPull?: pulumi.Input<boolean>;
    permissionPush?: pulumi.Input<boolean>;
    /**
     * Flag if the repository should be private or not.
     */
    private?: pulumi.Input<boolean>;
    readme?: pulumi.Input<string>;
    repoTemplate?: pulumi.Input<boolean>;
    repositoryId?: pulumi.Input<string>;
    sshUrl?: pulumi.Input<string>;
    updated?: pulumi.Input<string>;
    /**
     * The Owner of the repository
     */
    username?: pulumi.Input<string>;
    /**
     * A link to a website with more information.
     */
    website?: pulumi.Input<string>;
}
/**
 * The set of arguments for constructing a Repository resource.
 */
export interface RepositoryArgs {
    allowManualMerge?: pulumi.Input<boolean>;
    allowMergeCommits?: pulumi.Input<boolean>;
    allowRebase?: pulumi.Input<boolean>;
    allowRebaseExplicit?: pulumi.Input<boolean>;
    allowSquashMerge?: pulumi.Input<boolean>;
    /**
     * Set to 'true' to archive the repository instead of deleting on destroy.
     */
    archiveOnDestroy?: pulumi.Input<boolean>;
    archived?: pulumi.Input<boolean>;
    /**
     * Flag if the repository should be initiated with the configured values
     */
    autoInit?: pulumi.Input<boolean>;
    autodetectManualMerge?: pulumi.Input<boolean>;
    /**
     * The default branch of the repository. Defaults to `main`
     */
    defaultBranch?: pulumi.Input<string>;
    /**
     * The description of the repository.
     */
    description?: pulumi.Input<string>;
    /**
     * A specific gitignore that should be commited to the repositoryon creation if `auto_init` is set to `true` Need to exist
     * in the gitea instance
     */
    gitignores?: pulumi.Input<string>;
    /**
     * A flag if the repository should have issue management enabled or not.
     */
    hasIssues?: pulumi.Input<boolean>;
    /**
     * A flag if the repository should have the native project management enabled or not.
     */
    hasProjects?: pulumi.Input<boolean>;
    /**
     * A flag if the repository should acceppt pull requests or not.
     */
    hasPullRequests?: pulumi.Input<boolean>;
    /**
     * A flag if the repository should have the native wiki enabled or not.
     */
    hasWiki?: pulumi.Input<boolean>;
    ignoreWhitespaceConflicts?: pulumi.Input<boolean>;
    /**
     * The Issue Label configuration to be used in this repository. Need to exist in the gitea instance
     */
    issueLabels?: pulumi.Input<string>;
    /**
     * The license under which the source code of this repository should be. Need to exist in the gitea instance
     */
    license?: pulumi.Input<string>;
    migrationCloneAddress?: pulumi.Input<string>;
    /**
     * DEPRECATED in favor of `migration_clone_address`
     */
    migrationCloneAddresse?: pulumi.Input<string>;
    migrationIssueLabels?: pulumi.Input<boolean>;
    migrationLfs?: pulumi.Input<boolean>;
    migrationLfsEndpoint?: pulumi.Input<string>;
    migrationMilestones?: pulumi.Input<boolean>;
    /**
     * valid time units are 'h', 'm', 's'. 0 to disable automatic sync
     */
    migrationMirrorInterval?: pulumi.Input<string>;
    migrationReleases?: pulumi.Input<boolean>;
    /**
     * git/github/gitlab/gitea/gogs
     */
    migrationService?: pulumi.Input<string>;
    migrationServiceAuthPassword?: pulumi.Input<string>;
    migrationServiceAuthToken?: pulumi.Input<string>;
    migrationServiceAuthUsername?: pulumi.Input<string>;
    mirror?: pulumi.Input<boolean>;
    /**
     * The Name of the repository
     */
    name?: pulumi.Input<string>;
    /**
     * Flag if the repository should be private or not.
     */
    private?: pulumi.Input<boolean>;
    readme?: pulumi.Input<string>;
    repoTemplate?: pulumi.Input<boolean>;
    repositoryId?: pulumi.Input<string>;
    /**
     * The Owner of the repository
     */
    username: pulumi.Input<string>;
    /**
     * A link to a website with more information.
     */
    website?: pulumi.Input<string>;
}
