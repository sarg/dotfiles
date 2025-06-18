import * as pulumi from "@pulumi/pulumi";
export declare class RepositoryBranchProtection extends pulumi.CustomResource {
    /**
     * Get an existing RepositoryBranchProtection resource's state with the given name, ID, and optional extra
     * properties used to qualify the lookup.
     *
     * @param name The _unique_ name of the resulting resource.
     * @param id The _unique_ provider ID of the resource to lookup.
     * @param state Any extra arguments used during the lookup.
     * @param opts Optional settings to control the behavior of the CustomResource.
     */
    static get(name: string, id: pulumi.Input<pulumi.ID>, state?: RepositoryBranchProtectionState, opts?: pulumi.CustomResourceOptions): RepositoryBranchProtection;
    /**
     * Returns true if the given object is an instance of RepositoryBranchProtection.  This is designed to work even
     * when multiple copies of the Pulumi SDK have been loaded into the same process.
     */
    static isInstance(obj: any): obj is RepositoryBranchProtection;
    /**
     * Only reviews from allowlisted teams will count to the required approvals. Without approval allowlist, reviews from
     * anyone with write access count to the required approvals.
     */
    readonly approvalWhitelistTeams: pulumi.Output<string[] | undefined>;
    /**
     * Only reviews from allowlisted users will count to the required approvals. Without approval allowlist, reviews from
     * anyone with write access count to the required approvals.
     */
    readonly approvalWhitelistUsers: pulumi.Output<string[] | undefined>;
    /**
     * Merging will not be possible when it has official review requests, even if there are enough approvals.
     */
    readonly blockMergeOnOfficialReviewRequests: pulumi.Output<boolean | undefined>;
    /**
     * Merging will not be possible when head branch is behind base branch.
     */
    readonly blockMergeOnOutdatedBranch: pulumi.Output<boolean | undefined>;
    /**
     * Merging will not be possible when changes are requested by official reviewers, even if there are enough approvals.
     */
    readonly blockMergeOnRejectedReviews: pulumi.Output<boolean | undefined>;
    /**
     * Webhook creation timestamp
     */
    readonly createdAt: pulumi.Output<string>;
    /**
     * When new commits that change the content of the pull request are pushed to the branch, old approvals will be dismissed.
     */
    readonly dismissStaleApprovals: pulumi.Output<boolean | undefined>;
    /**
     * True if a approval whitelist is used.
     */
    readonly enableApprovalWhitelist: pulumi.Output<boolean>;
    /**
     * True if a merge whitelist is used.
     */
    readonly enableMergeWhitelist: pulumi.Output<boolean>;
    /**
     * Anyone with write access will be allowed to push to this branch (but not force push), add a whitelist users or teams to
     * limit access.
     */
    readonly enablePush: pulumi.Output<boolean | undefined>;
    /**
     * True if a push whitelist is used.
     */
    readonly enablePushWhitelist: pulumi.Output<boolean>;
    /**
     * Require status checks to pass before merging. When enabled, commits must first be pushed to another branch, then merged
     * or pushed directly to a branch that matches this rule after status checks have passed. If no contexts are matched, the
     * last commit must be successful regardless of context
     */
    readonly enableStatusCheck: pulumi.Output<boolean>;
    /**
     * Allow only allowlisted teams to merge pull requests into this branch.
     */
    readonly mergeWhitelistTeams: pulumi.Output<string[] | undefined>;
    /**
     * Allow only allowlisted users to merge pull requests into this branch.
     */
    readonly mergeWhitelistUsers: pulumi.Output<string[] | undefined>;
    /**
     * Repository name
     */
    readonly name: pulumi.Output<string>;
    /**
     * Protected file patterns (separated using semicolon ';')
     */
    readonly protectedFilePatterns: pulumi.Output<string | undefined>;
    /**
     * Allow deploy keys with write access to push. Requires enable_push to be set to true.
     */
    readonly pushWhitelistDeployKeys: pulumi.Output<boolean | undefined>;
    /**
     * Allowlisted teams for pushing. Requires enable_push to be set to true.
     */
    readonly pushWhitelistTeams: pulumi.Output<string[] | undefined>;
    /**
     * Allowlisted users for pushing. Requires enable_push to be set to true.
     */
    readonly pushWhitelistUsers: pulumi.Output<string[] | undefined>;
    readonly repositoryBranchProtectionId: pulumi.Output<string>;
    /**
     * Reject pushes to this branch if they are unsigned or unverifiable.
     */
    readonly requireSignedCommits: pulumi.Output<boolean | undefined>;
    /**
     * Allow only to merge pull request with enough positive reviews.
     */
    readonly requiredApprovals: pulumi.Output<number | undefined>;
    /**
     * Protected Branch Name Pattern
     */
    readonly ruleName: pulumi.Output<string>;
    /**
     * Enter patterns to specify which status checks must pass before branches can be merged into a branch that matches this
     * rule. Each line specifies a pattern. Patterns cannot be empty.
     */
    readonly statusCheckPatterns: pulumi.Output<string[] | undefined>;
    /**
     * Unprotected file patterns (separated using semicolon ';')
     */
    readonly unprotectedFilePatterns: pulumi.Output<string | undefined>;
    /**
     * Webhook creation timestamp
     */
    readonly updatedAt: pulumi.Output<string>;
    /**
     * User name or organization name
     */
    readonly username: pulumi.Output<string>;
    /**
     * Create a RepositoryBranchProtection resource with the given unique name, arguments, and options.
     *
     * @param name The _unique_ name of the resource.
     * @param args The arguments to use to populate this resource's properties.
     * @param opts A bag of options that control this resource's behavior.
     */
    constructor(name: string, args: RepositoryBranchProtectionArgs, opts?: pulumi.CustomResourceOptions);
}
/**
 * Input properties used for looking up and filtering RepositoryBranchProtection resources.
 */
export interface RepositoryBranchProtectionState {
    /**
     * Only reviews from allowlisted teams will count to the required approvals. Without approval allowlist, reviews from
     * anyone with write access count to the required approvals.
     */
    approvalWhitelistTeams?: pulumi.Input<pulumi.Input<string>[]>;
    /**
     * Only reviews from allowlisted users will count to the required approvals. Without approval allowlist, reviews from
     * anyone with write access count to the required approvals.
     */
    approvalWhitelistUsers?: pulumi.Input<pulumi.Input<string>[]>;
    /**
     * Merging will not be possible when it has official review requests, even if there are enough approvals.
     */
    blockMergeOnOfficialReviewRequests?: pulumi.Input<boolean>;
    /**
     * Merging will not be possible when head branch is behind base branch.
     */
    blockMergeOnOutdatedBranch?: pulumi.Input<boolean>;
    /**
     * Merging will not be possible when changes are requested by official reviewers, even if there are enough approvals.
     */
    blockMergeOnRejectedReviews?: pulumi.Input<boolean>;
    /**
     * Webhook creation timestamp
     */
    createdAt?: pulumi.Input<string>;
    /**
     * When new commits that change the content of the pull request are pushed to the branch, old approvals will be dismissed.
     */
    dismissStaleApprovals?: pulumi.Input<boolean>;
    /**
     * True if a approval whitelist is used.
     */
    enableApprovalWhitelist?: pulumi.Input<boolean>;
    /**
     * True if a merge whitelist is used.
     */
    enableMergeWhitelist?: pulumi.Input<boolean>;
    /**
     * Anyone with write access will be allowed to push to this branch (but not force push), add a whitelist users or teams to
     * limit access.
     */
    enablePush?: pulumi.Input<boolean>;
    /**
     * True if a push whitelist is used.
     */
    enablePushWhitelist?: pulumi.Input<boolean>;
    /**
     * Require status checks to pass before merging. When enabled, commits must first be pushed to another branch, then merged
     * or pushed directly to a branch that matches this rule after status checks have passed. If no contexts are matched, the
     * last commit must be successful regardless of context
     */
    enableStatusCheck?: pulumi.Input<boolean>;
    /**
     * Allow only allowlisted teams to merge pull requests into this branch.
     */
    mergeWhitelistTeams?: pulumi.Input<pulumi.Input<string>[]>;
    /**
     * Allow only allowlisted users to merge pull requests into this branch.
     */
    mergeWhitelistUsers?: pulumi.Input<pulumi.Input<string>[]>;
    /**
     * Repository name
     */
    name?: pulumi.Input<string>;
    /**
     * Protected file patterns (separated using semicolon ';')
     */
    protectedFilePatterns?: pulumi.Input<string>;
    /**
     * Allow deploy keys with write access to push. Requires enable_push to be set to true.
     */
    pushWhitelistDeployKeys?: pulumi.Input<boolean>;
    /**
     * Allowlisted teams for pushing. Requires enable_push to be set to true.
     */
    pushWhitelistTeams?: pulumi.Input<pulumi.Input<string>[]>;
    /**
     * Allowlisted users for pushing. Requires enable_push to be set to true.
     */
    pushWhitelistUsers?: pulumi.Input<pulumi.Input<string>[]>;
    repositoryBranchProtectionId?: pulumi.Input<string>;
    /**
     * Reject pushes to this branch if they are unsigned or unverifiable.
     */
    requireSignedCommits?: pulumi.Input<boolean>;
    /**
     * Allow only to merge pull request with enough positive reviews.
     */
    requiredApprovals?: pulumi.Input<number>;
    /**
     * Protected Branch Name Pattern
     */
    ruleName?: pulumi.Input<string>;
    /**
     * Enter patterns to specify which status checks must pass before branches can be merged into a branch that matches this
     * rule. Each line specifies a pattern. Patterns cannot be empty.
     */
    statusCheckPatterns?: pulumi.Input<pulumi.Input<string>[]>;
    /**
     * Unprotected file patterns (separated using semicolon ';')
     */
    unprotectedFilePatterns?: pulumi.Input<string>;
    /**
     * Webhook creation timestamp
     */
    updatedAt?: pulumi.Input<string>;
    /**
     * User name or organization name
     */
    username?: pulumi.Input<string>;
}
/**
 * The set of arguments for constructing a RepositoryBranchProtection resource.
 */
export interface RepositoryBranchProtectionArgs {
    /**
     * Only reviews from allowlisted teams will count to the required approvals. Without approval allowlist, reviews from
     * anyone with write access count to the required approvals.
     */
    approvalWhitelistTeams?: pulumi.Input<pulumi.Input<string>[]>;
    /**
     * Only reviews from allowlisted users will count to the required approvals. Without approval allowlist, reviews from
     * anyone with write access count to the required approvals.
     */
    approvalWhitelistUsers?: pulumi.Input<pulumi.Input<string>[]>;
    /**
     * Merging will not be possible when it has official review requests, even if there are enough approvals.
     */
    blockMergeOnOfficialReviewRequests?: pulumi.Input<boolean>;
    /**
     * Merging will not be possible when head branch is behind base branch.
     */
    blockMergeOnOutdatedBranch?: pulumi.Input<boolean>;
    /**
     * Merging will not be possible when changes are requested by official reviewers, even if there are enough approvals.
     */
    blockMergeOnRejectedReviews?: pulumi.Input<boolean>;
    /**
     * When new commits that change the content of the pull request are pushed to the branch, old approvals will be dismissed.
     */
    dismissStaleApprovals?: pulumi.Input<boolean>;
    /**
     * Anyone with write access will be allowed to push to this branch (but not force push), add a whitelist users or teams to
     * limit access.
     */
    enablePush?: pulumi.Input<boolean>;
    /**
     * Allow only allowlisted teams to merge pull requests into this branch.
     */
    mergeWhitelistTeams?: pulumi.Input<pulumi.Input<string>[]>;
    /**
     * Allow only allowlisted users to merge pull requests into this branch.
     */
    mergeWhitelistUsers?: pulumi.Input<pulumi.Input<string>[]>;
    /**
     * Repository name
     */
    name?: pulumi.Input<string>;
    /**
     * Protected file patterns (separated using semicolon ';')
     */
    protectedFilePatterns?: pulumi.Input<string>;
    /**
     * Allow deploy keys with write access to push. Requires enable_push to be set to true.
     */
    pushWhitelistDeployKeys?: pulumi.Input<boolean>;
    /**
     * Allowlisted teams for pushing. Requires enable_push to be set to true.
     */
    pushWhitelistTeams?: pulumi.Input<pulumi.Input<string>[]>;
    /**
     * Allowlisted users for pushing. Requires enable_push to be set to true.
     */
    pushWhitelistUsers?: pulumi.Input<pulumi.Input<string>[]>;
    repositoryBranchProtectionId?: pulumi.Input<string>;
    /**
     * Reject pushes to this branch if they are unsigned or unverifiable.
     */
    requireSignedCommits?: pulumi.Input<boolean>;
    /**
     * Allow only to merge pull request with enough positive reviews.
     */
    requiredApprovals?: pulumi.Input<number>;
    /**
     * Protected Branch Name Pattern
     */
    ruleName: pulumi.Input<string>;
    /**
     * Enter patterns to specify which status checks must pass before branches can be merged into a branch that matches this
     * rule. Each line specifies a pattern. Patterns cannot be empty.
     */
    statusCheckPatterns?: pulumi.Input<pulumi.Input<string>[]>;
    /**
     * Unprotected file patterns (separated using semicolon ';')
     */
    unprotectedFilePatterns?: pulumi.Input<string>;
    /**
     * User name or organization name
     */
    username: pulumi.Input<string>;
}
