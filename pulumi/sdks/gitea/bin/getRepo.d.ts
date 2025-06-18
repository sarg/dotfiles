import * as pulumi from "@pulumi/pulumi";
export declare function getRepo(args: GetRepoArgs, opts?: pulumi.InvokeOptions): Promise<GetRepoResult>;
/**
 * A collection of arguments for invoking getRepo.
 */
export interface GetRepoArgs {
    id?: string;
    name: string;
    username: string;
}
/**
 * A collection of values returned by getRepo.
 */
export interface GetRepoResult {
    readonly cloneUrl: string;
    readonly created: string;
    readonly defaultBranch: string;
    readonly description: string;
    readonly fork: boolean;
    readonly forks: number;
    readonly fullName: string;
    readonly htmlUrl: string;
    readonly id: string;
    readonly mirror: boolean;
    readonly name: string;
    readonly openIssueCount: number;
    readonly permissionAdmin: boolean;
    readonly permissionPull: boolean;
    readonly permissionPush: boolean;
    readonly private: boolean;
    readonly size: number;
    readonly sshUrl: string;
    readonly stars: number;
    readonly updated: string;
    readonly username: string;
    readonly watchers: number;
    readonly website: string;
}
export declare function getRepoOutput(args: GetRepoOutputArgs, opts?: pulumi.InvokeOutputOptions): pulumi.Output<GetRepoResult>;
/**
 * A collection of arguments for invoking getRepo.
 */
export interface GetRepoOutputArgs {
    id?: pulumi.Input<string>;
    name: pulumi.Input<string>;
    username: pulumi.Input<string>;
}
