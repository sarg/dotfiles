import * as pulumi from "@pulumi/pulumi";
export declare function getOrg(args?: GetOrgArgs, opts?: pulumi.InvokeOptions): Promise<GetOrgResult>;
/**
 * A collection of arguments for invoking getOrg.
 */
export interface GetOrgArgs {
    name?: string;
}
/**
 * A collection of values returned by getOrg.
 */
export interface GetOrgResult {
    readonly avatarUrl: string;
    readonly description: string;
    readonly fullName: string;
    readonly id: number;
    readonly location: string;
    readonly name: string;
    readonly visibility: string;
    readonly website: string;
}
export declare function getOrgOutput(args?: GetOrgOutputArgs, opts?: pulumi.InvokeOutputOptions): pulumi.Output<GetOrgResult>;
/**
 * A collection of arguments for invoking getOrg.
 */
export interface GetOrgOutputArgs {
    name?: pulumi.Input<string>;
}
