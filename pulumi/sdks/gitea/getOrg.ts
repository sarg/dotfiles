// *** WARNING: this file was generated by pulumi-language-nodejs. ***
// *** Do not edit by hand unless you're certain you know what you are doing! ***

import * as pulumi from "@pulumi/pulumi";
import * as utilities from "./utilities";

export function getOrg(args?: GetOrgArgs, opts?: pulumi.InvokeOptions): Promise<GetOrgResult> {
    args = args || {};
    opts = pulumi.mergeOptions(utilities.resourceOptsDefaults(), opts || {});
    return pulumi.runtime.invoke("gitea:index/getOrg:getOrg", {
        "name": args.name,
    }, opts, utilities.getPackage());
}

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
export function getOrgOutput(args?: GetOrgOutputArgs, opts?: pulumi.InvokeOutputOptions): pulumi.Output<GetOrgResult> {
    args = args || {};
    opts = pulumi.mergeOptions(utilities.resourceOptsDefaults(), opts || {});
    return pulumi.runtime.invokeOutput("gitea:index/getOrg:getOrg", {
        "name": args.name,
    }, opts, utilities.getPackage());
}

/**
 * A collection of arguments for invoking getOrg.
 */
export interface GetOrgOutputArgs {
    name?: pulumi.Input<string>;
}
