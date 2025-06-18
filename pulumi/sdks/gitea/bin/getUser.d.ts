import * as pulumi from "@pulumi/pulumi";
export declare function getUser(args?: GetUserArgs, opts?: pulumi.InvokeOptions): Promise<GetUserResult>;
/**
 * A collection of arguments for invoking getUser.
 */
export interface GetUserArgs {
    username?: string;
}
/**
 * A collection of values returned by getUser.
 */
export interface GetUserResult {
    readonly avatarUrl: string;
    readonly created: string;
    readonly email: string;
    readonly fullName: string;
    readonly id: number;
    readonly isAdmin: boolean;
    readonly language: string;
    readonly lastLogin: string;
    readonly username: string;
}
export declare function getUserOutput(args?: GetUserOutputArgs, opts?: pulumi.InvokeOutputOptions): pulumi.Output<GetUserResult>;
/**
 * A collection of arguments for invoking getUser.
 */
export interface GetUserOutputArgs {
    username?: pulumi.Input<string>;
}
