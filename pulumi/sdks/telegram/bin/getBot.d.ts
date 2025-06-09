import * as pulumi from "@pulumi/pulumi";
export declare function getBot(args?: GetBotArgs, opts?: pulumi.InvokeOptions): Promise<GetBotResult>;
/**
 * A collection of arguments for invoking getBot.
 */
export interface GetBotArgs {
    id?: string;
}
/**
 * A collection of values returned by getBot.
 */
export interface GetBotResult {
    readonly id: string;
    readonly name: string;
    readonly userId: number;
    readonly username: string;
}
export declare function getBotOutput(args?: GetBotOutputArgs, opts?: pulumi.InvokeOutputOptions): pulumi.Output<GetBotResult>;
/**
 * A collection of arguments for invoking getBot.
 */
export interface GetBotOutputArgs {
    id?: pulumi.Input<string>;
}
