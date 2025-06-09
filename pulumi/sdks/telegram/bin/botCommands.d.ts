import * as pulumi from "@pulumi/pulumi";
export declare class BotCommands extends pulumi.CustomResource {
    /**
     * Get an existing BotCommands resource's state with the given name, ID, and optional extra
     * properties used to qualify the lookup.
     *
     * @param name The _unique_ name of the resulting resource.
     * @param id The _unique_ provider ID of the resource to lookup.
     * @param state Any extra arguments used during the lookup.
     * @param opts Optional settings to control the behavior of the CustomResource.
     */
    static get(name: string, id: pulumi.Input<pulumi.ID>, state?: BotCommandsState, opts?: pulumi.CustomResourceOptions): BotCommands;
    /**
     * Returns true if the given object is an instance of BotCommands.  This is designed to work even
     * when multiple copies of the Pulumi SDK have been loaded into the same process.
     */
    static isInstance(obj: any): obj is BotCommands;
    readonly botCommandsId: pulumi.Output<string>;
    readonly commands: pulumi.Output<{
        [key: string]: string;
    }[]>;
    /**
     * Create a BotCommands resource with the given unique name, arguments, and options.
     *
     * @param name The _unique_ name of the resource.
     * @param args The arguments to use to populate this resource's properties.
     * @param opts A bag of options that control this resource's behavior.
     */
    constructor(name: string, args: BotCommandsArgs, opts?: pulumi.CustomResourceOptions);
}
/**
 * Input properties used for looking up and filtering BotCommands resources.
 */
export interface BotCommandsState {
    botCommandsId?: pulumi.Input<string>;
    commands?: pulumi.Input<pulumi.Input<{
        [key: string]: pulumi.Input<string>;
    }>[]>;
}
/**
 * The set of arguments for constructing a BotCommands resource.
 */
export interface BotCommandsArgs {
    botCommandsId?: pulumi.Input<string>;
    commands: pulumi.Input<pulumi.Input<{
        [key: string]: pulumi.Input<string>;
    }>[]>;
}
