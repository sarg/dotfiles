// *** WARNING: this file was generated by pulumi-language-nodejs. ***
// *** Do not edit by hand unless you're certain you know what you are doing! ***

import * as pulumi from "@pulumi/pulumi";
import * as utilities from "../utilities";

declare var exports: any;
const __config = new pulumi.Config("telegram");

export declare const botToken: string | undefined;
Object.defineProperty(exports, "botToken", {
    get() {
        return __config.get("botToken");
    },
    enumerable: true,
});

