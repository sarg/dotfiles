"use strict";
// *** WARNING: this file was generated by pulumi-language-nodejs. ***
// *** Do not edit by hand unless you're certain you know what you are doing! ***
Object.defineProperty(exports, "__esModule", { value: true });
const pulumi = require("@pulumi/pulumi");
const __config = new pulumi.Config("gitea");
Object.defineProperty(exports, "baseUrl", {
    get() {
        return __config.get("baseUrl");
    },
    enumerable: true,
});
Object.defineProperty(exports, "cacertFile", {
    get() {
        return __config.get("cacertFile");
    },
    enumerable: true,
});
Object.defineProperty(exports, "insecure", {
    get() {
        return __config.getObject("insecure");
    },
    enumerable: true,
});
Object.defineProperty(exports, "password", {
    get() {
        return __config.get("password");
    },
    enumerable: true,
});
Object.defineProperty(exports, "token", {
    get() {
        return __config.get("token");
    },
    enumerable: true,
});
Object.defineProperty(exports, "username", {
    get() {
        return __config.get("username");
    },
    enumerable: true,
});
//# sourceMappingURL=vars.js.map