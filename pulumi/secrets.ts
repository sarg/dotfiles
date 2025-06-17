// To parse this data:
//
//   import { Convert, Secrets } from "./file";
//
//   const secrets = Convert.toSecrets(json);
//
// These functions will throw an error if the JSON doesn't
// match the expected interface, even if the JSON is valid.

export interface Secrets {
    telegram:   Telegram;
    hetzner:    Hetzner;
    selectel:   Selectel;
    vpn:        VPN;
    cloudflare: Cloudflare;
    google:     Google;
    pulumi:     Pulumi;
    dyndns:     Dyndn[];
}

export interface Cloudflare {
    API_TOKEN:  string;
    ACCOUNT_ID: string;
}

export interface Dyndn {
    token: string;
    name:  string;
}

export interface Google {
    ORG_ID:      string;
    CREDENTIALS: string;
}

export interface Hetzner {
    API_TOKEN:       string;
    PRIMARY_IPV6_ID: string;
}

export interface Pulumi {
    PULUMI_CONFIG_PASSPHRASE: string;
    CODEBERG_API_TOKEN:       string;
    GITHUB_API_TOKEN:         string;
}

export interface Selectel {
    PASS:   string;
    ORG_ID: string;
}

export interface Telegram {
    BOT_TOKEN: string;
}

export interface VPN {
    WG_CLIENT:  string;
    WG_PRIVATE: string;
    WG_PSK:     string;
}

// Converts JSON strings to/from your types
// and asserts the results of JSON.parse at runtime
export class Convert {
    public static toSecrets(json: string): Secrets {
        return cast(JSON.parse(json), r("Secrets"));
    }

    public static secretsToJson(value: Secrets): string {
        return JSON.stringify(uncast(value, r("Secrets")), null, 2);
    }
}

function invalidValue(typ: any, val: any, key: any, parent: any = ''): never {
    const prettyTyp = prettyTypeName(typ);
    const parentText = parent ? ` on ${parent}` : '';
    const keyText = key ? ` for key "${key}"` : '';
    throw Error(`Invalid value${keyText}${parentText}. Expected ${prettyTyp} but got ${JSON.stringify(val)}`);
}

function prettyTypeName(typ: any): string {
    if (Array.isArray(typ)) {
        if (typ.length === 2 && typ[0] === undefined) {
            return `an optional ${prettyTypeName(typ[1])}`;
        } else {
            return `one of [${typ.map(a => { return prettyTypeName(a); }).join(", ")}]`;
        }
    } else if (typeof typ === "object" && typ.literal !== undefined) {
        return typ.literal;
    } else {
        return typeof typ;
    }
}

function jsonToJSProps(typ: any): any {
    if (typ.jsonToJS === undefined) {
        const map: any = {};
        typ.props.forEach((p: any) => map[p.json] = { key: p.js, typ: p.typ });
        typ.jsonToJS = map;
    }
    return typ.jsonToJS;
}

function jsToJSONProps(typ: any): any {
    if (typ.jsToJSON === undefined) {
        const map: any = {};
        typ.props.forEach((p: any) => map[p.js] = { key: p.json, typ: p.typ });
        typ.jsToJSON = map;
    }
    return typ.jsToJSON;
}

function transform(val: any, typ: any, getProps: any, key: any = '', parent: any = ''): any {
    function transformPrimitive(typ: string, val: any): any {
        if (typeof typ === typeof val) return val;
        return invalidValue(typ, val, key, parent);
    }

    function transformUnion(typs: any[], val: any): any {
        // val must validate against one typ in typs
        const l = typs.length;
        for (let i = 0; i < l; i++) {
            const typ = typs[i];
            try {
                return transform(val, typ, getProps);
            } catch (_) {}
        }
        return invalidValue(typs, val, key, parent);
    }

    function transformEnum(cases: string[], val: any): any {
        if (cases.indexOf(val) !== -1) return val;
        return invalidValue(cases.map(a => { return l(a); }), val, key, parent);
    }

    function transformArray(typ: any, val: any): any {
        // val must be an array with no invalid elements
        if (!Array.isArray(val)) return invalidValue(l("array"), val, key, parent);
        return val.map(el => transform(el, typ, getProps));
    }

    function transformDate(val: any): any {
        if (val === null) {
            return null;
        }
        const d = new Date(val);
        if (isNaN(d.valueOf())) {
            return invalidValue(l("Date"), val, key, parent);
        }
        return d;
    }

    function transformObject(props: { [k: string]: any }, additional: any, val: any): any {
        if (val === null || typeof val !== "object" || Array.isArray(val)) {
            return invalidValue(l(ref || "object"), val, key, parent);
        }
        const result: any = {};
        Object.getOwnPropertyNames(props).forEach(key => {
            const prop = props[key];
            const v = Object.prototype.hasOwnProperty.call(val, key) ? val[key] : undefined;
            result[prop.key] = transform(v, prop.typ, getProps, key, ref);
        });
        Object.getOwnPropertyNames(val).forEach(key => {
            if (!Object.prototype.hasOwnProperty.call(props, key)) {
                result[key] = transform(val[key], additional, getProps, key, ref);
            }
        });
        return result;
    }

    if (typ === "any") return val;
    if (typ === null) {
        if (val === null) return val;
        return invalidValue(typ, val, key, parent);
    }
    if (typ === false) return invalidValue(typ, val, key, parent);
    let ref: any = undefined;
    while (typeof typ === "object" && typ.ref !== undefined) {
        ref = typ.ref;
        typ = typeMap[typ.ref];
    }
    if (Array.isArray(typ)) return transformEnum(typ, val);
    if (typeof typ === "object") {
        return typ.hasOwnProperty("unionMembers") ? transformUnion(typ.unionMembers, val)
            : typ.hasOwnProperty("arrayItems")    ? transformArray(typ.arrayItems, val)
            : typ.hasOwnProperty("props")         ? transformObject(getProps(typ), typ.additional, val)
            : invalidValue(typ, val, key, parent);
    }
    // Numbers can be parsed by Date but shouldn't be.
    if (typ === Date && typeof val !== "number") return transformDate(val);
    return transformPrimitive(typ, val);
}

function cast<T>(val: any, typ: any): T {
    return transform(val, typ, jsonToJSProps);
}

function uncast<T>(val: T, typ: any): any {
    return transform(val, typ, jsToJSONProps);
}

function l(typ: any) {
    return { literal: typ };
}

function a(typ: any) {
    return { arrayItems: typ };
}

function u(...typs: any[]) {
    return { unionMembers: typs };
}

function o(props: any[], additional: any) {
    return { props, additional };
}

function m(additional: any) {
    return { props: [], additional };
}

function r(name: string) {
    return { ref: name };
}

const typeMap: any = {
    "Secrets": o([
        { json: "telegram", js: "telegram", typ: r("Telegram") },
        { json: "hetzner", js: "hetzner", typ: r("Hetzner") },
        { json: "selectel", js: "selectel", typ: r("Selectel") },
        { json: "vpn", js: "vpn", typ: r("VPN") },
        { json: "cloudflare", js: "cloudflare", typ: r("Cloudflare") },
        { json: "google", js: "google", typ: r("Google") },
        { json: "pulumi", js: "pulumi", typ: r("Pulumi") },
        { json: "dyndns", js: "dyndns", typ: a(r("Dyndn")) },
    ], false),
    "Cloudflare": o([
        { json: "API_TOKEN", js: "API_TOKEN", typ: "" },
        { json: "ACCOUNT_ID", js: "ACCOUNT_ID", typ: "" },
    ], false),
    "Dyndn": o([
        { json: "token", js: "token", typ: "" },
        { json: "name", js: "name", typ: "" },
    ], false),
    "Google": o([
        { json: "ORG_ID", js: "ORG_ID", typ: "" },
        { json: "CREDENTIALS", js: "CREDENTIALS", typ: "" },
    ], false),
    "Hetzner": o([
        { json: "API_TOKEN", js: "API_TOKEN", typ: "" },
        { json: "PRIMARY_IPV6_ID", js: "PRIMARY_IPV6_ID", typ: "" },
    ], false),
    "Pulumi": o([
        { json: "PULUMI_CONFIG_PASSPHRASE", js: "PULUMI_CONFIG_PASSPHRASE", typ: "" },
        { json: "CODEBERG_API_TOKEN", js: "CODEBERG_API_TOKEN", typ: "" },
        { json: "GITHUB_API_TOKEN", js: "GITHUB_API_TOKEN", typ: "" },
    ], false),
    "Selectel": o([
        { json: "PASS", js: "PASS", typ: "" },
        { json: "ORG_ID", js: "ORG_ID", typ: "" },
    ], false),
    "Telegram": o([
        { json: "BOT_TOKEN", js: "BOT_TOKEN", typ: "" },
    ], false),
    "VPN": o([
        { json: "WG_CLIENT", js: "WG_CLIENT", typ: "" },
        { json: "WG_PRIVATE", js: "WG_PRIVATE", typ: "" },
        { json: "WG_PSK", js: "WG_PSK", typ: "" },
    ], false),
};
