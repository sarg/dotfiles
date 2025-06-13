import * as pulumi from "@pulumi/pulumi";
import * as openstack from "@pulumi/openstack";
import { SshKey } from ".";

export class Selectel extends pulumi.ComponentResource {
    constructor(
        name: string,
        args: { sshKey: SshKey },
        opts?: pulumi.ComponentResourceOptions,
    ) {
        super("components:index:Selectel", name, args, opts);
        const sshKeyKeypair = new openstack.compute.Keypair(
            `ssh_key`,
            { name: args.sshKey.name, publicKey: args.sshKey.key },
            { parent: this },
        );

        const _private = new openstack.networking.Network(
            `private`,
            { name: "nat" },
            { parent: this },
        );

        const external = openstack.networking.getNetworkOutput({
            external: true,
        });

        const router = new openstack.networking.Router(
            `router`,
            {
                name: "router-nat",
                externalNetworkId: external.apply((external) => external.id),
            },
            { parent: this },
        );

        const privateSubnet = new openstack.networking.Subnet(
            `private`,
            {
                networkId: _private.id,
                cidr: "192.168.0.0/24",
                dnsNameservers: ["188.93.16.19", "188.93.17.19"],
                enableDhcp: false,
            },
            { parent: _private },
        );

        const _interface = new openstack.networking.RouterInterface(
            `interface`,
            { routerId: router.id, subnetId: privateSubnet.id },
            { parent: router },
        );

        const vpn = new openstack.networking.Port(
            `vpn`,
            {
                networkId: _private.id,
                name: "vpn",
                fixedIps: [{ subnetId: privateSubnet.id }],
            },
            { parent: _private },
        );
    }
}
