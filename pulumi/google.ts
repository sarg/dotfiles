import * as pulumi from "@pulumi/pulumi";
import * as gcp from "@pulumi/gcp";

export class Google extends pulumi.ComponentResource {
    constructor(
        name: string,
        args?: pulumi.Inputs,
        opts?: pulumi.ComponentResourceOptions,
    ) {
        super("components:index:Google", name, args, opts);
        const project = new gcp.organizations.Project(
            `default`,
            {
                name: "Terraform Project",
                orgId: process.env["GOOGLE_ORG_ID"]!,
            },
            { parent: this },
        );

        const servicesService: gcp.projects.Service[] = [
            "generativelanguage.googleapis.com",
            "cloudresourcemanager.googleapis.com",
            "apikeys.googleapis.com",
            "serviceusage.googleapis.com",
        ].map(
            (val) =>
                new gcp.projects.Service(
                    `services-${val}`,
                    { project: project.projectId, service: val },
                    { parent: this },
                ),
        );

        const gptel = new gcp.projects.ApiKey(
            `gptel`,
            {
                displayName: "gptel.el",
                project: project.projectId,
                restrictions: {
                    apiTargets: [
                        {
                            service: "generativelanguage.googleapis.com",
                        },
                    ],
                },
            },
            {
                parent: this,
                dependsOn: servicesService,
            },
        );

        this.registerOutputs({
            gptelApikey: gptel.keyString,
        });
    }
}
