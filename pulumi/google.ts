import * as pulumi from '@pulumi/pulumi';
import * as gcp from '@pulumi/gcp';

export class Google extends pulumi.ComponentResource {
  serviceAccountKey: gcp.serviceaccount.Key;
  gptelKey: gcp.projects.ApiKey;

  constructor(name: string, args: { orgId: string }, opts?: pulumi.ComponentResourceOptions) {
    super('components:index:Google', name, args, opts);
    const project = new gcp.organizations.Project(
      `default`,
      { name: 'Terraform Project', orgId: args.orgId },
      { parent: this },
    );

    const serviceAccount = new gcp.serviceaccount.Account(
      'pulumi',
      { project: project.projectId, displayName: 'Pulumi Deployer' },
      { parent: project },
    );

    const pulumiAdmin = new gcp.projects.IAMBinding(
      'admin',
      {
        role: 'roles/admin',
        project: project.projectId,
        members: [pulumi.interpolate`serviceAccount:${serviceAccount.email}`],
      },
      { parent: serviceAccount },
    );

    this.serviceAccountKey = new gcp.serviceaccount.Key(
      'key',
      { serviceAccountId: serviceAccount.name, publicKeyType: 'TYPE_X509_PEM_FILE' },
      { parent: serviceAccount, additionalSecretOutputs: ['privateKey'] },
    );

    const servicesService: gcp.projects.Service[] = [
      'generativelanguage.googleapis.com',
      'iam.googleapis.com',
      'cloudresourcemanager.googleapis.com',
      'apikeys.googleapis.com',
      'serviceusage.googleapis.com',
    ].map(
      (val) =>
        new gcp.projects.Service(
          `services-${val}`,
          { project: project.projectId, service: val },
          { parent: project },
        ),
    );

    this.gptelKey = new gcp.projects.ApiKey(
      `gptel`,
      {
        displayName: 'gptel.el',
        project: project.projectId,
        restrictions: {
          apiTargets: [{ service: 'generativelanguage.googleapis.com' }],
        },
      },
      { parent: project, dependsOn: servicesService },
    );
  }
}
