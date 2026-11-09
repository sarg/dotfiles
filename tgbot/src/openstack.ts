interface StringId {
	id: string;
}

export interface PublicPort extends StringId {
	network_id: string;
	ip_address: string;
}

type Config = {
	region: string;
	projectId: string;
	user: {
		name: string;
		domain: { name: string };
		password: string;
	};
};

export class Openstack {
	private baseUri: string;
	private publicNetworkUri: string;
	private projectId: string;
	private authToken: Promise<string>;

	constructor(cfg: Config) {
		this.baseUri = `https://${cfg.region}.cloud.api.selcloud.ru`;
		this.publicNetworkUri = `https://${cfg.region}.cloud.api.selcloud.ru/public-network`;
		this.projectId = cfg.projectId;
		this.authToken = this.auth(cfg.user);
	}

	async networkCreatePublicPort(description: string): Promise<PublicPort> {
		const response = await fetch(`${this.publicNetworkUri}/v1/public_ports`, {
			method: 'POST',
			headers: {
				'Content-Type': 'application/json',
				'X-Auth-Token': await this.authToken,
			},
			body: JSON.stringify({ project_id: this.projectId, description }),
		});

		if (!response.ok) {
			throw new Error(`Failed to create public port: ${response.status} ${await response.text()}`);
		}

		const json = (await response.json()) as { port: PublicPort };
		return json.port;
	}

	async networkFindPublicPorts(description: string): Promise<PublicPort[]> {
		const response = await fetch(
			`${this.publicNetworkUri}/v1/public_ports?description=${encodeURIComponent(description)}`,
			{
				headers: {
					'X-Auth-Token': await this.authToken,
				},
			},
		);

		if (!response.ok) {
			throw new Error(`Failed to list public ports: ${response.status} ${await response.text()}`);
		}

		const json = (await response.json()) as { ports: PublicPort[] };
		return json.ports;
	}

	async networkDeletePublicPort(id: string, force = false): Promise<Response> {
		const query = force ? '?force=true' : '';
		return fetch(`${this.publicNetworkUri}/v1/public_ports/${id}${query}`, {
			method: 'DELETE',
			headers: {
				'X-Auth-Token': await this.authToken,
			},
		});
	}

	async networkFindPort(name: string): Promise<StringId> {
		return fetch(`${this.baseUri}/network/v2.0/ports?name=${name}&fields=id`, {
			headers: {
				'X-Auth-Token': await this.authToken,
			},
		})
			.then((response) => response.json() as Promise<{ ports: StringId[] }>)
			.then((json) => json.ports[0]);
	}

	async networkFindFloatingIp(): Promise<StringId> {
		return fetch(`${this.baseUri}/network/v2.0/floatingips`, {
			headers: {
				'X-Auth-Token': await this.authToken,
			},
		})
			.then((response) => response.json() as Promise<{ floatingips: StringId[] }>)
			.then((json) => json.floatingips[0]);
	}

	async networkDeleteFloatingIp(id: string) {
		return fetch(`${this.baseUri}/network/v2.0/floatingips/${id}`, {
			method: 'DELETE',
			headers: {
				'X-Auth-Token': await this.authToken,
			},
		});
	}

	async networkCreateIp(floatingip: object): Promise<StringId> {
		return fetch(`${this.baseUri}/network/v2.0/floatingips`, {
			method: 'POST',
			headers: {
				'X-Auth-Token': await this.authToken,
			},
			body: JSON.stringify({ floatingip }),
		})
			.then((response) => response.json() as Promise<{ floatingip: StringId }>)
			.then((json) => json.floatingip);
	}

	async networkFindNetwork(name: string): Promise<StringId> {
		return fetch(`${this.baseUri}/network/v2.0/networks?name=${name}&fields=id`, {
			headers: {
				'X-Auth-Token': await this.authToken,
			},
		})
			.then((response) => response.json() as Promise<{ networks: StringId[] }>)
			.then((json) => json.networks[0]);
	}

	async imageFindImage(name: string): Promise<StringId> {
		const response = await fetch(`${this.baseUri}/image/v2/images?name=${encodeURIComponent(name)}`, {
			headers: {
				'X-Auth-Token': await this.authToken,
			},
		});

		if (!response.ok) {
			throw new Error(`Failed to find image "${name}": ${response.status} ${await response.text()}`);
		}

		const json = (await response.json()) as { images: StringId[] };
		const image = json.images[0];
		if (!image) throw new Error(`Image "${name}" not found`);

		return image;
	}

	private async auth(user: object): Promise<string> {
		const response = await fetch('https://cloud.api.selcloud.ru/identity/v3/auth/tokens', {
			method: 'POST',
			headers: {
				'Content-Type': 'application/json',
			},
			body: JSON.stringify({
				auth: {
					identity: {
						methods: ['password'],
						password: {
							user: user,
						},
					},
					scope: {
						project: {
							id: this.projectId,
						},
					},
				},
			}),
		});

		const token = response.headers.get('X-Subject-Token');
		if (!response.ok || !token) {
			throw new Error(`Selectel authentication failed: ${response.status} ${await response.text()}`);
		}

		return token;
	}

	async computeFindServer(name: string): Promise<StringId | undefined> {
		const response = await fetch(`${this.baseUri}/compute/v2.1/servers?name=${encodeURIComponent(name)}`, {
			headers: {
				'Content-Type': 'application/json',
				'X-Auth-Token': await this.authToken,
			},
		});

		if (!response.ok) {
			throw new Error(`Failed to find server "${name}": ${response.status} ${await response.text()}`);
		}

		const json = (await response.json()) as { servers: StringId[] };
		return json.servers.at(0);
	}

	async computeCreateServer(server: object): Promise<Response> {
		return fetch(`${this.baseUri}/compute/v2.1/servers`, {
			method: 'POST',
			headers: {
				'Content-Type': 'application/json',
				'X-Auth-Token': await this.authToken,
				'OpenStack-API-Version': 'compute 2.67',
			},
			body: JSON.stringify({ server }),
		});
	}

	async computeDeleteServer(id: string): Promise<Response> {
		return fetch(`${this.baseUri}/compute/v2.1/servers/${id}`, {
			method: 'DELETE',
			headers: {
				'Content-Type': 'application/json',
				'X-Auth-Token': await this.authToken,
			},
		});
	}
}
