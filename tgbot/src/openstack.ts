interface StringId {
	id: string;
}

type Config = {
	region: string;
	user: {
		name: string;
		domain: { name: string };
		password: string;
	};
};

export class Openstack {
	private baseUri: string;
	private authToken: Promise<string>;

	constructor(cfg: Config) {
		this.baseUri = `https://${cfg.region}.cloud.api.selcloud.ru`;
		this.authToken = this.auth(cfg.user);
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
		return fetch(`${this.baseUri}/image/v2/images?name=${name}`, {
			headers: {
				'X-Auth-Token': await this.authToken,
			},
		})
			.then((response) => response.json() as Promise<{ images: StringId[] }>)
			.then((json) => json.images[0]);
	}

	private async auth(user: object): Promise<string> {
		return await fetch('https://cloud.api.selcloud.ru/identity/v3/auth/tokens', {
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
							id: 'd7baf8ca4fe34f0a804f001f04a7e902',
						},
					},
				},
			}),
		}).then((response) => response.headers.get('X-Subject-Token')!);
	}

	async computeFindServer(name: string): Promise<StringId | undefined> {
		return fetch(`${this.baseUri}/compute/v2.1/servers?name=${name}`, {
			headers: {
				'Content-Type': 'application/json',
				'X-Auth-Token': await this.authToken,
			},
		})
			.then((response) => response.json() as Promise<{ servers: StringId[] }>)
			.then((json) => json.servers.at(0));
	}

	async computeCreateServer(server: object): Promise<void> {
		fetch(`${this.baseUri}/compute/v2.1/servers`, {
			method: 'POST',
			headers: {
				'Content-Type': 'application/json',
				'X-Auth-Token': await this.authToken,
				'OpenStack-API-Version': 'compute 2.67',
			},
			body: JSON.stringify({ server }),
		});
	}

	async computeDeleteServer(id: string): Promise<void> {
		fetch(`${this.baseUri}/compute/v2.1/servers/${id}`, {
			method: 'DELETE',
			headers: {
				'Content-Type': 'application/json',
				'X-Auth-Token': await this.authToken,
			},
		});
	}
}
