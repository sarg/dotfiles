const baseUri = 'https://ru-9.cloud.api.selcloud.ru';
var authToken!: string;

interface StringId {
	id: string;
}

export async function networkFindPort(name: string): Promise<StringId> {
	return fetch(`${baseUri}/network/v2.0/ports?name=${name}&fields=id`, {
		headers: {
			'X-Auth-Token': authToken,
		},
	})
		.then((response) => response.json() as Promise<{ ports: StringId[] }>)
		.then((json) => json.ports[0]);
}

export async function networkFindFloatingIp(): Promise<StringId> {
	return fetch(`${baseUri}/network/v2.0/floatingips`, {
		headers: {
			'X-Auth-Token': authToken,
		},
	})
		.then((response) => response.json() as Promise<{ floatingips: StringId[] }>)
		.then((json) => json.floatingips[0]);
}

export async function networkDeleteFloatingIp(id: string) {
	return fetch(`${baseUri}/network/v2.0/floatingips/${id}`, {
		method: 'DELETE',
		headers: {
			'X-Auth-Token': authToken,
		},
	});
}

export async function networkCreateIp(floatingip: object): Promise<StringId> {
	return fetch(`${baseUri}/network/v2.0/floatingips`, {
		method: 'POST',
		headers: {
			'X-Auth-Token': authToken,
		},
		body: JSON.stringify({ floatingip }),
	})
		.then((response) => response.json() as Promise<{ floatingip: StringId }>)
		.then((json) => json.floatingip);
}

export async function networkFindNetwork(name: string): Promise<StringId> {
	return fetch(`${baseUri}/network/v2.0/networks?name=${name}&fields=id`, {
		headers: {
			'X-Auth-Token': authToken,
		},
	})
		.then((response) => response.json() as Promise<{ networks: StringId[] }>)
		.then((json) => json.networks[0]);
}

export async function imageFindImage(name: string): Promise<StringId> {
	return fetch(`${baseUri}/image/v2/images?name=${name}`, {
		headers: {
			'X-Auth-Token': authToken,
		},
	})
		.then((response) => response.json() as Promise<{ images: StringId[] }>)
		.then((json) => json.images[0]);
}

export async function auth(user: {
	name: string;
	domain: { name: string };
	password: string;
}): Promise<void> {
	authToken = await fetch('https://cloud.api.selcloud.ru/identity/v3/auth/tokens', {
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

export async function computeFindServer(name: string): Promise<StringId | undefined> {
	return fetch(`${baseUri}/compute/v2.1/servers?name=${name}`, {
		headers: {
			'Content-Type': 'application/json',
			'X-Auth-Token': authToken,
		},
	})
		.then((response) => response.json() as Promise<{ servers: StringId[] }>)
		.then((json) => json.servers.at(0));
}

export async function computeCreateServer(server: object): Promise<void> {
	fetch(`${baseUri}/compute/v2.1/servers`, {
		method: 'POST',
		headers: {
			'Content-Type': 'application/json',
			'X-Auth-Token': authToken,
			'OpenStack-API-Version': 'compute 2.67',
		},
		body: JSON.stringify({ server }),
	});
}

export async function computeDeleteServer(id: string): Promise<void> {
	fetch(`${baseUri}/compute/v2.1/servers/${id}`, {
		method: 'DELETE',
		headers: {
			'Content-Type': 'application/json',
			'X-Auth-Token': authToken,
		},
	});
}
