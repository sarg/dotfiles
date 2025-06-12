const baseUri = 'https://api.hetzner.cloud/v1';

export class Hetzner {
	private apiToken: string;

	constructor(apiToken: string) {
		this.apiToken = apiToken;
	}

	async createServer(s: object) {
		return await fetch(`${baseUri}/servers`, {
			method: 'POST',
			headers: {
				Authorization: `Bearer ${this.apiToken}`,
				'Content-Type': 'application/json',
			},
			body: JSON.stringify(s),
		});
	}

	async findServer(name: string) {
		return await fetch(`${baseUri}/servers?name=${name}`, {
			headers: {
				Authorization: `Bearer ${this.apiToken}`,
			},
		});
	}

	async deleteServer(id: string) {
		return await fetch(`https://api.hetzner.cloud/v1/servers/${id}`, {
			method: 'DELETE',
			headers: {
				Authorization: `Bearer ${this.apiToken}`,
				'Content-Type': 'application/json',
			},
		});
	}
}
