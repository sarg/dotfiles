import Cloudflare from 'cloudflare';
import { env } from 'cloudflare:workers';

class Dyndns {
	private client: Cloudflare;
	private zone: Promise<Cloudflare.Zones.Zone>;

	constructor(zoneName: string) {
		this.client = new Cloudflare({ apiToken: env.CLOUDFLARE_API_TOKEN });
		this.zone = this.client.zones.list({ name: zoneName }).then((r) => r.result[0]);
	}

	async setIp(subdomain: string, ips: string[]) {
		const zone = await this.zone;
		const name = `${subdomain}.${zone.name}`;
		const records = (
			await this.client.dns.records.list({ zone_id: zone.id, name: { exact: name } })
		).result.filter((r) => r.type === 'A' || r.type === 'AAAA');

		const updates = [];
		const creates = [];

		for (const ip of ips) {
			const type: 'A' | 'AAAA' = ip.includes(':') ? 'AAAA' : 'A';
			const existing = records.find((r) => r.type === type);
			if (existing) {
				updates.push({ id: existing.id, content: ip });
			} else {
				creates.push({ name, type, ttl: 60, content: ip });
			}
		}

		return this.client.dns.records.batch({
			zone_id: zone.id,
			posts: creates,
			patches: updates,
		});
	}

	async clean(subdomain: string) {
		const zone = await this.zone;
		const r = await this.client.dns.records.list({
			zone_id: zone.id,
			name: { exact: `${subdomain}.${zone.name}` },
		});

		if (r.result.length > 0)
			return this.client.dns.records.batch({
				zone_id: zone.id,
				deletes: r.result.map((record) => ({ id: record.id })),
			});
	}
}

export default {
	async fetch(request, env, ctx): Promise<Response> {
		const url = new URL(request.url);
		const ip = request.headers.get('CF-Connecting-IP')!;
		const m = url.pathname.match(/^\/(\w+)$/);
		if (!m) return new Response(JSON.stringify({ origin: ip }), { status: 200 });

		const [_, token] = m;
		const name = await env.DYNDNS_TOKENS.get(token);
		if (!name) return new Response('Wrong token', { status: 403 });

		switch (request.method) {
			case 'POST':
			case 'GET':
				const ipList = [url.searchParams.get('ipv4'), url.searchParams.get('ipv6')].filter(
					(v) => v !== null,
				);
				if (ipList.length == 0) ipList.push(ip);
				await new Dyndns(env.DOMAIN).setIp(name, ipList);
				return new Response(`${name} ip set to: ${ipList}`, { status: 200 });

			case 'DELETE':
				await new Dyndns(env.DOMAIN).clean(name);
				return new Response(`${name} removed`, { status: 200 });
		}

		return new Response(`${request.method} not supported`, { status: 500 });
	},
} satisfies ExportedHandler<Env>;
