import Cloudflare from 'cloudflare';
import { env } from 'cloudflare:workers';

class Dyndns {
	private client: Cloudflare;
	private zone: Promise<Cloudflare.Zones.Zone>;

	constructor(zoneName: string) {
		this.client = new Cloudflare({ apiToken: env.CLOUDFLARE_API_TOKEN });
		this.zone = this.client.zones.list({ name: zoneName }).then((r) => r.result[0]);
	}

	async setIp(subdomain: string, ip: string) {
		const zone = await this.zone;
		const type = ip.includes(':') ? 'AAAA' : 'A';
		const name = `${subdomain}.${zone.name}`;
		const list = await this.client.dns.records.list({
			zone_id: zone.id,
			type,
			name: { exact: name },
		});

		if (list.result.length > 0) {
			return this.client.dns.records.edit(list.result[0].id, {
				zone_id: zone.id,
				content: ip,
			});
		} else {
			return this.client.dns.records.create({
				zone_id: zone.id,
				name,
				type,
				ttl: 60,
				content: ip,
			});
		}
	}

	async clean(subdomain: string) {
		const zone = await this.zone;
		const r = await this.client.dns.records.list({
			zone_id: zone.id,
			name: { exact: `${subdomain}.${zone.name}` },
		});

		if (r.result.length > 0)
			this.client.dns.records.batch({
				zone_id: zone.id,
				deletes: r.result.map((record) => ({ id: record.id })),
			});
	}
}

export default {
	async fetch(request, env, ctx): Promise<Response> {
		const url = new URL(request.url);
		const ip = request.headers.get('CF-Connecting-IP')!;
		const m = url.pathname.match(/^\/(\w+)\/(\w+)$/);
		if (!m) return new Response(JSON.stringify({ origin: ip }), { status: 200 });

		const [_, cmd, token] = m;
		const name = await env.DYNDNS_TOKENS.get(token);
		if (!name) return new Response('Wrong token', { status: 403 });

		const zoneName = url.host.substring(url.host.indexOf('.') + 1);
		const dyndns = new Dyndns(zoneName);

		switch (cmd) {
			case 'set':
				const ipList = [url.searchParams.get('ipv4'), url.searchParams.get('ipv6')].filter((v) => v !== null);
				if (ipList.length == 0) ipList.push(ip);
				await Promise.all(ipList.map((ip) => dyndns.setIp(name, ip)));
				return new Response(`${name} ip set to: ${ipList}`, { status: 200 });

			case 'rm':
				await dyndns.clean(name);
				return new Response(`${name} removed`, { status: 200 });
		}

		return new Response(`Unknown ${cmd}`, { status: 500 });
	},
} satisfies ExportedHandler<Env>;
