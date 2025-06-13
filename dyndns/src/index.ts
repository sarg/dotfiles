import Cloudflare from 'cloudflare';
import { env } from 'cloudflare:workers';

const zoneId = 'dc878e614aeb6a02dc27a40ea5faf24b';
const zone = 'sarg.org.ru';
const client = new Cloudflare({
	apiToken: env.CLOUDFLARE_API_TOKEN,
});

async function setIp(subdomain: string, ip: string) {
	const type = ip.includes(':') ? 'AAAA' : 'A';
	const name = `${subdomain}.${zone}`;
	const list = await client.dns.records.list({
		zone_id: zoneId,
		type,
		name: { exact: name },
	});

	if (list.result.length > 0) {
		return client.dns.records.edit(list.result[0].id, {
			zone_id: zoneId,
			content: ip,
		});
	} else {
		return client.dns.records.create({
			zone_id: zoneId,
			name,
			type,
			ttl: 60,
			content: ip,
		});
	}
}

async function clean(subdomain: string) {
	const r = await client.dns.records.list({
		zone_id: zoneId,
		name: { exact: `${subdomain}.${zone}` },
	});

	if (r.result.length > 0)
		client.dns.records.batch({
			zone_id: zoneId,
			deletes: r.result.map((record) => ({ id: record.id })),
		});
}

export default {
	async fetch(request, env, ctx): Promise<Response> {
		const ip = request.headers.get('CF-Connecting-IP')!;
		const path = new URL(request.url).pathname;
		const m = path.match(/^\/(\w+)\/(\w+)$/);
		if (!m) return new Response(JSON.stringify({ origin: ip }), { status: 200 });

		const [_, cmd, token] = m;
		const name = await env.DYNDNS_TOKENS.get(token);
		if (!name) return new Response('Wrong token', { status: 403 });

		switch (cmd) {
			case 'set':
				await setIp(name, ip);
				return new Response(`${name} ip set to: ${ip}`, { status: 200 });

			case 'rm':
				await clean(name);
				return new Response(`${name} removed`, { status: 200 });
		}

		return new Response(`Unknown ${cmd}`, { status: 500 });
	},
} satisfies ExportedHandler<Env>;
