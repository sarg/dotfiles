import { env } from 'cloudflare:workers';
import userDataTmpl from './hetzner.yaml';
import { Hetzner } from './hetzner';
import { Openstack } from './openstack';
import { randomBytes } from 'node:crypto';

interface Payload {
	message?: { chat: { id: number }; text?: string };
}
const openstackConfig = {
	region: 'ru-9',
	user: {
		name: 'openstack',
		domain: { name: env.SELECTEL_ORG_ID },
		password: env.SELECTEL_PASS,
	},
};

function respond(chatId: number, msg: string | object): Promise<Response> {
	const isText = typeof msg === 'string';
	return fetch(`https://api.telegram.org/bot${env.BOT_TOKEN}/sendMessage?chat_id=${chatId}`, {
		method: 'POST',
		headers: { 'Content-Type': 'application/json' },
		body: JSON.stringify(isText ? { text: msg } : msg),
	});
}

async function startHetzner(userData: string) {
	return new Hetzner(env.HCLOUD_TOKEN).createServer({
		name: 'vpn',
		location: 'hel1',
		server_type: 'cax11',
		start_after_create: true,
		image: 'ubuntu-24.04',
		ssh_keys: ['thinkpad'],
		public_net: {
			enable_ipv4: true,
			enable_ipv6: true,
			ipv6: parseInt(env.HETZNER_IPV6_ID), // vpn_ipv6
		},
		user_data: userData,
	});
}

async function stopHetzner() {
	const hetzner = new Hetzner(env.HCLOUD_TOKEN);
	const response = await hetzner.findServer('vpn');
	if (!response.ok) return false;

	const servers = ((await response.json()) as { servers: { id: string }[] }).servers;
	return servers[0] && hetzner.deleteServer(servers[0].id).then((r) => r.ok);
}

async function startSelectel(userData: string): Promise<void> {
	const openstack = new Openstack(openstackConfig);
	const [port, externalNetwork, image] = await Promise.all([
		openstack.networkFindPort('vpn'),
		openstack.networkFindNetwork('external-network'),
		openstack.imageFindImage('Ubuntu 24.04 LTS 64-bit'),
	]);

	openstack.computeCreateServer({
		name: 'vpn',
		flavorRef: '9011',
		networks: [{ port: port.id }],
		key_name: 'thinkpad',
		availability_zone: 'ru-9a',
		user_data: btoa(userData),
		block_device_mapping_v2: [
			{
				uuid: image.id,
				boot_index: 0,
				source_type: 'image',
				volume_type: 'basic.ru-9a',
				destination_type: 'volume',
				volume_size: 5,
				delete_on_termination: true,
			},
		],
	});

	await openstack.networkCreateIp({
		floating_network_id: externalNetwork.id,
		port_id: port.id,
	});
}

async function stopSelectel(): Promise<boolean> {
	const openstack = new Openstack(openstackConfig);
	const server = await openstack.computeFindServer('vpn');
	if (!server) return false;
	openstack.computeDeleteServer(server.id);
	const floatingIp = await openstack.networkFindFloatingIp();
	await openstack.networkDeleteFloatingIp(floatingIp.id);
	return true;
}

function prepareUserData(chatId: number): string {
	return userDataTmpl
		.replace('[input:CHAT_ID]', chatId.toString())
		.replaceAll(/\[secret:([^\]]+)\]/g, (_, s) => process.env[s]!);
}

async function vpnHandler(chatId: number, args: string[]) {
	if (args.length === 0) {
		await respond(
			chatId,
			{
				text: 'Please choose a VPN action:',
				reply_markup: {
					keyboard: [
						[{ text: '/vpn de' }],
						[{ text: '/vpn ru' }],
						[{ text: '/vpn stop' }],
					],
					resize_keyboard: true,
					one_time_keyboard: true,
				},
			},
		);
		return;
	}

	switch (args[0]) {
		case 'de':
			var response = await startHetzner(prepareUserData(chatId));
			if (!response.ok) {
				console.log({ response });
				await respond(chatId, 'Error creating VPN');
			} else {
				await respond(chatId, 'Creating hetzner VPN');
			}
			break;
		case 'ru':
			await startSelectel(prepareUserData(chatId));
			await respond(chatId, 'Starting selectel VPN');
			break;
		case 'stop':
			if (await stopHetzner()) {
				await respond(chatId, 'Stopped hetzner VPN');
			} else if (await stopSelectel()) {
				await respond(chatId, 'Stopped selectel VPN');
			} else {
				await respond(chatId, 'Nothing to stop');
			}
			break;
		default:
			console.log({ msg: 'Unknown arguments', args });
			await respond(chatId, 'Unknown VPN action.');
	}
}

async function setupHandler(chatId: number, origin: string, args: string[]): Promise<Response> {
	const chat = chatId.toString();
	var token = await env.NOTIFY_TOKENS.get(chat);
	if (!token || args[0] === 'renew') {
		if (token) env.NOTIFY_TOKENS.delete(token);
		token = randomBytes(5).toString('hex');
		env.NOTIFY_TOKENS.put(token, chat);
		env.NOTIFY_TOKENS.put(chat, token);
	}

	return respond(chatId, `${origin}/notify/${token}`);
}

async function notify(s: string, request: Request): Promise<Response> {
	const chatId = await env.NOTIFY_TOKENS.get(s);
	if (!chatId) return notFound(`${s} is not a valid token`);
	const contentType = request.headers.get('Content-Type');
	const msg = contentType === 'application/json' ? ((await request.json()) as object) : await request.text();

	return respond(parseInt(chatId), msg).then(accepted);
}

async function hook(request: Request): Promise<Response> {
	const url = new URL(request.url);

	const payload = (await request.json()) as Payload;
	const msg = payload.message;

	if (!msg || !msg.text) {
		console.log({ msg: 'Unknown command', payload });
		return accepted();
	}

	const chatId = msg.chat.id;
	const args = msg.text.split(' ', 2);
	switch (args.shift()) {
		case '/vpn':
			await vpnHandler(chatId, args);
			break;

		case '/setup':
			await setupHandler(chatId, url.origin, args);
			break;

		default:
			await respond(chatId, 'Try /vpn or /setup');
	}

	return accepted();
}

async function notFound(msg: string) {
	return new Response(msg, { status: 404 });
}

async function accepted() {
	return new Response(null, { status: 201 });
}

type Method = 'POST';
type Handler = (m: string[], r: Request) => Promise<Response>;
class Route {
	r: RegExp;
	m: Method;
	h: Handler;

	constructor(m: Method, r: RegExp, h: Handler) {
		this.m = m;
		this.r = r;
		this.h = h;
	}
}

export default {
	async fetch(request): Promise<Response> {
		const url = new URL(request.url);
		const routes = [
			new Route('POST', /^\/notify\/(\w+)$/, (m, r) => notify(m[1], r)),
			new Route('POST', /^\/hook$/, (_, r) => hook(r)),
		];

		const handler =
			routes
				.filter((route) => route.m === request.method)
				.map((route) => {
					const m = url.pathname.match(route.r);
					return m && route.h.call(route, m, request);
				})
				.find((m) => m) || notFound('no route');

		return await handler;
	},
} satisfies ExportedHandler<Env>;
