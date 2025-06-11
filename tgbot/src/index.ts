import { env } from 'cloudflare:workers';
import userDataTmpl from './hetzner.yaml';
import * as openstack from './openstack';
import { randomBytes } from 'node:crypto';

interface Payload {
	message?: { chat: { id: number }; text?: string };
}
const selectelUser = {
	name: 'openstack',
	domain: { name: env.SELECTEL_ORG_ID },
	password: env.SELECTEL_PASS,
};
var userData!: string;

async function respond(chatId: number, text: string): Promise<void> {
	await fetch(
		`https://api.telegram.org/bot${env.BOT_TOKEN}/sendMessage?chat_id=${chatId}`,
		{
			method: 'POST',
			headers: { 'Content-Type': 'application/json' },
			body: JSON.stringify({ text }),
		},
	);
}

async function deployHetzner(): Promise<void> {
	await fetch('https://api.hetzner.cloud/v1/servers', {
		method: 'POST',
		headers: {
			Authorization: `Bearer ${env.HCLOUD_TOKEN}`,
			'Content-Type': 'application/json',
		},
		body: JSON.stringify({
			name: 'vpn',
			location: 'hel1',
			server_type: 'cax11',
			start_after_create: true,
			image: 'ubuntu-24.04',
			ssh_keys: ['thinkpad'],
			public_net: {
				enable_ipv4: true,
				enable_ipv6: true,
				ipv6: env.HETZNER_IPV6_ID, // vpn_ipv6
			},
			user_data: userData,
		}),
	});
}

async function stopHetzner(): Promise<boolean> {
	const response = await fetch(`https://api.hetzner.cloud/v1/servers?name=vpn`, {
		headers: {
			Authorization: `Bearer ${env.HCLOUD_TOKEN}`,
		},
	});

	if (!response.ok) return false;

	const servers = ((await response.json()) as { servers: { id: string }[] }).servers;
	if (servers.length == 0) return false;

	const deleteResponse = await fetch(
		`https://api.hetzner.cloud/v1/servers/${servers[0].id}`,
		{
			method: 'DELETE',
			headers: {
				Authorization: `Bearer ${env.HCLOUD_TOKEN}`,
				'Content-Type': 'application/json',
			},
		},
	);

	return deleteResponse.ok;
}

async function startSelectel(): Promise<void> {
	await openstack.auth(selectelUser);
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
	await openstack.auth(selectelUser);
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
		.replaceAll(/\[secret:([^\]]+)\]/g, (_, s) => env[s as keyof Env]);
}

async function vpnHandler(chatId: number, args: string[]) {
	switch (args[0] ?? 'ru') {
		case 'de':
			userData = prepareUserData(chatId);
			await deployHetzner();
			await respond(chatId, 'Creating hetzner VPN');
			break;
		case 'ru':
			userData = prepareUserData(chatId);
			await startSelectel();
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
	}
}

async function setupHandler(chatId: number, origin: string): Promise<void> {
	const chat = chatId.toString();
	const existing = await env.NOTIFY_TOKENS.get(chat);
	if (existing) env.NOTIFY_TOKENS.delete(existing);

	const token = randomBytes(5).toString('hex');
	env.NOTIFY_TOKENS.put(token, chat);
	env.NOTIFY_TOKENS.put(chat, token);
	return respond(chatId, `${origin}/notify/${token}`);
}

export default {
	async fetch(request): Promise<Response> {
		if (request.method != 'POST') return new Response(null, { status: 404 });
		const url = new URL(request.url);
		{
			const m = url.pathname.match(/^\/notify\/(\w+)$/);
			if (m) {
				const chatId = await env.NOTIFY_TOKENS.get(m[1]);
				if (chatId) {
					await respond(parseInt(chatId), await request.text());
				}
				return new Response(null, { status: 201 });
			}
		}

		const payload = (await request.json()) as Payload;
		const cmd = payload.message?.text;

		if (!cmd) {
			console.log({ msg: 'Unknown command', payload });
			return new Response(null, { status: 201 });
		}

		const chatId = payload.message?.chat.id!;
		const args = cmd.split(' ', 2);
		switch (args[0]) {
			case '/vpn':
				await vpnHandler(chatId, args.slice(1));
				break;

			case '/setup':
				await setupHandler(chatId, url.origin);
				break;

			default:
				await respond(chatId, 'Try /vpn or /setup');
		}

		return new Response(null, { status: 201 });
	},
} satisfies ExportedHandler<Env>;
