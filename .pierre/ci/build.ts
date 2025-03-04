import { run } from 'pierre';
import { getVersion } from '../version';
import { upload_file } from '../upload';

async function add_toolchains() {
	await run('rustup target add x86_64-pc-windows-gnu', { label: 'Add target windows (x86_64)' });
	await run('rustup target add x86_64-apple-darwin', { label: 'Add target darwin (x86_64)' });
	await run('rustup target add aarch64-apple-darwin', { label: 'Add target darwin (aarch64)' });
}

async function build() {
	await run('cargo build -r', { label: 'Build linux (x86_64)' });
	await run('cargo build -r --target x86_64-pc-windows-gnu', { label: 'Build windows (x86_64)' });
	await run('cargo build -r --target x86_64-apple-darwin', { label: 'Build darwin (x86_64)' });
	await run('cargo build -r --target aarch64-apple-darwin', { label: 'Build darwin (aarch64)' });
}

async function upload() {
	const time = Date.now();
	const version = await getVersion();

	await upload_file({ time, version });
	await upload_file({ time, version, path: 'x86_64-pc-windows-gnu' });
	await upload_file({ time, version, path: 'x86_64-apple-darwin' });
	await upload_file({ time, version, path: 'aarch64-apple-darwin' });
}

export default [add_toolchains, build, upload];
