import { run } from 'pierre';
import { getVersion } from '../version';
import { upload_file } from '../upload';

async function add_linkers() {
	const appleSDK = 'https://github.com/roblabla/MacOSX-SDKs/releases/download/13.3/MacOSX13.3.sdk.tar.xz ';

	await run('apt update && apt install tar mingw-w64 -y', { label: 'Add mingw-w64' });
	await run(`curl -L ${appleSDK} | tar xJ`, { label: 'Add darwin' });
	await run('export SDKROOT=$(pwd)/MacOSX13.3.sdk/ && export CARGO_TARGET_X86_64_APPLE_DARWIN_LINKER=rust-lld');
}

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

export default [add_linkers, add_toolchains, build, upload];
