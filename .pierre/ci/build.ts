import { run } from 'pierre';
import { getVersion } from '../version';
import { upload_file } from '../upload';

const LD_LIBRARY_PATH = 'LD_LIBRARY_PATH=/usr/lib/x86_64-linux-gnu:$LD_LIBRARY_PATH';
const APPLE_SDK = 'github.com/roblabla/MacOSX-SDKs/releases/download/13.3/MacOSX13.3.sdk.tar.xz';
const ZIG_BUILD_BINARY = 'github.com/rust-cross/cargo-zigbuild/releases/download/v0.19.8/cargo-zigbuild-v0.19.8.x86_64-unknown-linux-musl.tar.gz';

async function add_linkers() {
	await run('apt-get update && apt-get install mingw-w64 -y', { label: 'Add mingw-w64' });
	await run(`${LD_LIBRARY_PATH} curl -L https://${APPLE_SDK} | tar xJ -C /opt`, { label: 'Add darwin sdk' });
	await run(`${LD_LIBRARY_PATH} curl -L https://${ZIG_BUILD_BINARY} | tar xz -C opt`, { label: 'Add cargo-zigbuild' });
}

async function add_toolchains() {
	await run('rustup target add x86_64-pc-windows-gnu', { label: 'Add target windows (x86_64)' });
	await run('rustup target add x86_64-apple-darwin', { label: 'Add target darwin (x86_64)' });
	await run('rustup target add aarch64-apple-darwin', { label: 'Add target darwin (aarch64)' });
}

async function build() {
	const time = Date.now();
	const version = await getVersion();
	const buildCommand = '/opt/cargo-zigbuild zigbuild -r';

	await run(buildCommand, { label: 'Build linux (x86_64)' });
	await upload_file({ time, version });

	await run(`${buildCommand} --target x86_64-pc-windows-gnu`, { label: 'Build windows (x86_64)' });
	await upload_file({ time, version, path: 'x86_64-pc-windows-gnu' });

	await run(`${buildCommand} --target x86_64-apple-darwin`, { label: 'Build darwin (x86_64)' });
	await upload_file({ time, version, path: 'x86_64-apple-darwin' });

	await run(`${buildCommand} --target aarch64-apple-darwin`, { label: 'Build darwin (aarch64)' });
	await upload_file({ time, version, path: 'aarch64-apple-darwin' });
}

export default [add_linkers];
