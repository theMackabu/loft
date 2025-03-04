import fs from 'fs/promises';

export async function getVersion() {
	const cargoTomlContent = await fs.readFile('Cargo.toml', 'utf-8');
	const versionMatch = cargoTomlContent.match(/version\s*=\s*"([^"]+)"/);

	if (!versionMatch || !versionMatch[1]) {
		throw new Error('Version not found in Cargo.toml');
	}

	return versionMatch[1];
}

export function extract_arch(path?: string): [string, string] {
	if (!path || path === 'release') {
		return ['x86_64', 'linux'];
	} else if (path.includes('windows')) {
		const parts = path.split('-');
		return [parts[0], 'windows'];
	} else {
		const parts = path.split('-');
		if (parts.length >= 2) {
			return [parts[0], parts[parts.length - 1]];
		}
		return ['unknown', 'unknown'];
	}
}
