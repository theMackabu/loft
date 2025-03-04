import fs from 'fs/promises';

export async function getVersion() {
	const cargoTomlContent = await fs.readFile('Cargo.toml', 'utf-8');
	const versionMatch = cargoTomlContent.match(/version\s*=\s*"([^"]+)"/);

	if (!versionMatch || !versionMatch[1]) {
		throw new Error('Version not found in Cargo.toml');
	}

	return versionMatch[1];
}
