import * as Minio from 'minio';
import { run, annotate, Icons } from 'pierre';
import { extract_arch } from './version';

interface UploadSettings {
	time: number;
	path: string?;
	version: string;
}

export async function upload_file(upl: UploadSettings) {
	const [arch, platform] = extract_arch(upl.path);
	const path = upl.path ? `${upl.path}/release` : 'release';

	const isWindows = path.includes('windows');
	const sourceFile = `target/${path}/loft${isWindows ? '.exe' : ''}`;

	const destFile = `loft-${upl.version}-${arch}-${platform}`;
	const destObject = `pierre/${upl.time}/${upl.version}/${destFile}`;

	const minioClient = new Minio.Client({
		useSSL: true,
		endPoint: 's3.themackabu.dev',
		accessKey: process.env.S3_KEY,
		secretKey: process.env.S3_SECRET
	});

	await run(`chmod +x ${sourceFile}`, { label: `make executable for ${platform}` });
	await run(`echo ${destObject}`, { label: `uploading ${destFile}` });

	await minioClient.fPutObject('artifacts', destObject, sourceFile, {
		'Content-Type': 'application/octet-stream'
	});

	annotate({
		icon: Icons.File,
		color: 'fg',
		label: 'Binary',
		href: `https://artifacts.s3.themackabu.dev/${destObject}`
	});
}
