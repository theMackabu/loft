import * as Minio from 'minio';
import { annotate, Icons } from 'pierre';
import { extract_arch } from './version';

interface UploadSettings {
	time: number;
	path: string?;
	version: string;
}

export async function upload_file(upl: UploadSettings) {
	const path = upl.path ?? 'release';
	const [arch, platform] = extract_arch(path);

	const bucket = 'artifacts';
	const sourceFile = `target/${path}/loft`;

	const destFile = `loft-${upl.version}-${arch}-${platform}`;
	const destObject = `pierre/${upl.time}/${upl.version}/${destFile}`;

	const minioClient = new Minio.Client({
		useSSL: true,
		endPoint: 's3.themackabu.dev',
		accessKey: process.env.S3_KEY,
		secretKey: process.env.S3_SECRET
	});

	await minioClient.fPutObject(bucket, destObject, sourceFile, {
		'Content-Type': 'application/octet-stream'
	});

	annotate({
		icon: Icons.File,
		color: 'fg',
		label: 'Binary',
		href: `https://artifacts.s3.themackabu.dev/${destinationObject}`
	});
}
