import { run, annotate } from 'pierre';
import * as Minio from 'minio';

async function build() {
	await run('cargo build --release', { label: 'Build binaries' });
}

async function upload() {
	const timestamp = Date.now();
	const bucket = 'artifacts';

	const sourceFile = 'target/release/loft';
	const destinationObject = `pierre/loft-${timestamp}`;

	const minioClient = new Minio.Client({
		useSSL: true,
		endPoint: 's3.themackabu.dev',
		accessKey: process.env.S3_KEY,
		secretKey: process.env.S3_SECRET
	});

	await minioClient.fPutObject(bucket, destinationObject, sourceFile, {
		'Content-Type': 'application/octet-stream'
	});

	annotate({
		icon: Icons.File,
		color: 'fg',
		label: 'Binary',
		href: `https://artifacts.s3.themackabu.dev/${destinationObject}`
	});
}

export default [build, upload];
