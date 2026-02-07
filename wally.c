/* wally - animated gif and video wallpaper for X11 */
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xatom.h>
#include <X11/extensions/XShm.h>
#include <sys/ipc.h>
#include <sys/shm.h>
#include <sys/time.h>
#include <gif_lib.h>
#include <libavcodec/avcodec.h>
#include <libavformat/avformat.h>
#include <libswscale/swscale.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <signal.h>

#define MIN(a, b) ((a) < (b) ? (a) : (b))

static Display *dpy;
static Window root;
static GC gc;
static int running = 1;
static int sw, sh;
static int use_shm = 0;
static XShmSegmentInfo shminfo;

enum { ORIGINAL, CENTER, FILL, SCALE };

long long
gettime_us(void)
{
	struct timeval tv;
	gettimeofday(&tv, NULL);
	return (long long)tv.tv_sec * 1000000 + tv.tv_usec;
}

void
die(const char *msg)
{
	fprintf(stderr, "wally: %s\n", msg);
	exit(1);
}

void
usage(void)
{
	die("usage: wally [-c|-f|-s] [-o] <file>\n"
	    "  -c  center on screen\n"
	    "  -f  fill screen (may crop)\n"
	    "  -s  scale to fit screen\n"
	    "  -o  set once and exit (for startup scripts)\n"
	    "  supports: gif, mp4, webm, mkv");
}

void
cleanup(int sig)
{
	running = 0;
}

void
setroot(Pixmap pix)
{
	Atom prop_root, prop_esetroot;

	prop_root = XInternAtom(dpy, "_XROOTPMAP_ID", False);
	prop_esetroot = XInternAtom(dpy, "ESETROOT_PMAP_ID", False);

	XChangeProperty(dpy, root, prop_root, XA_PIXMAP, 32, PropModeReplace, (unsigned char *)&pix, 1);
	XChangeProperty(dpy, root, prop_esetroot, XA_PIXMAP, 32, PropModeReplace, (unsigned char *)&pix, 1);
	XSetWindowBackgroundPixmap(dpy, root, pix);
	XClearWindow(dpy, root);
	XSync(dpy, False);
}

int
isgif(char *path)
{
	char *ext = strrchr(path, '.');
	return ext && strcmp(ext, ".gif") == 0;
}

void
putpixel(XImage *img, int x, int y, unsigned char r, unsigned char g, unsigned char b)
{
	unsigned long pixel;
	
	if (img->bits_per_pixel == 32) {
		pixel = (r << 16) | (g << 8) | b;
		((uint32_t *)img->data)[y * (img->bytes_per_line / 4) + x] = pixel;
	} else if (img->bits_per_pixel == 24) {
		int offset = y * img->bytes_per_line + x * 3;
		img->data[offset + 2] = r;
		img->data[offset + 1] = g;
		img->data[offset + 0] = b;
	} else {
		XPutPixel(img, x, y, (r << 16) | (g << 8) | b);
	}
}

void
scaleframe(XImage *img, unsigned char *rgb, int fw, int fh, int mode)
{
	int x, y, sx, sy, dw, dh;
	double scale;

	memset(img->data, 0, img->bytes_per_line * img->height);

	switch (mode) {
	case CENTER:
		sx = (sw - fw) / 2;
		sy = (sh - fh) / 2;
		for (y = 0; y < fh; y++) {
			if (sy + y < 0 || sy + y >= sh)
				continue;
			for (x = 0; x < fw; x++) {
				if (sx + x >= 0 && sx + x < sw) {
					int idx = (y * fw + x) * 3;
					putpixel(img, sx + x, sy + y, rgb[idx], rgb[idx+1], rgb[idx+2]);
				}
			}
		}
		break;
	case FILL:
		scale = (double)sw / fw > (double)sh / fh ? (double)sw / fw : (double)sh / fh;
		dw = (int)(fw * scale);
		dh = (int)(fh * scale);
		sx = (sw - dw) / 2;
		sy = (sh - dh) / 2;
		for (y = 0; y < sh; y++) {
			int fy = (int)((y - sy) * fh / (double)dh);
			if (fy < 0 || fy >= fh)
				continue;
			for (x = 0; x < sw; x++) {
				int fx = (int)((x - sx) * fw / (double)dw);
				if (fx >= 0 && fx < fw) {
					int idx = (fy * fw + fx) * 3;
					putpixel(img, x, y, rgb[idx], rgb[idx+1], rgb[idx+2]);
				}
			}
		}
		break;
	case SCALE:
		scale = MIN((double)sw / fw, (double)sh / fh);
		dw = (int)(fw * scale);
		dh = (int)(fh * scale);
		sx = (sw - dw) / 2;
		sy = (sh - dh) / 2;
		for (y = 0; y < dh; y++) {
			int fy = (int)(y * fh / (double)dh);
			if (sy + y < 0 || sy + y >= sh || fy >= fh)
				continue;
			for (x = 0; x < dw; x++) {
				int fx = (int)(x * fw / (double)dw);
				if (sx + x >= 0 && sx + x < sw && fx < fw) {
					int idx = (fy * fw + fx) * 3;
					putpixel(img, sx + x, sy + y, rgb[idx], rgb[idx+1], rgb[idx+2]);
				}
			}
		}
		break;
	default: /* ORIGINAL */
		for (y = 0; y < fh && y < sh; y++) {
			for (x = 0; x < fw && x < sw; x++) {
				int idx = (y * fw + x) * 3;
				putpixel(img, x, y, rgb[idx], rgb[idx+1], rgb[idx+2]);
			}
		}
	}
}

int
playvideo(char *path, int mode, int once, XImage *img, Pixmap pix)
{
	AVFormatContext *fmt_ctx = NULL;
	AVCodecContext *codec_ctx = NULL;
	const AVCodec *codec = NULL;
	AVFrame *frame = NULL, *scaled_frame = NULL;
	AVPacket packet;
	struct SwsContext *sws_ctx = NULL;
	int video_stream = -1, ret, i;
	int target_w, target_h;
	double fps, frame_delay, scale;
	long long frame_start, render_time, sleep_time;

	if (avformat_open_input(&fmt_ctx, path, NULL, NULL) < 0)
		return -1;
	if (avformat_find_stream_info(fmt_ctx, NULL) < 0)
		goto cleanup;

	for (i = 0; i < fmt_ctx->nb_streams; i++) {
		if (fmt_ctx->streams[i]->codecpar->codec_type == AVMEDIA_TYPE_VIDEO) {
			video_stream = i;
			break;
		}
	}
	if (video_stream == -1)
		goto cleanup;

	codec = avcodec_find_decoder(fmt_ctx->streams[video_stream]->codecpar->codec_id);
	if (!codec)
		goto cleanup;

	codec_ctx = avcodec_alloc_context3(codec);
	avcodec_parameters_to_context(codec_ctx, fmt_ctx->streams[video_stream]->codecpar);
	if (avcodec_open2(codec_ctx, codec, NULL) < 0)
		goto cleanup;

	switch (mode) {
	case FILL:
		scale = (double)sw / codec_ctx->width > (double)sh / codec_ctx->height 
		        ? (double)sw / codec_ctx->width : (double)sh / codec_ctx->height;
		target_w = (int)(codec_ctx->width * scale);
		target_h = (int)(codec_ctx->height * scale);
		break;
	case SCALE:
		scale = MIN((double)sw / codec_ctx->width, (double)sh / codec_ctx->height);
		target_w = (int)(codec_ctx->width * scale);
		target_h = (int)(codec_ctx->height * scale);
		break;
	case CENTER:
	case ORIGINAL:
	default:
		target_w = codec_ctx->width;
		target_h = codec_ctx->height;
		break;
	}

	frame = av_frame_alloc();
	scaled_frame = av_frame_alloc();
	scaled_frame->format = AV_PIX_FMT_RGB24;
	scaled_frame->width = target_w;
	scaled_frame->height = target_h;
	av_frame_get_buffer(scaled_frame, 0);

	sws_ctx = sws_getContext(codec_ctx->width, codec_ctx->height, codec_ctx->pix_fmt,
	                         target_w, target_h, AV_PIX_FMT_RGB24,
	                         SWS_LANCZOS, NULL, NULL, NULL);

	fps = av_q2d(fmt_ctx->streams[video_stream]->r_frame_rate);
	frame_delay = 1000000.0 / fps;

	while (running) {
		frame_start = gettime_us();
		
		ret = av_read_frame(fmt_ctx, &packet);
		if (ret < 0) {
			if (once)
				break;
			av_seek_frame(fmt_ctx, video_stream, 0, AVSEEK_FLAG_BACKWARD);
			continue;
		}

		if (packet.stream_index == video_stream) {
			avcodec_send_packet(codec_ctx, &packet);
			if (avcodec_receive_frame(codec_ctx, frame) == 0) {
				sws_scale(sws_ctx, (const uint8_t *const *)frame->data, frame->linesize,
				         0, codec_ctx->height, scaled_frame->data, scaled_frame->linesize);

				scaleframe(img, scaled_frame->data[0], target_w, target_h, mode);
				
				if (use_shm)
					XShmPutImage(dpy, pix, gc, img, 0, 0, 0, 0, sw, sh, False);
				else
					XPutImage(dpy, pix, gc, img, 0, 0, 0, 0, sw, sh);
				
				setroot(pix);
				
				/* account for render time */
				render_time = gettime_us() - frame_start;
				sleep_time = frame_delay - render_time;
				if (sleep_time > 0)
					usleep(sleep_time);
			}
		}
		av_packet_unref(&packet);
	}

cleanup:
	if (scaled_frame) av_frame_free(&scaled_frame);
	if (frame) av_frame_free(&frame);
	if (sws_ctx) sws_freeContext(sws_ctx);
	if (codec_ctx) avcodec_free_context(&codec_ctx);
	if (fmt_ctx) avformat_close_input(&fmt_ctx);
	return 0;
}

unsigned long
getpixel(ColorMapObject *cmap, GifByteType idx, int transparent)
{
	if (idx == transparent)
		return 0;
	GifColorType *c = &cmap->Colors[idx];
	return (c->Red << 16) | (c->Green << 8) | c->Blue;
}

void
renderframe(unsigned int *canvas, GifFileType *gif, int frame_idx)
{
	SavedImage *frame = &gif->SavedImages[frame_idx];
	ColorMapObject *cmap = frame->ImageDesc.ColorMap ? frame->ImageDesc.ColorMap : gif->SColorMap;
	GifByteType *raster = frame->RasterBits;
	GraphicsControlBlock gcb;
	int transparent = -1;
	int x, y, i;

	if (DGifSavedExtensionToGCB(gif, frame_idx, &gcb) == GIF_OK && gcb.TransparentColor != -1)
		transparent = gcb.TransparentColor;

	if (frame_idx > 0) {
		GraphicsControlBlock prev_gcb;
		if (DGifSavedExtensionToGCB(gif, frame_idx - 1, &prev_gcb) == GIF_OK) {
			if (prev_gcb.DisposalMode == DISPOSE_BACKGROUND) {
				SavedImage *prev = &gif->SavedImages[frame_idx - 1];
				for (y = prev->ImageDesc.Top; y < prev->ImageDesc.Top + prev->ImageDesc.Height; y++)
					for (x = prev->ImageDesc.Left; x < prev->ImageDesc.Left + prev->ImageDesc.Width; x++)
						if (x < gif->SWidth && y < gif->SHeight)
							canvas[y * gif->SWidth + x] = 0;
			}
		}
	} else {
		memset(canvas, 0, gif->SWidth * gif->SHeight * sizeof(unsigned int));
	}

	i = 0;
	for (y = 0; y < frame->ImageDesc.Height; y++) {
		for (x = 0; x < frame->ImageDesc.Width; x++) {
			GifByteType idx = raster[i++];
			if (idx != transparent) {
				int px = frame->ImageDesc.Left + x;
				int py = frame->ImageDesc.Top + y;
				if (px < gif->SWidth && py < gif->SHeight)
					canvas[py * gif->SWidth + px] = getpixel(cmap, idx, transparent);
			}
		}
	}
}

void
scalegif(XImage *img, unsigned int *canvas, int gw, int gh, int mode)
{
	int x, y, sx, sy, dw, dh;
	double scale;
	unsigned long pixel;

	memset(img->data, 0, img->bytes_per_line * img->height);

	switch (mode) {
	case CENTER:
		sx = (sw - gw) / 2;
		sy = (sh - gh) / 2;
		for (y = 0; y < gh; y++) {
			if (sy + y < 0 || sy + y >= sh)
				continue;
			for (x = 0; x < gw; x++) {
				if (sx + x >= 0 && sx + x < sw) {
					pixel = canvas[y * gw + x];
					putpixel(img, sx + x, sy + y, (pixel >> 16) & 0xFF, 
					        (pixel >> 8) & 0xFF, pixel & 0xFF);
				}
			}
		}
		break;
	case FILL:
		scale = (double)sw / gw > (double)sh / gh ? (double)sw / gw : (double)sh / gh;
		dw = (int)(gw * scale);
		dh = (int)(gh * scale);
		sx = (sw - dw) / 2;
		sy = (sh - dh) / 2;
		for (y = 0; y < sh; y++) {
			int gy = (int)((y - sy) * gw / (double)dw);
			if (gy < 0 || gy >= gh)
				continue;
			for (x = 0; x < sw; x++) {
				int gx = (int)((x - sx) * gw / (double)dw);
				if (gx >= 0 && gx < gw) {
					pixel = canvas[gy * gw + gx];
					putpixel(img, x, y, (pixel >> 16) & 0xFF, 
					        (pixel >> 8) & 0xFF, pixel & 0xFF);
				}
			}
		}
		break;
	case SCALE:
		scale = MIN((double)sw / gw, (double)sh / gh);
		dw = (int)(gw * scale);
		dh = (int)(gh * scale);
		sx = (sw - dw) / 2;
		sy = (sh - dh) / 2;
		for (y = 0; y < dh; y++) {
			int gy = (int)(y * gw / (double)dw);
			if (sy + y < 0 || sy + y >= sh || gy >= gh)
				continue;
			for (x = 0; x < dw; x++) {
				int gx = (int)(x * gw / (double)dw);
				if (sx + x >= 0 && sx + x < sw && gx < gw) {
					pixel = canvas[gy * gw + gx];
					putpixel(img, sx + x, sy + y, (pixel >> 16) & 0xFF, 
					        (pixel >> 8) & 0xFF, pixel & 0xFF);
				}
			}
		}
		break;
	default: /* ORIGINAL */
		for (y = 0; y < gh && y < sh; y++) {
			for (x = 0; x < gw && x < sw; x++) {
				pixel = canvas[y * gw + x];
				putpixel(img, x, y, (pixel >> 16) & 0xFF, 
				        (pixel >> 8) & 0xFF, pixel & 0xFF);
			}
		}
	}
}

int
playgif(char *path, int mode, int once, XImage *img, Pixmap pix)
{
	GifFileType *gif;
	GraphicsControlBlock gcb;
	unsigned int *canvas;
	int err, frame = 0, delay;
	long long frame_start, render_time, sleep_time;

	if (!(gif = DGifOpenFileName(path, &err)))
		return -1;
	if (DGifSlurp(gif) == GIF_ERROR) {
		DGifCloseFile(gif, NULL);
		return -1;
	}

	canvas = calloc(gif->SWidth * gif->SHeight, sizeof(unsigned int));

	while (running) {
		frame_start = gettime_us();
		
		if (DGifSavedExtensionToGCB(gif, frame, &gcb) == GIF_OK)
			delay = gcb.DelayTime <= 0 ? 20000 : gcb.DelayTime * 10000;
		else
			delay = 20000;

		renderframe(canvas, gif, frame);
		scalegif(img, canvas, gif->SWidth, gif->SHeight, mode);
		
		if (use_shm)
			XShmPutImage(dpy, pix, gc, img, 0, 0, 0, 0, sw, sh, False);
		else
			XPutImage(dpy, pix, gc, img, 0, 0, 0, 0, sw, sh);
		
		setroot(pix);

		if (once)
			break;

		/* account for render time */
		render_time = gettime_us() - frame_start;
		sleep_time = delay - render_time;
		if (sleep_time > 0)
			usleep(sleep_time);

		frame = (frame + 1) % gif->ImageCount;
	}

	free(canvas);
	DGifCloseFile(gif, NULL);
	return 0;
}

int
main(int argc, char *argv[])
{
	XImage *img;
	Pixmap pix;
	Visual *vis;
	int screen, depth, mode = ORIGINAL, once = 0, i;
	char *filepath = NULL;

	for (i = 1; i < argc; i++) {
		if (argv[i][0] == '-') {
			switch (argv[i][1]) {
			case 'c': mode = CENTER; break;
			case 'f': mode = FILL; break;
			case 's': mode = SCALE; break;
			case 'o': once = 1; break;
			default: usage();
			}
		} else {
			filepath = argv[i];
		}
	}

	if (!filepath)
		usage();

	signal(SIGINT, cleanup);
	signal(SIGTERM, cleanup);

	if (!(dpy = XOpenDisplay(NULL)))
		die("cannot open display");

	screen = DefaultScreen(dpy);
	root = RootWindow(dpy, screen);
	vis = DefaultVisual(dpy, screen);
	depth = DefaultDepth(dpy, screen);
	gc = DefaultGC(dpy, screen);
	sw = DisplayWidth(dpy, screen);
	sh = DisplayHeight(dpy, screen);

	use_shm = XShmQueryExtension(dpy);
	
	if (use_shm) {
		img = XShmCreateImage(dpy, vis, depth, ZPixmap, NULL, &shminfo, sw, sh);
		shminfo.shmid = shmget(IPC_PRIVATE, img->bytes_per_line * img->height, IPC_CREAT | 0777);
		shminfo.shmaddr = img->data = shmat(shminfo.shmid, 0, 0);
		shminfo.readOnly = False;
		XShmAttach(dpy, &shminfo);
		shmctl(shminfo.shmid, IPC_RMID, 0);
	} else {
		img = XCreateImage(dpy, vis, depth, ZPixmap, 0, NULL, sw, sh, 32, 0);
		img->data = malloc(img->bytes_per_line * img->height);
	}

	pix = XCreatePixmap(dpy, root, sw, sh, depth);

	if (isgif(filepath)) {
		if (playgif(filepath, mode, once, img, pix) < 0)
			playvideo(filepath, mode, once, img, pix);
	} else {
		playvideo(filepath, mode, once, img, pix);
	}

	if (use_shm) {
		XShmDetach(dpy, &shminfo);
		shmdt(shminfo.shmaddr);
	} else {
		XDestroyImage(img);
	}
	
	XFreePixmap(dpy, pix);
	XCloseDisplay(dpy);

	return 0;
}