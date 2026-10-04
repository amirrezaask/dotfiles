import type { HTMLAttributes, ImgHTMLAttributes } from "react";
import { cn } from "../../lib/utils";

export function Attachment({ className, state = "done", orientation = "horizontal", ...props }: HTMLAttributes<HTMLDivElement> & { state?: "idle" | "uploading" | "processing" | "error" | "done"; orientation?: "horizontal" | "vertical" }) {
  return <div data-state={state} data-orientation={orientation} className={cn("attachment", className)} {...props} />;
}

export function AttachmentMedia({ className, variant = "icon", ...props }: HTMLAttributes<HTMLDivElement> & { variant?: "icon" | "image" }) {
  return <div data-variant={variant} className={cn("attachment-media", className)} {...props} />;
}

export function AttachmentImage({ className, alt = "", src, ...props }: ImgHTMLAttributes<HTMLImageElement>) {
  if (!src) return null;
  return <img className={cn("attachment-image", className)} alt={alt} src={src} {...props} />;
}

export function AttachmentContent({ className, ...props }: HTMLAttributes<HTMLDivElement>) {
  return <div className={cn("attachment-content", className)} {...props} />;
}

export function AttachmentTitle({ className, ...props }: HTMLAttributes<HTMLDivElement>) {
  return <div className={cn("attachment-title", className)} {...props} />;
}

export function AttachmentDescription({ className, ...props }: HTMLAttributes<HTMLDivElement>) {
  return <div className={cn("attachment-description", className)} {...props} />;
}

export function AttachmentGroup({ className, ...props }: HTMLAttributes<HTMLDivElement>) {
  return <div className={cn("attachment-group", className)} {...props} />;
}

export function AttachmentActions({ className, ...props }: HTMLAttributes<HTMLDivElement>) {
  return <div className={cn("attachment-actions", className)} {...props} />;
}

export function AttachmentAction({ className, ...props }: HTMLAttributes<HTMLButtonElement>) {
  return <button type="button" className={cn("attachment-action", className)} {...props} />;
}
