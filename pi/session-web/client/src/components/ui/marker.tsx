import type { HTMLAttributes } from "react";
import { cn } from "../../lib/utils";

export function Marker({ className, variant = "default", ...props }: HTMLAttributes<HTMLDivElement> & { variant?: "default" | "border" | "separator" }) {
  return <div data-variant={variant} className={cn("marker", className)} {...props} />;
}

export function MarkerIcon({ className, ...props }: HTMLAttributes<HTMLSpanElement>) {
  return <span className={cn("marker-icon", className)} {...props} />;
}

export function MarkerContent({ className, ...props }: HTMLAttributes<HTMLSpanElement>) {
  return <span className={cn("marker-content", className)} {...props} />;
}
