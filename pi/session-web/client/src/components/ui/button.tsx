import type { ButtonHTMLAttributes, Ref } from "react";
import { cn } from "../../lib/utils";

type ButtonProps = ButtonHTMLAttributes<HTMLButtonElement> & {
  ref?: Ref<HTMLButtonElement>;
  variant?: "default" | "ghost" | "outline";
  size?: "default" | "sm" | "icon";
};

export function Button({ className, variant = "default", size = "default", ref, ...props }: ButtonProps) {
  return (
    <button
      ref={ref}
      data-variant={variant}
      data-size={size}
      className={cn("button", className)}
      {...props}
    />
  );
}
