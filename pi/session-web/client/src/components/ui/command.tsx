import * as CommandPrimitive from "cmdk";
import type { ComponentProps } from "react";
import { cn } from "../../lib/utils";

export function Command({ className, ...props }: ComponentProps<typeof CommandPrimitive.Command>) {
  return <CommandPrimitive.Command className={cn("command", className)} {...props} />;
}

export function CommandList({ className, ...props }: ComponentProps<typeof CommandPrimitive.CommandList>) {
  return <CommandPrimitive.CommandList className={cn("command-list", className)} {...props} />;
}

export function CommandEmpty({ className, ...props }: ComponentProps<typeof CommandPrimitive.CommandEmpty>) {
  return <CommandPrimitive.CommandEmpty className={cn("command-empty", className)} {...props} />;
}

export function CommandGroup({ className, ...props }: ComponentProps<typeof CommandPrimitive.CommandGroup>) {
  return <CommandPrimitive.CommandGroup className={cn("command-group", className)} {...props} />;
}

export function CommandItem({ className, ...props }: ComponentProps<typeof CommandPrimitive.CommandItem>) {
  return <CommandPrimitive.CommandItem className={cn("command-item", className)} {...props} />;
}

export function CommandSeparator({ className, ...props }: ComponentProps<typeof CommandPrimitive.CommandSeparator>) {
  return <CommandPrimitive.CommandSeparator className={cn("command-separator", className)} {...props} />;
}
