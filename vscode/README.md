# Vscode config

# Installation

```bash
cp settings.json ~/.config/Code/User/settings.json
cat extensions.txt | while read extension;do 
	code --install-extension $extension 
done
```
