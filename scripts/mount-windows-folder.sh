# Set our local user credentials
#WIN_HOST_USER="..." # Username of local user, or the user portion of a Microsoft Account email address
#WIN_HOST_PASS="..."

# If you are using a Microsoft Account, uncomment and assign the domain portion of your email address
#WIN_HOST_DOMAIN="live.com"

# Fetch the current user's uid and gid.
WSL_UID=`id -u`
WSL_GID=`id -g` # You can use `id -g my-group` if you want the id of a specific group

# Create a function we can call to make it easier to mount multiple folders
mount_cifs() {
	# If the mount point is already in use, echo a notice and exit. You can remove the echo portion if it annoys you
	mount | grep "$2" &>/dev/null && echo "$2: Already mounted" && return 1
	# If the mount point folder does not exist, create it
	if [ ! -f "$2" ]; then
		sudo mkdir "$2"
	fi
	# (Local User) Comment out if using Microsoft Account
	#sudo mount -t cifs "//127.0.0.1/$1" "$2" -o "username=$WIN_HOST_USER,password=$WIN_HOST_PASS,uid=$WSL_UID,gid=$WSL_GID"
	#sudo mount -t cifs "//127.0.0.1/$1" "$2" -o "username=$WIN_HOST_USER,password=$WIN_HOST_PASS,uid=$WSL_UID,gid=$WSL_GID"
	# (Microsoft User) Uncomment if using Microsoft Account
	#sudo mount -t cifs "//127.0.0.1/$1" "$2" -o "username=$WIN_HOST_USER,password=$WIN_HOST_PASS,domain=$WIN_HOST_DOMAIN,uid=$WSL_UID,gid=$WSL_GID"
	sudo mount -t cifs "//127.0.0.1/$1" "$2" -o "credentials=/home/sdienst/cifscredentials,uid=$WSL_UID,gid=$WSL_GID"
}
mount_cifs daten /data
mount_cifs import /import
#mount_cifs current-workspace /home/sdienst/workspaces
