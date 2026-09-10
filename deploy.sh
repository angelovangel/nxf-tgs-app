#!/usr/bin/env bash
# ==============================================================================
# nxf-tgs-app Deployment Script
# Description: Automated setup script for fresh Ubuntu/Debian server
# Requirements handled:
#   - System packages & build dependencies
#   - Java JRE + Nextflow installation
#   - Miniserve binary installation
#   - R environment & renv package restoration
#   - Directory structure & .Renviron configuration
#   - credentials.rds generation (for shinymanager)
#   - systemd service configuration for Shiny App & Miniserve
# ==============================================================================

set -euo pipefail

# --- Color Formatting ---
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
CYAN='\033[0;36m'
BOLD='\033[1m'
NC='\033[0m' # No Color

log_info()    { echo -e "${BLUE}[INFO]${NC} $*"; }
log_success() { echo -e "${GREEN}[SUCCESS]${NC} $*"; }
log_warn()    { echo -e "${YELLOW}[WARN]${NC} $*"; }
log_error()   { echo -e "${RED}[ERROR]${NC} $*"; }

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
APP_DIR="${SCRIPT_DIR}"
cd "${APP_DIR}"

# --- Determine Service User & Group ---
if [ -n "${SUDO_USER:-}" ] && [ "${SUDO_USER}" != "root" ]; then
    DEFAULT_SERVICE_USER="${SUDO_USER}"
else
    DEFAULT_SERVICE_USER="$(id -un)"
fi
DEFAULT_SERVICE_GROUP="$(id -gn "${DEFAULT_SERVICE_USER}" 2>/dev/null || echo "${DEFAULT_SERVICE_USER}")"
DEFAULT_SERVICE_HOME="$(eval echo "~${DEFAULT_SERVICE_USER}")"

# --- Default Parameters ---
APP_USER_DEFAULT="admin"
APP_USER="${APP_USER:-}"
APP_PASS="${APP_PASS:-}"
DATA_PATH_DEFAULT="${DEFAULT_PATH:-$DEFAULT_SERVICE_HOME}"
MINISERVE_DIR_DEFAULT="${MINISERVE_PATH:-$APP_DIR/miniserve}"
SHINY_PORT=3838
MINISERVE_PORT=8080
NEXTFLOW_VER="25.04.7"
SERVICE_USER="${DEFAULT_SERVICE_USER}"
SERVICE_GROUP="${DEFAULT_SERVICE_GROUP}"
SKIP_SYS_DEPS=false
SKIP_DOCKER=false
SKIP_SYSTEMD=false
NON_INTERACTIVE=false
RESET_CREDENTIALS=false

# --- Argument Parsing ---
print_help() {
    cat <<EOF
Usage: ./deploy.sh [OPTIONS]

Options:
  -u, --user <username>         Admin username for Shiny app login (default: ${APP_USER_DEFAULT})
  -p, --password <password>     Admin password for Shiny app login
  -d, --data-path <path>        Default data path for shinyFiles volume (default: ${DATA_PATH_DEFAULT})
  -m, --miniserve-path <path>   Path where miniserve serves files (default: ${MINISERVE_DIR_DEFAULT})
  --shiny-port <port>           Port for Shiny application (default: ${SHINY_PORT})
  --miniserve-port <port>       Port for miniserve file server (default: ${MINISERVE_PORT})
  --nextflow-ver <ver>          Nextflow version to install (default: ${NEXTFLOW_VER})
  --service-user <user>         System user to run systemd services (default: ${DEFAULT_SERVICE_USER})
  --skip-sys-deps               Skip apt-get system package installations
  --skip-docker                 Skip Docker installation
  --skip-systemd                Skip creating and starting systemd services
  --reset-credentials           Force overwrite of credentials.rds if it already exists
  -y, --non-interactive         Run without interactive prompts
  -h, --help                    Show this help message
EOF
}

while [[ $# -gt 0 ]]; do
    case "$1" in
        -u|--user)
            APP_USER="$2"
            shift 2
            ;;
        -p|--password)
            APP_PASS="$2"
            shift 2
            ;;
        -d|--data-path)
            DATA_PATH_DEFAULT="$2"
            shift 2
            ;;
        -m|--miniserve-path)
            MINISERVE_DIR_DEFAULT="$2"
            shift 2
            ;;
        --shiny-port)
            SHINY_PORT="$2"
            shift 2
            ;;
        --miniserve-port)
            MINISERVE_PORT="$2"
            shift 2
            ;;
        --nextflow-ver)
            NEXTFLOW_VER="$2"
            shift 2
            ;;
        --service-user)
            SERVICE_USER="$2"
            SERVICE_GROUP="$(id -gn "${SERVICE_USER}" 2>/dev/null || echo "${SERVICE_USER}")"
            shift 2
            ;;
        --skip-sys-deps)
            SKIP_SYS_DEPS=true
            shift
            ;;
        --skip-docker)
            SKIP_DOCKER=true
            shift
            ;;
        --skip-systemd)
            SKIP_SYSTEMD=true
            shift
            ;;
        --reset-credentials)
            RESET_CREDENTIALS=true
            shift
            ;;
        -y|--non-interactive)
            NON_INTERACTIVE=true
            shift
            ;;
        -h|--help)
            print_help
            exit 0
            ;;
        *)
            log_error "Unknown option: $1"
            print_help
            exit 1
            ;;
    esac
done

echo -e "${BOLD}${CYAN}===================================================${NC}"
echo -e "${BOLD}${CYAN}       Deploying nxf-tgs-app on Fresh Server        ${NC}"
echo -e "${BOLD}${CYAN}===================================================${NC}"

# --- Helper: Command Detection with Sudo ---
SUDO=""
if [ "$(id -u)" -ne 0 ]; then
    if command -v sudo >/dev/null 2>&1; then
        SUDO="sudo"
    else
        log_warn "Not running as root and 'sudo' is not installed. System package installation might fail."
    fi
fi

# ==============================================================================
# 1. System Packages & Build Dependencies
# ==============================================================================
if [ "$SKIP_SYS_DEPS" = false ]; then
    if command -v apt-get >/dev/null 2>&1; then
        log_info "Detected Debian/Ubuntu system. Installing system packages..."
        export DEBIAN_FRONTEND=noninteractive
        $SUDO apt-get update -y
        $SUDO apt-get install -y \
            curl \
            wget \
            git \
            tmux \
            tar \
            gzip \
            unzip \
            build-essential \
            ca-certificates \
            gnupg \
            dirmngr \
            software-properties-common \
            openjdk-17-jre-headless \
            r-base \
            r-base-dev \
            libcurl4-openssl-dev \
            libssl-dev \
            libxml2-dev \
            libfontconfig1-dev \
            libharfbuzz-dev \
            libfribidi-dev \
            libfreetype6-dev \
            libpng-dev \
            libtiff-dev \
            libjpeg-dev \
            libgit2-dev \
            libuv1-dev
        log_success "System packages installed successfully."
    else
        log_warn "apt-get not detected. Please ensure R, Java 11+, tmux, curl, and build tools are installed."
    fi
else
    log_info "Skipping system dependencies (--skip-sys-deps)."
fi

# ==============================================================================
# 2. Nextflow Installation (v25.04.7)
# ==============================================================================
log_info "Checking Nextflow (target version: ${NEXTFLOW_VER})..."
INSTALL_BIN_DIR="/usr/local/bin"

install_nextflow() {
    log_info "Installing Nextflow v${NEXTFLOW_VER}..."
    local target_dir="/tmp/nxf_install_$$"
    mkdir -p "$target_dir"
    cd "$target_dir"
    
    export NXF_VER="${NEXTFLOW_VER}"
    curl -fsSL https://get.nextflow.io | bash
    chmod +x nextflow

    if [ -w "$INSTALL_BIN_DIR" ]; then
        mv nextflow "$INSTALL_BIN_DIR/nextflow"
    elif [ -n "$SUDO" ]; then
        $SUDO mv nextflow "$INSTALL_BIN_DIR/nextflow"
    else
        mkdir -p "$HOME/.local/bin"
        mv nextflow "$HOME/.local/bin/nextflow"
        export PATH="$HOME/.local/bin:$PATH"
    fi
    cd "${APP_DIR}"
    rm -rf "$target_dir"
}

if ! command -v nextflow >/dev/null 2>&1; then
    install_nextflow
else
    CURRENT_NXF_VER="$(nextflow -v 2>/dev/null | grep -oE '[0-9]+\.[0-9]+\.[0-9]+' | head -n 1 || echo "")"
    if [ "$CURRENT_NXF_VER" != "$NEXTFLOW_VER" ]; then
        log_info "Current Nextflow is ${CURRENT_NXF_VER}, upgrading to ${NEXTFLOW_VER}..."
        install_nextflow
    else
        log_success "Nextflow v${NEXTFLOW_VER} is already installed."
    fi
fi

# Pre-cache target version
NXF_VER="${NEXTFLOW_VER}" nextflow info >/dev/null 2>&1 || true
log_success "Nextflow ready: $(nextflow -v 2>/dev/null || echo "v${NEXTFLOW_VER}")"

# ==============================================================================
# 3. Docker Installation & Permissions
# ==============================================================================
if [ "$SKIP_DOCKER" = false ]; then
    log_info "Checking Docker..."
    if ! command -v docker >/dev/null 2>&1; then
        log_info "Docker not found. Installing Docker via official get.docker.com script..."
        if [ -n "$SUDO" ] || [ "$(id -u)" -eq 0 ]; then
            curl -fsSL https://get.docker.com | $SUDO sh
            $SUDO systemctl enable --now docker 2>/dev/null || true
            log_success "Docker installed successfully: $(docker --version 2>/dev/null || echo 'Installed')"
        else
            log_warn "Root/sudo privileges required to install Docker. Please install Docker manually."
        fi
    else
        log_success "Docker is already installed: $(docker --version 2>/dev/null || echo 'Found')"
    fi

    # Ensure user has non-root access to docker socket
    if command -v docker >/dev/null 2>&1 && ([ -n "$SUDO" ] || [ "$(id -u)" -eq 0 ]); then
        log_info "Configuring Docker group permissions for user '${SERVICE_USER}'..."
        $SUDO usermod -aG docker "$SERVICE_USER" 2>/dev/null || true
        if [ -n "${SUDO_USER:-}" ] && [ "${SUDO_USER}" != "$SERVICE_USER" ] && [ "${SUDO_USER}" != "root" ]; then
            $SUDO usermod -aG docker "$SUDO_USER" 2>/dev/null || true
        fi
        log_success "User '${SERVICE_USER}' added to 'docker' group."
    fi
else
    log_info "Skipping Docker installation (--skip-docker)."
fi

# ==============================================================================
# 4. Miniserve Installation
# ==============================================================================
log_info "Checking miniserve..."
if ! command -v miniserve >/dev/null 2>&1; then
    log_info "miniserve not found on PATH. Installing latest release..."
    ARCH="$(uname -m)"
    case "$ARCH" in
        x86_64)
            MINISERVE_ARCH="x86_64"
            ;;
        aarch64|arm64)
            MINISERVE_ARCH="aarch64"
            ;;
        *)
            MINISERVE_ARCH="x86_64"
            ;;
    esac

    # Query GitHub API to get exact asset URL (asset filenames contain version tags)
    MINISERVE_URL=""
    LATEST_JSON="$(curl -fsSL https://api.github.com/repos/svenstaro/miniserve/releases/latest 2>/dev/null || echo "")"
    if [ -n "$LATEST_JSON" ]; then
        MINISERVE_URL="$(echo "$LATEST_JSON" | grep "browser_download_url" | grep -E "${MINISERVE_ARCH}-unknown-linux-(musl|gnu)" | cut -d '"' -f 4 | head -n 1 || echo "")"
    fi

    # Fallback to direct known release if GitHub API is rate-limited
    if [ -z "$MINISERVE_URL" ]; then
        MINISERVE_URL="https://github.com/svenstaro/miniserve/releases/download/v0.29.0/miniserve-v0.29.0-${MINISERVE_ARCH}-unknown-linux-musl"
    fi

    TEMP_MINISERVE="/tmp/miniserve_$$"
    log_info "Downloading miniserve from ${MINISERVE_URL}..."
    if curl -fsSL -o "$TEMP_MINISERVE" "$MINISERVE_URL"; then
        chmod +x "$TEMP_MINISERVE"
        INSTALL_BIN_DIR="/usr/local/bin"
        if [ -w "$INSTALL_BIN_DIR" ]; then
            mv "$TEMP_MINISERVE" "$INSTALL_BIN_DIR/miniserve"
        elif [ -n "$SUDO" ]; then
            $SUDO mv "$TEMP_MINISERVE" "$INSTALL_BIN_DIR/miniserve"
        else
            mkdir -p "$HOME/.local/bin"
            mv "$TEMP_MINISERVE" "$HOME/.local/bin/miniserve"
            export PATH="$HOME/.local/bin:$PATH"
        fi
        log_success "miniserve installed successfully: $(miniserve --version 2>/dev/null || echo 'Installed')"
    else
        log_warn "Could not download precompiled miniserve. Please install miniserve manually."
    fi
else
    log_success "miniserve is already installed: $(miniserve --version 2>/dev/null || echo 'Found')"
fi

# ==============================================================================
# 5. Directory Structure & Permissions
# ==============================================================================
log_info "Setting up application directory structure..."
mkdir -p logs output work miniserve www
chmod +x bin/*.sh 2>/dev/null || true
log_success "Directories ready: logs/, output/, work/, miniserve/, www/"

# ==============================================================================
# 6. Configure .Renviron
# ==============================================================================
log_info "Configuring .Renviron..."
if [ ! -f .Renviron ]; then
    cat <<EOF > .Renviron
DEFAULT_PATH='${DATA_PATH_DEFAULT}'
MINISERVE_PATH='${MINISERVE_DIR_DEFAULT}'
EOF
    log_success "Created .Renviron with DEFAULT_PATH='${DATA_PATH_DEFAULT}' and MINISERVE_PATH='${MINISERVE_DIR_DEFAULT}'."
else
    log_info ".Renviron already exists. Preserving existing settings."
fi

# ==============================================================================
# 7. Setup credentials.rds
# ==============================================================================
log_info "Configuring credentials.rds..."
if [ -f "credentials.rds" ] && [ "$RESET_CREDENTIALS" = false ]; then
    log_info "credentials.rds already exists. Skipping (use --reset-credentials to overwrite)."
else
    if [ -z "$APP_USER" ]; then
        if [ "$NON_INTERACTIVE" = true ]; then
            APP_USER="$APP_USER_DEFAULT"
        else
            read -rp "Enter admin username for Shiny login [${APP_USER_DEFAULT}]: " input_user
            APP_USER="${input_user:-$APP_USER_DEFAULT}"
        fi
    fi

    if [ -z "$APP_PASS" ]; then
        if [ "$NON_INTERACTIVE" = true ]; then
            APP_PASS="$(tr -dc 'A-Za-z0-9' </dev/urandom | head -c 16 || openssl rand -base64 12)"
            log_warn "Generated random admin password: ${APP_PASS}"
        else
            while [ -z "$APP_PASS" ]; do
                read -rsp "Enter admin password for Shiny login: " input_pass
                echo ""
                if [ -z "$input_pass" ]; then
                    log_warn "Password cannot be empty. Please enter a valid password."
                else
                    APP_PASS="$input_pass"
                fi
            done
        fi
    fi

    log_info "Generating credentials.rds for user '${APP_USER}'..."
    Rscript -e "
      args <- commandArgs(trailingOnly = TRUE)
      user <- args[1]
      pass <- args[2]
      credentials <- data.frame(
        user = user,
        password = pass,
        admin = TRUE,
        comment = '',
        stringsAsFactors = FALSE
      )
      saveRDS(credentials, 'credentials.rds')
    " "$APP_USER" "$APP_PASS"

    chmod 600 credentials.rds
    log_success "credentials.rds generated successfully."
fi

# ==============================================================================
# 8. R Dependencies via renv & Posit Binary Repository
# ==============================================================================
# Detect distribution codename for Posit Package Manager Linux binaries (jammy, noble, focal)
DISTRO_CODENAME="jammy"
if [ -f /etc/os-release ]; then
    . /etc/os-release
    DISTRO_CODENAME="${VERSION_CODENAME:-jammy}"
fi
export DISTRO_CODENAME

# Check memory and enable swap if RAM is low (<4GB) to prevent OOM killer
if [ -f /proc/meminfo ]; then
    TOTAL_MEM_KB=$(grep MemTotal /proc/meminfo | awk '{print $2}')
    SWAP_TOTAL_KB=$(grep SwapTotal /proc/meminfo | awk '{print $2}')
    if [ "${TOTAL_MEM_KB:-0}" -lt 4194304 ] && [ "${SWAP_TOTAL_KB:-0}" -lt 1048576 ]; then
        log_info "Low RAM detected ($((TOTAL_MEM_KB / 1024))MB) with insufficient swap. Setting up 2GB swap to prevent OOM kills..."
        if [ ! -f /swapfile ] && ([ -n "$SUDO" ] || [ "$(id -u)" -eq 0 ]); then
            $SUDO fallocate -l 2G /swapfile 2>/dev/null || $SUDO dd if=/dev/zero of=/swapfile bs=1M count=2048
            $SUDO chmod 600 /swapfile
            $SUDO mkswap /swapfile
            $SUDO swapon /swapfile || true
            log_success "2GB swapfile enabled."
        elif [ -f /swapfile ]; then
            $SUDO swapon /swapfile 2>/dev/null || true
        fi
    fi
fi

log_info "Restoring R package dependencies (using pre-compiled binaries for Ubuntu ${DISTRO_CODENAME})..."
export MAKEFLAGS="-j1"

Rscript -e '
  distro <- Sys.getenv("DISTRO_CODENAME", "jammy")
  ppm_repo <- paste0("https://packagemanager.posit.co/cran/__linux__/", distro, "/latest")
  options(repos = c(CRAN = ppm_repo))
  options(HTTPUserAgent = sprintf("R/%s R (%s)", getRversion(), paste(getRversion(), R.version$platform, R.version$arch, R.version$os)))
  Sys.setenv(RENV_CONFIG_PPM_ENABLED = "TRUE")
  Sys.setenv(RENV_CONFIG_PPM_DEFAULT = "TRUE")

  if (!requireNamespace("renv", quietly = TRUE)) {
    message("Installing renv package...")
    install.packages("renv", repos = ppm_repo)
  }
  
  if (file.exists("renv.lock")) {
    message("Restoring environment from renv.lock...")
    # Use binary PPM repository during restore
    renv::restore(prompt = FALSE, repos = c(CRAN = ppm_repo))
  } else {
    message("renv.lock not found, installing required packages directly...")
    pkgs <- c("shiny", "shinyFiles", "shinyjs", "shinymanager", "bslib", 
              "bsicons", "stringr", "dplyr", "processx", "shinybusy", 
              "digest", "hover", "reactable", "prettyunits", "fs")
    for (pkg in pkgs) {
      if (!requireNamespace(pkg, quietly = TRUE)) {
        install.packages(pkg, repos = ppm_repo)
      }
    }
  }
'

log_info "Verifying required R packages..."
Rscript -e '
  pkgs <- c("shiny", "shinyFiles", "shinyjs", "shinymanager", "bslib", 
            "bsicons", "stringr", "dplyr", "processx", "shinybusy", 
            "digest", "hover", "reactable", "prettyunits", "fs")
  missing <- c()
  for (pkg in pkgs) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      missing <- c(missing, pkg)
    }
  }
  if (length(missing) > 0) {
    stop(paste("Missing required packages:", paste(missing, collapse = ", ")))
  }
'
log_success "All required R packages are installed and verified."

# ==============================================================================
# 9. Setup Systemd Services
# ==============================================================================
if [ "$SKIP_SYSTEMD" = false ] && command -v systemctl >/dev/null 2>&1; then
    log_info "Setting up systemd services for Miniserve and NXF-TGS Shiny app..."

    R_BIN="$(command -v R || echo "/usr/bin/R")"
    MINISERVE_BIN="$(command -v miniserve || echo "/usr/local/bin/miniserve")"
    SERVICE_HOME="$(eval echo "~${SERVICE_USER}")"

    # --- Miniserve Service ---
    MINISERVE_SERVICE_FILE="/etc/systemd/system/miniserve.service"
    cat <<EOF | $SUDO tee "$MINISERVE_SERVICE_FILE" >/dev/null
[Unit]
Description=Miniserve File Server for NXF TGS App
After=network.target

[Service]
Type=simple
User=${SERVICE_USER}
Group=${SERVICE_GROUP}
WorkingDirectory=${APP_DIR}
ExecStart=${MINISERVE_BIN} -I -p ${MINISERVE_PORT} --interfaces 0.0.0.0 ${MINISERVE_DIR_DEFAULT}
Restart=always
RestartSec=5
StandardOutput=append:${APP_DIR}/logs/miniserve.log
StandardError=append:${APP_DIR}/logs/miniserve.log

[Install]
WantedBy=multi-user.target
EOF
    log_success "Created systemd service: ${MINISERVE_SERVICE_FILE}"

    # --- Shiny App Service ---
    SHINY_SERVICE_FILE="/etc/systemd/system/nxf-tgs-app.service"
    cat <<EOF | $SUDO tee "$SHINY_SERVICE_FILE" >/dev/null
[Unit]
Description=NXF TGS Shiny App
After=network.target miniserve.service

[Service]
Type=simple
User=${SERVICE_USER}
Group=${SERVICE_GROUP}
WorkingDirectory=${APP_DIR}
Environment="PATH=/usr/local/bin:/usr/bin:/bin:${SERVICE_HOME}/.local/bin"
Environment="HOME=${SERVICE_HOME}"
ExecStart=${R_BIN} -e "shiny::runApp(appDir='${APP_DIR}', host='0.0.0.0', port=${SHINY_PORT})"
Restart=always
RestartSec=5
StandardOutput=append:${APP_DIR}/logs/shiny-app.log
StandardError=append:${APP_DIR}/logs/shiny-app.log

[Install]
WantedBy=multi-user.target
EOF
    log_success "Created systemd service: ${SHINY_SERVICE_FILE}"

    # Reload systemd and enable/start services
    $SUDO systemctl daemon-reload
    $SUDO systemctl enable --now miniserve.service
    $SUDO systemctl enable --now nxf-tgs-app.service

    log_success "Enabled and started miniserve.service and nxf-tgs-app.service."
else
    if [ "$SKIP_SYSTEMD" = true ]; then
        log_info "Skipping systemd service setup (--skip-systemd)."
    else
        log_warn "systemctl not available; skipping systemd service registration."
    fi
fi

# ==============================================================================
# 10. Firewall Configuration & Port Verification
# ==============================================================================
log_info "Checking firewall and opening ports (${SHINY_PORT}, ${MINISERVE_PORT})..."

# Check UFW
if command -v ufw >/dev/null 2>&1; then
    if $SUDO ufw status | grep -qw "active"; then
        log_info "UFW firewall is active. Opening ports ${SHINY_PORT}/tcp and ${MINISERVE_PORT}/tcp..."
        $SUDO ufw allow "${SHINY_PORT}/tcp" comment "NXF-TGS Shiny App" >/dev/null 2>&1 || true
        $SUDO ufw allow "${MINISERVE_PORT}/tcp" comment "Miniserve File Server" >/dev/null 2>&1 || true
        log_success "UFW firewall rules updated for ports ${SHINY_PORT} and ${MINISERVE_PORT}."
    else
        log_info "UFW is inactive. Ports ${SHINY_PORT} and ${MINISERVE_PORT} are unrestricted by UFW."
    fi
fi

# Check firewalld
if command -v firewall-cmd >/dev/null 2>&1 && systemctl is-active --quiet firewalld 2>/dev/null; then
    log_info "firewalld is active. Opening ports ${SHINY_PORT}/tcp and ${MINISERVE_PORT}/tcp..."
    $SUDO firewall-cmd --permanent --add-port="${SHINY_PORT}/tcp" >/dev/null 2>&1 || true
    $SUDO firewall-cmd --permanent --add-port="${MINISERVE_PORT}/tcp" >/dev/null 2>&1 || true
    $SUDO firewall-cmd --reload >/dev/null 2>&1 || true
    log_success "firewalld rules updated."
fi

# ==============================================================================
# 11. Service Health Check & Status Report
# ==============================================================================
if [ "$SKIP_SYSTEMD" = false ] && command -v systemctl >/dev/null 2>&1; then
    log_info "Verifying service status and health..."
    sleep 3

    # Check Miniserve
    MINISERVE_STATUS="$(systemctl is-active miniserve.service 2>/dev/null || echo 'inactive')"
    if [ "$MINISERVE_STATUS" = "active" ]; then
        log_success "miniserve.service is active (running)."
    else
        log_error "miniserve.service failed to start (status: ${MINISERVE_STATUS}). Recent logs:"
        $SUDO journalctl -u miniserve.service -n 15 --no-pager || true
    fi

    # Check Shiny App
    SHINY_STATUS="$(systemctl is-active nxf-tgs-app.service 2>/dev/null || echo 'inactive')"
    if [ "$SHINY_STATUS" = "active" ]; then
        log_success "nxf-tgs-app.service is active (running)."
    else
        log_error "nxf-tgs-app.service failed to start (status: ${SHINY_STATUS}). Recent logs:"
        $SUDO journalctl -u nxf-tgs-app.service -n 20 --no-pager || true
    fi

    # Test HTTP endpoint connectivity locally
    log_info "Testing local HTTP endpoints..."
    SHINY_HTTP_CODE="$(curl -s -o /dev/null -w "%{http_code}" --connect-timeout 5 "http://127.0.0.1:${SHINY_PORT}" 2>/dev/null || echo "000")"
    MINISERVE_HTTP_CODE="$(curl -s -o /dev/null -w "%{http_code}" --connect-timeout 3 "http://127.0.0.1:${MINISERVE_PORT}" 2>/dev/null || echo "000")"

    if [ "$SHINY_HTTP_CODE" != "000" ]; then
        log_success "Shiny App HTTP response: ${SHINY_HTTP_CODE} OK (http://127.0.0.1:${SHINY_PORT})"
    else
        log_warn "Shiny App not responding on http://127.0.0.1:${SHINY_PORT} yet. It may still be initializing R packages."
    fi

    if [ "$MINISERVE_HTTP_CODE" != "000" ]; then
        log_success "Miniserve HTTP response: ${MINISERVE_HTTP_CODE} OK (http://127.0.0.1:${MINISERVE_PORT})"
    else
        log_warn "Miniserve not responding on http://127.0.0.1:${MINISERVE_PORT} yet."
    fi
fi

# ==============================================================================
# 12. Deployment Summary & Usage Instructions
# ==============================================================================
IP_ADDR="$(hostname -I 2>/dev/null | awk '{print $1}' || echo "YOUR_SERVER_IP")"

echo ""
echo -e "${BOLD}${GREEN}===================================================${NC}"
echo -e "${BOLD}${GREEN}          Deployment Setup Completed!              ${NC}"
echo -e "${BOLD}${GREEN}===================================================${NC}"
echo ""
echo -e "${BOLD}Access URLs:${NC}"
echo -e "   Shiny App:  ${CYAN}http://${IP_ADDR}:${SHINY_PORT}${NC}"
echo -e "   File Share: ${CYAN}http://${IP_ADDR}:${MINISERVE_PORT}${NC}"
echo ""
echo -e "${BOLD}Service Status Summary:${NC}"
if command -v systemctl >/dev/null 2>&1 && [ "$SKIP_SYSTEMD" = false ]; then
    echo -e "   nxf-tgs-app.service: $(systemctl is-active nxf-tgs-app 2>/dev/null || echo 'inactive')"
    echo -e "   miniserve.service:   $(systemctl is-active miniserve 2>/dev/null || echo 'inactive')"
fi
echo ""
echo -e "${BOLD}Useful Management Commands:${NC}"
echo -e "   Check status:  sudo systemctl status nxf-tgs-app miniserve"
echo -e "   Restart app:   sudo systemctl restart nxf-tgs-app"
echo -e "   View app logs: sudo journalctl -u nxf-tgs-app -f"
echo -e "   Restart files: sudo systemctl restart miniserve"
echo ""
echo -e "${BOLD}Note:${NC} Nextflow pipelines will run with Docker/Singularity as configured."
echo ""
