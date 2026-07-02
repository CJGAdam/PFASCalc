# ==============================================================================
# Shinylive Deployment & Gatekeeper Injection Script
# ==============================================================================

message("Step 1: Staging specific files to optimize WebAssembly payload...")

# 1. Create a temporary staging directory
build_dir <- "build_stage"
if (dir.exists(build_dir)) unlink(build_dir, recursive = TRUE)
dir.create(build_dir)
dir.create(file.path(build_dir, "data", "rds"), recursive = TRUE)

# 2. Copy ONLY the required files into the staging directory
file.copy("app.R", file.path(build_dir, "app.R"))
file.copy("data/rds/Table_1.rds", file.path(build_dir, "data/rds/Table_1.rds"))
file.copy("data/rds/Table_7.rds", file.path(build_dir, "data/rds/Table_7.rds"))
file.copy("data/rds/Table_8.rds", file.path(build_dir, "data/rds/Table_8.rds"))

message("Step 2: Defining zero-second HTML gatekeeper...")

# 3. Define the Gatekeeper as pure HTML
gatekeeper_html <- '
<div id="simple-gatekeeper" style="position: fixed; top: 0; left: 0; width: 100vw; height: 100vh; background: #f8f9fa; z-index: 999999; display: flex; flex-direction: column; justify-content: center; align-items: center; font-family: system-ui, sans-serif;">
  <div style="box-shadow: 0 4px 6px rgba(0,0,0,0.1); padding: 3rem; border-radius: 8px; background: white; text-align: center; max-width: 400px;">
    <!-- Lock Icon SVG -->
    <svg xmlns="http://www.w3.org/2000/svg" width="48" height="48" fill="#2c3e50" viewBox="0 0 16 16" style="margin-bottom: 1rem;">
      <path d="M8 1a2 2 0 0 1 2 2v4H6V3a2 2 0 0 1 2-2zm3 6V3a3 3 0 0 0-6 0v4a2 2 0 0 0-2 2v5a2 2 0 0 0 2 2h6a2 2 0 0 0 2-2V9a2 2 0 0 0-2-2z"/>
    </svg>
    
    <h3 style="color: #2c3e50; font-weight: bold; margin-bottom: 1rem;">Restricted Access</h3>
    <p style="color: #6c757d; margin-bottom: 1.5rem;">This PFAS calculator is an internal tool for authorized facility users only.</p>
    
    <div style="display: flex; gap: 8px;">
      <input type="password" id="gate_pass" placeholder="Enter access code..." style="padding: 8px; border: 1px solid #ced4da; border-radius: 4px; flex-grow: 1;" onkeypress="if(event.key === \'Enter\') document.getElementById(\'unlock_btn\').click();">
      <button id="unlock_btn" style="padding: 8px 16px; background-color: #2c3e50; color: white; border: none; border-radius: 4px; font-weight: bold; cursor: pointer;" onclick="
        if(document.getElementById(\'gate_pass\').value === \'NPRI2026\') {
          document.getElementById(\'simple-gatekeeper\').style.display = \'none\';
        } else {
          alert(\'Incorrect access code.\');
          document.getElementById(\'gate_pass\').value = \'\';
        }
      ">Unlock</button>
    </div>
  </div>
</div>
'

message("Step 3: Building Shinylive app and injecting gatekeeper natively...")

# 4. Export using native template_params instead of appending post-build
shinylive::export(
  appdir = build_dir, 
  destdir = "docs",
  template_params = list(
    include_after_body = gatekeeper_html
  )
)

# 5. Clean up the temporary staging directory
unlink(build_dir, recursive = TRUE)

message("=====================================================")
message(" SUCCESS: App is ready for testing or deployment!")
message("=====================================================")
message(" -> To test locally, run this in your console:")
message("    httpuv::runStaticServer('docs/')")