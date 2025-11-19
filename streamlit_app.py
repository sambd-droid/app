# app.py
import streamlit as st
from streamlit_folium import st_folium
import folium
import ee
import geemap.foliumap as geemap
import json

# ---------- Install / Setup notes ----------
# If you haven't installed packages, run in your environment:
# pip install earthengine-api geemap streamlit streamlit-folium folium

# Authenticate & initialize Earth Engine
# For local dev, first run: ee.Authenticate()  (this opens a browser the first time)
# If using a service account or headless server, set credentials appropriately.

PROJECT_ID = "pdr-jnu" # Define the project ID
SERVICE_ACCOUNT_KEY_FILE = "pdr-jnu-61e21aa3f147.json" # Path to your service account key file

try:
    # Read the service account email from the JSON key file
    with open(SERVICE_ACCOUNT_KEY_FILE, 'r') as f:
        credentials_data = json.load(f)
    service_account_email = credentials_data['client_email']

    # Use Service Account credentials for authentication
    credentials = ee.ServiceAccountCredentials(
        service_account_email,
        key_file=SERVICE_ACCOUNT_KEY_FILE
    )
    ee.Initialize(credentials=credentials, project=PROJECT_ID)
except FileNotFoundError:
    st.error(f"Service account key file not found at: {SERVICE_ACCOUNT_KEY_FILE}")
    st.stop() # Stop the Streamlit app if key file is missing
except KeyError:
    st.error("Invalid service account key file: 'client_email' not found.")
    st.stop() # Stop the Streamlit app if key file is invalid
except Exception as e:
    st.error(f"Failed to initialize Earth Engine with service account: {e}")
    st.info("Please ensure the service account key file is correct and accessible, and that the project ID is valid.")
    st.stop() # Stop the Streamlit app if EE initialization fails

st.set_page_config(layout="wide")
st.title("PDR from Map Picker")

# ---------- Configure your PDR image ----------
# Replace this with the correct EE asset or computation that produces PDR.
# Examples:
# - ee.Image('users/yourusername/pdr_asset')
# - or derived from bands: ee.Image(...calculation...)
pdr_image = ee.Image('projects/pdr-jnu/assets/pdr')  # <<-- REPLACE THIS with your actual asset

# Optionally set visualization parameters for the map layer
viz_params = {
    'min': 0,
    'max': 100,
    'palette': ['blue', 'green', 'yellow', 'red']
}

# ---------- Create folium map ----------
m = geemap.Map(center=[23.8, 90.4], zoom=7)  # center on Bangladesh (example)

# Get Earth Engine Tile URL and add as a Folium TileLayer
map_id_dict = pdr_image.visualize(**viz_params).getMapId()
tile_url = map_id_dict['tile_fetcher'].url_format

folium.TileLayer(
    tiles=tile_url,
    attr='Google Earth Engine',
    name='PDR layer',
    overlay=True,
    control=True
).add_to(m)

# Add a click popup to show lat/lon (optional)
m.add_child(folium.LatLngPopup())

# Display the map with streamlit_folium and capture returned dict
map_data = st_folium(m, width=700, height=500)

# ---------- Handle click ----------
clicked = map_data.get('last_clicked') if map_data else None
if clicked:
    lat = clicked['lat']
    lon = clicked['lng']
    st.write(f"Clicked at: latitude {lat:.6f}, longitude {lon:.6f}")

    # Build EE point and query reduceRegion
    pt = ee.Geometry.Point([lon, lat])
    try:
        # Choose the band name that contains PDR; here we assume single-band named 'b1' or 'pdr'
        # If your image has a specific band, change 'pdr_band' accordingly.
        # Example: pdr_image.select('pdr_band')
        region_dict = pdr_image.reduceRegion(
            reducer=ee.Reducer.first(),
            geometry=pt,
            scale=30,            # adjust to your image's resolution
            maxPixels=1e13,
            bestEffort=True
        ).getInfo()
    except Exception as e:
        st.error(f"Error querying Earth Engine: {e}")
        region_dict = None

    if region_dict:
        # region_dict is a dict like {'band_name': value}
        st.write("PDR values at the clicked point:")
        for band, val in region_dict.items():
            st.write(f"  {band} : {val}")
    else:
        st.warning("No value returned at this point (maybe the point is outside the image or contains masked data).")

else:
    st.info("Click the map to get the PDR value at that location.")

# ---------- Optional: Allow user to set parameters ----------
with st.expander("Query options"):
    scale = st.number_input("Scale (meters)", value=30)
    band_name = st.text_input("Band to read (optional; leave empty to show all bands)", value="")
    if st.button("Re-query last click (if any)"):
        if not clicked:
            st.warning("No click to re-query. Please click on the map first.")
        else:
            pt = ee.Geometry.Point([clicked['lng'], clicked['lat']])
            img = pdr_image.select(band_name) if band_name else pdr_image
            try:
                region_dict = img.reduceRegion(ee.Reducer.first(), pt, scale=int(scale), bestEffort=True, maxPixels=1e13).getInfo()
                st.write(region_dict)
            except Exception as e:
                st.error(f"Error: {e}")
