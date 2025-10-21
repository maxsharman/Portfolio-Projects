#Importing Libraries
import streamlit as st
import fastf1
import pandas as pd
import plotly.express as px
import matplotlib.pyplot as plt
import numpy as np
from matplotlib import colormaps
from matplotlib.collections import LineCollection

#Adding a Title
st.title("My F1 Silverstone 2025 Dashboard")
st.write("Welcome to my F1 app!")

# Creating a cache to load data quickly
@st.cache_data
def load_race_laps(year, race):
    session = fastf1.get_session(year, race, 'R')
    session.load()
    laps = session.laps
    return laps

# Loading the Silverstone 2025 Gran Prix
laps = load_race_laps(2025, 'Silverstone')
# Finding laps for unique drivers
drivers = laps['Driver'].unique()
# Adding team colours to drivers
driver_colours = {
    'VER' : '#3671C6',
    'TSU' : '#3671C6',
    'NOR' : '#FF8000',
    'PIA' : '#FF8000',
    'LEC' : '#E8002D',
    'HAM' : '#E8002D',
    'RUS' : '#27F4D2',
    'ANT' : '#27F4D2',
    'ALB' : '#64C4FF',
    'SAI' : '#64C4FF',
    'LAW' : '#3671C6',
    'HAD' : '#3671C6',
    'ALO' : '#229971',
    'STR' : '#229971',
    'HUL' : '#52E252',
    'BOR' : '#52E252',
    'OCO' : '#B6BABD',
    'BEA' : '#B6BABD',
    'GAS' : '#0093CC',
    'COL' : '#0093CC'
}

# Creating a list for the coloured names
coloured_names = []
for driver in drivers:
    # Get the colour for this driver
    colour = driver_colours.get(driver, '#FFFFFF')

    # Creating an HTML span tag. This will show each drivers team colour on their name.
    html = f"<span style='color:{colour};font-weight:bold;'>{driver}</span>"

    coloured_names.append(html)

# Joining the drivers names together with commas
st.write("Available drivers:")
st.markdown(", ".join(coloured_names), unsafe_allow_html=True)

# Creating a multiselect dropdown menu
selected_drivers = st.multiselect(
    "Select drivers to compare (max 5 recommended):",
    options=drivers,
    default=['LEC', 'NOR', 'HUL']  # Starting with a selected few
)

# Filter laps for selected drivers
if selected_drivers:
    filtered_laps = laps[laps['Driver'].isin(selected_drivers)].copy()
# Reformatting the lap times to show Minutes:Seconds:Milliseconds
    filtered_laps['LapTimeSeconds'] = filtered_laps['LapTime'].dt.total_seconds()

    filtered_laps['LapTimeFormatted'] = filtered_laps['LapTimeSeconds'].apply(
        lambda x: f"{int(x // 60)}:{x % 60:06.3f}" if pd.notna(x) else None
    )

    # Show the data with FORMATTED time instead of seconds
    st.write(f"Total laps for selected drivers: {len(filtered_laps)}")
    display_df = filtered_laps[['Driver', 'LapNumber', 'LapTimeFormatted', 'Compound', 'Stint']]  # Changed here!
    st.dataframe(display_df)

    # Figure 1 Lap Times
    figone = px.line(
        filtered_laps,
        x='LapNumber',
        y='LapTimeSeconds',
        color='Driver',
        color_discrete_map=driver_colours,
        title='Lap Time Evolution',
        hover_data={'LapTimeFormatted': True, 'LapTimeSeconds': False}
    )

    st.plotly_chart(figone, use_container_width=True)
else:
    st.warning("Please select at least one driver")

# Figure2 Braking Zones

# Load session
@st.cache_data
def load_session(year, race):
    session = fastf1.get_session(year, race, 'R')
    session.load()
    return session

session = load_session(2025, 'Silverstone')

# Get all drivers who completed laps
drivers_with_laps = sorted(session.laps['Driver'].unique())

# Driver selector
selected_driver = st.selectbox(
    "Select Driver:",
    options=drivers_with_laps,
    index=0
)

# Get fastest lap for selected driver
@st.cache_data
def get_driver_fastest_lap(year, race, driver):
    session = fastf1.get_session(year, race, 'R')
    session.load()
    driver_laps = session.laps.pick_driver(driver)
    fastest_lap = driver_laps.pick_fastest()
    tel = fastest_lap.get_telemetry()
    return tel, fastest_lap

tel, lap = get_driver_fastest_lap(2025, 'Silverstone', selected_driver)

# Create the track map. Track map code adapted from fastf1 example code
x = np.array(tel['X'].values)
y = np.array(tel['Y'].values)

points = np.array([x, y]).T.reshape(-1, 1, 2)
segments = np.concatenate([points[:-1], points[1:]], axis=1)
gear = tel['nGear'].to_numpy().astype(float)

# Create figure
fig, ax = plt.subplots(figsize=(10, 6))

cmap = colormaps['Paired']
lc_comp = LineCollection(segments, norm=plt.Normalize(1, cmap.N+1), cmap=cmap)
lc_comp.set_array(gear)
lc_comp.set_linewidth(4)

ax.add_collection(lc_comp)
ax.axis('equal')
ax.tick_params(labelleft=False, left=False, labelbottom=False, bottom=False)

plt.suptitle(
    f"Fastest Lap Gear Shift Visualisation\n"
    f"{lap['Driver']} - {session.event['EventName']} {session.event.year}"
)

cbar = plt.colorbar(mappable=lc_comp, label="Gear", ax=ax,
                    boundaries=np.arange(1, 10))
cbar.set_ticks(np.arange(1.5, 9.5))
cbar.set_ticklabels(np.arange(1, 9))

# Display in Streamlit
st.pyplot(fig)
plt.close(fig)

# Show lap time
st.metric("Fastest Lap Time", str(lap['LapTime'])[10:])
