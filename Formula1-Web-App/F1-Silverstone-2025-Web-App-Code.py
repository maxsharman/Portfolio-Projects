# ---------------------- F1 Web App Code ----------------------
#Importing Libraries
import streamlit as st
import fastf1
import pandas as pd
import plotly.express as px

# Adding a Title and applied a different font from Google fonts. I also used the F1 Red Hex code colour for the title.
st.markdown("""
    <link href="https://fonts.googleapis.com/css2?family=Rajdhani:wght@700&display=swap" rel="stylesheet">
    <h1 style='font-family: "Rajdhani", sans-serif; color: #FF1801;'>
        Silverstone 2025 F1 Dashboard
    </h1>
    """, unsafe_allow_html=True)

# Here is HTML code to create a fade down animation when opening the Web App
st.markdown("""
    <style>
    /* Animation */
    @keyframes fadeDown {
        from {
            opacity: 0;
            transform: translateY(-30px);
        }
        to {
            opacity: 1;
            transform: translateY(0);
        }
    }

    /* Title */
    h1 {
        animation: fadeDown 3s ease-out;
    }

    /* Apply to all content */
    .stApp > div {
        animation: fadeDown 0.8s ease-out;
    }
    </style>
    """, unsafe_allow_html=True)

# Added descriptive text
st.write("Welcome to my F1 app! This Web App showcases data insights "
         "from the memorable 2025 Silverstone Grand Prix. This was the 76th Grand Prix held at Silverstone!")

# Creating a cache to load data quickly. There is a lot of data here so this is necessary
@st.cache_data
def load_race_laps(year, race):
    session = fastf1.get_session(year, race, 'R')
    session.load()
    laps = session.laps
    return laps

# Using HTML to add gradient shapes in the background to create a racing blur effect.
# This is to try and build upon the F1 theme. I experimented with different gradient values to create this effect.
st.markdown("""
    <style>
    .stApp {
        background: 
            linear-gradient(90deg, 
                rgba(255, 24, 1, 0.15) 0%,
                transparent 30%,
                transparent 70%,
                rgba(255, 24, 1, 0.15) 100%
            ),
            linear-gradient(90deg,
                #181818 0%,
                #1a1a1a 50%,
                #181818 100%
            );
        background-size: 200% 100%;
    }

    .stApp::before {
        content: '';
        position: fixed;
        top: 0;
        left: -50%;
        right: -50%;
        bottom: 0;
        background: repeating-linear-gradient(
            90deg,
            transparent,
            transparent 100px,
            rgba(255, 24, 1, 0.03) 100px,
            rgba(255, 24, 1, 0.03) 200px,
            transparent 200px,
            transparent 250px,
            rgba(255, 24, 1, 0.06) 250px,
            rgba(255, 24, 1, 0.06) 255px
        );
        pointer-events: none;
        filter: blur(1px);
    }
    </style>
    """, unsafe_allow_html=True)

# Loading the Silverstone 2025 Grand Prix using the fastf1 package
laps = load_race_laps(2025, 'Silverstone')
# Finding laps for each driver
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

# Creating a multiselect dropdown menu to select drivers
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

    # Show the data with formatted time instead of seconds
    st.write(f"Total laps for selected drivers: {len(filtered_laps)}")
    display_df = filtered_laps[['Driver', 'LapNumber', 'LapTimeFormatted', 'Compound', 'Stint']]  # Changed here!
    st.dataframe(display_df)

# Creating the first interactive figure.
    # This is a line graph for lap time evolution.
    # This will show change of lap times over the whole race for the selected drivers.
    st.write("Lap Time Evolution")
    st.write("Note: Between laps 14-18 a safety car was deployed this was followed by a crash immediately after. The race restarted on lap 22.")
# Creating the line graph and setting x & y
    figone = px.line(
        filtered_laps,
        x='LapNumber',
        y='LapTimeSeconds',
        color='Driver',
        color_discrete_map=driver_colours,
        title='Lap Time Evolution',
        hover_data={'LapTimeFormatted': True, 'LapTimeSeconds': False}
    )
# Updating the figure to improve overall presentation. Adding colours and a new font to match with the F1 theme
    figone.update_layout(
        title=' Lap Time Evolution',
        xaxis_title='Lap Number',
        yaxis_title='Lap Time Seconds',
        plot_bgcolor='#1a1a1a',
        paper_bgcolor='#181818',
        font=dict(family='Rajdhani', color='white')
    )

# Adding plot to the streamlit app.
    # I also added a warning in case the user did not select a driver from the multiselect drop down.
    st.plotly_chart(figone, use_container_width=True)
else:
    st.warning("Please select at least one driver")


# Tire Strategy Visualisation
# This is the second visualisation for tire strategy.
# This will show what tires each driver used and swapped to during their pit stops.
st.write("Tire Strategy Comparison")

import plotly.graph_objects as go

# Tire colours. Each tire is represented by a different colour.
tire_colors = {
    'SOFT': '#FF1801',
    'MEDIUM': '#FFD700',
    'HARD': '#FFFFFF',
    'INTERMEDIATE': '#39B54A',
    'WET': '#0066CC'
}

fig = go.Figure()

# For each driver
for driver in drivers:
    driver_laps = laps[laps['Driver'] == driver].copy()

    # Group consecutive laps with same tire compound
    driver_laps['CompoundChange'] = (driver_laps['Compound'] != driver_laps['Compound'].shift()).cumsum()

    stints = driver_laps.groupby('CompoundChange').agg({
        'LapNumber': ['min', 'max'],
        'Compound': 'first'
    }).reset_index(drop=True)

    # Drawing each stint as a bar
    for i in range(len(stints)):
        start_lap = stints['LapNumber']['min'].iloc[i]
        end_lap = stints['LapNumber']['max'].iloc[i]
        compound = stints['Compound']['first'].iloc[i]
        stint_length = end_lap - start_lap + 1

        fig.add_trace(go.Bar(
            x=[stint_length],
            y=[driver],
            orientation='h',
            base=[start_lap],
            marker=dict(color=tire_colors.get(compound, '#888888')),
            showlegend=False,
            hovertext=f'{driver}<br>{compound}<br>Laps {start_lap}-{end_lap}'
        ))

# Adding legend for the tire colours
for compound, color in tire_colors.items():
    fig.add_trace(go.Bar(
        x=[None], y=[None],
        marker=dict(color=color),
        name=compound
    ))

# Customizing the layout for the figure by changing the font and titles.
fig.update_layout(
    title='Tire Strategy',
    xaxis_title='Lap Number',
    yaxis_title='Driver',
    plot_bgcolor='#1a1a1a',
    paper_bgcolor='#181818',
    font=dict(family='Rajdhani', color='white'),
    barmode='stack',
    height=600
)

st.plotly_chart(fig, use_container_width=True)