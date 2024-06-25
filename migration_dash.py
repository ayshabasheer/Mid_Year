import dash
from dash import dcc, html
from dash.dependencies import Input, Output, State
import plotly.graph_objects as go
import pandas as pd
import numpy as np

# Load the data
df = pd.read_csv('/Users/user/Library/CloudStorage/OneDrive-TheUniversityofManchester/Migration/MidYear/Log-Linear/final_prop_com.csv')

# Rename columns
df = df.rename(columns={"proportion_value_em": "Emigration", "proportion_value_im": "Immigration", "value_est": "Estimated"})

# Melt the dataframe
df_melted = df.melt(id_vars=['origin', 'destination', 'year', 'age', 'sex'],
                    value_vars=['Estimated', 'Emigration', 'Immigration'],
                    var_name='value_type', value_name='value')

# Initialize the Dash app
app = dash.Dash(__name__)

# Define color mapping for value types
color_map = {
    'Estimated': '#1f77b4',  # blue
    'Immigration': '#2ca02c',   # green
    'Emigration': '#d62728'    # red
}

# Define the layout
app.layout = html.Div([
    html.H1("Migration Flow Visualization"),
    
    html.Div([
        html.Div([
            html.Label("Origin:"),
            dcc.Dropdown(
                id='origin-dropdown',
                options=[{'label': i, 'value': i} for i in sorted(df_melted['origin'].unique())],
                value=df_melted['origin'].iloc[0]
            )
        ], style={'width': '30%', 'display': 'inline-block', 'marginRight': '5%'}),
        
        html.Div([
            html.Label("Destination:"),
            dcc.Dropdown(
                id='destination-dropdown',
                options=[{'label': i, 'value': i} for i in sorted(df_melted['destination'].unique())],
                value=df_melted['destination'].iloc[0]
            )
        ], style={'width': '30%', 'display': 'inline-block', 'marginRight': '5%'}),
        
        html.Div([
            html.Label("Year:"),
            dcc.Dropdown(
                id='year-dropdown',
                options=[{'label': str(i), 'value': i} for i in sorted(df_melted['year'].unique())],
                value=df_melted['year'].iloc[0]
            )
        ], style={'width': '30%', 'display': 'inline-block'})
    ]),
    
    html.Div([
        html.Label("Sex:"),
        dcc.Checklist(
            id='gender-checklist',
            options=[
                {'label': 'Female', 'value': 'F'},
                {'label': 'Male', 'value': 'M'}
            ],
            value=['F', 'M'],
            inline=True
        )
    ], style={'marginTop': '20px', 'marginBottom': '20px'}),
    
    html.Div(id='warning-message', style={'color': 'red', 'marginBottom': '10px'}),
    
    dcc.Graph(id='migration-graph')
])

# Define the callback to update the graph and show warning
@app.callback(
    [Output('migration-graph', 'figure'),
     Output('warning-message', 'children')],
    [Input('origin-dropdown', 'value'),
     Input('destination-dropdown', 'value'),
     Input('year-dropdown', 'value'),
     Input('gender-checklist', 'value')]
)
def update_graph(origin, destination, year, genders):
    if origin == destination:
        return {}, "Warning: Origin and destination cannot be the same."
    
    filtered_df = df_melted[
        (df_melted['origin'] == origin) & 
        (df_melted['destination'] == destination) & 
        (df_melted['year'] == year) &
        (df_melted['sex'].isin(genders))
    ]
    
    fig = go.Figure()
    
    for value_type in filtered_df['value_type'].unique():
        for gender in genders:
            data = filtered_df[(filtered_df['value_type'] == value_type) & (filtered_df['sex'] == gender)]
            if value_type == 'Estimated':
                ci = 1.96 * np.std(data['value']) / np.sqrt(len(data['value']))
                error_y = dict(type='data', array=[ci]*len(data), visible=True, color=color_map[value_type])
            else:
                error_y = None
            fig.add_trace(go.Scatter(
                x=data['age'],
                y=data['value'],
                mode='lines+markers',
                name=f'{value_type} ({gender})',
                line=dict(color=color_map[value_type]),
                marker=dict(
                    color=color_map[value_type],
                    symbol='circle' if gender == 'F' else 'square',
                    size=8
                ),
                error_y=error_y
            ))
    
    fig.update_layout(
        title=f'Migration Flow from {origin} to {destination} in {year}',
        xaxis_title='Age Group',
        yaxis_title='Migration Proportion'#,
        #legend_title='Migration flow and Gender'
    )
    
    return fig, ""

# Run the app
if __name__ == '__main__':
    app.run_server(debug=True)