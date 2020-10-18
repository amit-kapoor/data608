import pandas as pd
import dash
import dash_core_components as dcc
import dash_html_components as html
from dash.dependencies import Input, Output
import plotly.express as px
import numpy as np

external_stylesheets = ['https://codepen.io/chriddyp/pen/bWLwgP.css']

app = dash.Dash(__name__, external_stylesheets=external_stylesheets)

url = 'https://data.cityofnewyork.us/resource/nwxe-4ae8.json'

soql_url = ('https://data.cityofnewyork.us/resource/nwxe-4ae8.json?' +\
        '$select=spc_common,boroname,health,steward,count(tree_id)' +\
        '&$group=spc_common,boroname,health,steward').replace(' ', '%20')

# read json
trees = pd.read_json(url)

# select required columns
trees = trees[["boroname", "spc_common", "health", "steward"]]

# load unique boroughs
def load_borough():
	boros = trees['boroname']
	return boros.dropna().unique()

# load unique species
def load_species():
	sprecies = trees['spc_common']
	return sprecies.dropna().unique()

# load trees
def load_trees():
    trees = pd.DataFrame()
    #for boro in ['Bronx', 'Brooklyn', 'Manhattan', 'Queens', 'Staten Island']:
    for boro in load_borough().tolist():
        soql_url = ('https://data.cityofnewyork.us/resource/nwxe-4ae8.json?' + \
                    '$select=spc_common,count(tree_id),health,steward' + \
                    '&$where=boroname=\'{}\'' + \
                    '&$group=spc_common,health,steward').format(boro).replace(' ', '%20')
        soql_trees = pd.read_json(soql_url)
        soql_trees['borough'] = boro
        soql_trees = soql_trees.dropna()
        trees = trees.append(soql_trees)
    return trees

# layout
app.layout = html.Div([

	html.H1(children='NYC Tree Species Health'),
    html.Div(className='row',  # Define the row element
        children=[
            html.Div(className='two columns div-user-controls',
                children=[
                html.Label('Select borough:'), 
				dcc.Dropdown(
			        id='borough',
			        options=[
			        {'label': bor, 'value': bor} for bor in load_borough()
			        ],
			        value='Manhattan'
			    ),
				html.Label('Select species:'), 
				dcc.Dropdown(
			        id='species',
			        options=[
			        {'label': spc, 'value': spc} for spc in load_species()
			        ],
			        value='pin oak'
			    )
    		]),
    		html.Div(className='ten columns div-user-controls',
                children=[
                    html.Div(className='row',  
                        children=[
                            html.Div(className='six columns div-for-lower-left-chart',
                                children=[html.Div(id='tree_prop_by_health')]
                            )
                        ]
                    ),
                    html.Div(className='six columns div-for-lower-right-chart',
                        children=[
                            dcc.Graph(
                            	id='stewards_on_health'
                            )
                        ]
                    )
                ]
            ) 
    ])
])

# For question 1
@app.callback(
    Output('tree_prop_by_health', component_property='children'),
    [Input('borough', 'value'),
    Input('species', 'value')])
def draw_health_hist(input_boro, input_specy):
    trees_prop = trees[(trees.spc_common==input_specy)&(trees.boroname==input_boro)]

    return dcc.Graph(
            id='Health by Borough',
            figure={
                    'data':[{'x':trees_prop['health'], 'type': 'histogram', 'name': 'Health by Borough'}],
          			'layout':{'title':"Health by Borough"}
          	})

# For question 2
@app.callback(
    Output('stewards_on_health', component_property='figure'),
    [Input('borough', 'value'),
    Input('species', 'value')])
def draw_steward_hist(input_boro, input_specy):
	#get tree info
    trees = load_trees()
    # select using input
    trees = trees[(trees.spc_common==input_specy)&(trees.borough==input_boro)]
    # pivot trees
    trees_pvt = pd.pivot_table(trees, index='steward',values='count_tree_id', columns='health', aggfunc=np.sum)
    trees_pvt = trees_pvt.fillna(0)
    trees_pvt = trees_pvt.reset_index()
    # for proposition
    tree_wide = pd.concat([trees_pvt['steward'],trees_pvt[['Fair','Good','Poor']].apply(lambda x: 100 * x / float(x.sum()),axis=1)], axis=1)
    # to long
    tree_long = pd.melt(tree_wide, var_name='health', value_name='proportion', value_vars=['Poor','Fair','Good'],id_vars='steward')
    tree_long.steward = pd.Categorical(tree_long.steward, categories=["None", "1or2", "3or4","4orMore"],ordered=True)
    tree_long.health = pd.Categorical(tree_long.health, categories=["Poor", "Fair", "Good"],ordered=True)
    tree_long = tree_long.sort_values(['steward','health'], axis=0)
    # draw graph
    fig = px.bar(tree_long, x="steward", y='proportion', color='health', barmode="group")
    return fig
    
# run
if __name__ == '__main__':
    app.run_server(debug=True)



