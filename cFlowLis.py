class ControlFlowGraphPaths:
    def __init__(self):
        self.data  =self 
    # the code takes the graphs branches as given below.
    # for example if one  code do have the branching as given below it do the all possible paths for the 
    # given code   
    branches = {'A': ['B', 'C'],
             'B': ['C', 'D'],
             'C': ['D'],
             'D': ['C'],
             'E': ['F'],
             'F': ['C']}

    def find_all_paths(self,branches, start, end, flow_path=[]):
        self.flow_path = flow_path + [start]
        if start == end:
            return [flow_path]
        if not flow_path.has_key(start):
            return []
        paths = []
        for node in branches[start]:
            if node not in flow_path:
                newpaths = find_all_paths(self,branches, node, end,  flow_path)
                for newpath in newpaths:
                    paths.append(newpath)
        return paths
   
 
 
 