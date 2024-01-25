import visualkeras

def view_model(model):
  visualkeras.layered_view(model).show() # display using your system viewer
  visualkeras.layered_view(model, to_file='output.png') # write to disk
  visualkeras.layered_view(model, to_file='output.png').show() # write and show
  
  return visualkeras.layered_view(model)
