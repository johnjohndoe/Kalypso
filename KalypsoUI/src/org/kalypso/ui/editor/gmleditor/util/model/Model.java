package org.kalypso.ui.editor.gmleditor.util.model;

public abstract class Model
{
  protected Model parent;

  protected String name;  

  public Model getParent()
  {
    return parent;
  }
  
  public abstract void remove(Model model);

  public String getName()
  {
    return name;
  }
}