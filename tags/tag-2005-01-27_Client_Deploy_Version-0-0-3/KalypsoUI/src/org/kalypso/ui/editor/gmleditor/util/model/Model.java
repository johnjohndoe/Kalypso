package org.kalypso.ui.editor.gmleditor.util.model;

public abstract class Model
{
  protected Model parent;

  protected String name;

  protected IGMLDocumentListener listener = NullDeltaListener.getSoleInstance();
  

  public Model getParent()
  {
    return parent;
  }
  
  public abstract void remove(Model model);

  /*
   * The receiver should visit the toVisit object and pass along the argument.
   */
  public abstract void accept( IModelVisitor visitor, Object passAlongArgument );

  public String getName()
  {
    return name;
  }

  public void addListener( IGMLDocumentListener m_listener )
  {
    this.listener = m_listener;
  }

  public void removeListener( IGMLDocumentListener m_listener )
  {
    if( this.listener.equals( m_listener ) )
    {
      this.listener = NullDeltaListener.getSoleInstance();
    }
  }
}