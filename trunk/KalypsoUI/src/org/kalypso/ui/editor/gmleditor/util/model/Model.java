package org.kalypso.ui.editor.gmleditor.util.model;

import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;

public abstract class Model implements IModel
{
  private IModel m_parent;

  private String m_name;

  private List m_children = new LinkedList();

  protected Model( final IModel parent, final String name )
  {
    m_parent = parent;
    m_name = name;

    if( parent != null )
      parent.addChild( this );
  }

  public IModel getParent()
  {
    return m_parent;
  }

  public String getName()
  {
    return m_name;
  }

  /**
   * @see org.kalypso.ui.editor.gmleditor.util.model.IModel#getChildren()
   */
  public IModel[] getChildren()
  {
    return (IModel[])m_children.toArray( new IModel[m_children.size()] );
  }

  /**
   * @see org.kalypso.ui.editor.gmleditor.util.model.IModel#hasChildren()
   */
  public boolean hasChildren()
  {
    return m_children.size() != 0;
  }

  /**
   * @see org.kalypso.ui.editor.gmleditor.util.model.IModel#addChild(org.kalypso.ui.editor.gmleditor.util.model.IModel)
   */
  public void addChild( IModel model )
  {
    m_children.add( model );
  }

  public void removeChild( final IModel model )
  {
    m_children.remove( model );
  }

  /** Durchläuft dieses Model und rekursiv alle seine Kinder (fallse es welche hat) */
  public void accept( final IModelVisitor visitor )
  {
    if( visitor.visit( this ) )
    {
      for( final Iterator childIt = m_children.iterator(); childIt.hasNext(); )
      {
        final IModel child = (IModel)childIt.next();
        child.accept( visitor );
      }
    }
  }
}