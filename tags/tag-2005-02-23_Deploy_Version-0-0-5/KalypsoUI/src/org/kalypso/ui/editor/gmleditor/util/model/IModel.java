package org.kalypso.ui.editor.gmleditor.util.model;

public interface IModel
{
  public IModel getParent();
  
  public String getName();
  
  public boolean hasChildren();
  
  public IModel[] getChildren();

  public void addChild( final IModel model );
  
  public void removeChild( IModel model );
  
  /** Durchläuft dieses Model und rekursiv alle seine Kinder (fallse es welche hat) */
  public void accept( final IModelVisitor visitor );
}