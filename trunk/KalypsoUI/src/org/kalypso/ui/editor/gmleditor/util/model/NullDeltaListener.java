package org.kalypso.ui.editor.gmleditor.util.model;

public class NullDeltaListener implements IGMLDocumentListener
{
  protected static NullDeltaListener soleInstance = new NullDeltaListener();

  public static NullDeltaListener getSoleInstance()
  {
    return soleInstance;
  }

  /*
   * @see IDeltaListener#add(DeltaEvent)
   */
  public void add( GMLDocumentEvent event )
  {
  /**/
  }

  /*
   * @see IDeltaListener#remove(DeltaEvent)
   */
  public void remove( GMLDocumentEvent event )
  {
  /**/
  }

  /**
   * @see org.kalypso.ui.editor.gmleditor.util.model.IGMLDocumentListener#onChange(org.kalypso.ui.editor.gmleditor.util.model.GMLDocumentEvent)
   */
  public void onChange( GMLDocumentEvent event )
  {}

}