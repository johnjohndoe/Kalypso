package org.kalypso.editor.mapeditor;

import org.deegree.graphics.sld.UserStyle;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.Viewer;
import org.kalypso.ogc.MapModell;
import org.kalypso.ogc.event.ModellEvent;
import org.kalypso.ogc.event.ModellEventListener;
import org.kalypso.ogc.gml.IKalypsoTheme;

/**
 * Dieser TreeContentProvider akzeptiert nur MapModell'e als Input.
 * 
 * @author bce
 */
public class MapModellTreeContentProvider implements ITreeContentProvider, ModellEventListener
{
  protected Viewer m_viewer = null;

  /**
   * @see org.eclipse.jface.viewers.ITreeContentProvider#getChildren(java.lang.Object)
   */
  public Object[] getChildren( final Object parentElement )
  {
    if( parentElement instanceof IKalypsoTheme )
    {
      final IKalypsoTheme theme = (IKalypsoTheme)parentElement;
      final UserStyle[] styles = theme.getStyles();
      final ThemeStyleTreeObject[] result = new ThemeStyleTreeObject[styles.length];
      for( int i = 0; i < styles.length; i++ )
        result[i] = new ThemeStyleTreeObject( theme, styles[i] );

      return result;//( theme ).getStyles();
    }
    return null;
  }

  /**
   * @see org.eclipse.jface.viewers.ITreeContentProvider#getParent(java.lang.Object)
   */
  public Object getParent( final Object element )
  {
    return null;
  }

  /**
   * @see org.eclipse.jface.viewers.ITreeContentProvider#hasChildren(java.lang.Object)
   */
  public boolean hasChildren( final Object element )
  {
    return ( element instanceof IKalypsoTheme );
  }

  /**
   * @see org.eclipse.jface.viewers.IStructuredContentProvider#getElements(java.lang.Object)
   */
  public Object[] getElements( final Object inputElement )
  {
    final MapModell mm = (MapModell)inputElement;
    return mm.getAllThemes();
  }

  /**
   * @see org.eclipse.jface.viewers.IContentProvider#dispose()
   */
  public void dispose()
  {
    if( m_viewer != null )
    {
      final MapModell input = (MapModell)m_viewer.getInput();
      if( input != null )
        input.removeModellListener( this );
    }
  }

  /**
   * @see org.eclipse.jface.viewers.IContentProvider#inputChanged(org.eclipse.jface.viewers.Viewer,
   *      java.lang.Object, java.lang.Object)
   */
  public void inputChanged( final Viewer viewer, final Object oldInput, final Object newInput )
  {
    if( oldInput != null )
      ( (MapModell)oldInput ).removeModellListener( this );

    m_viewer = viewer;

    if( newInput != null )
      ( (MapModell)newInput ).addModellListener( this );
  }

  /**
   * @see org.kalypso.ogc.event.ModellEventListener#onModellChange(org.kalypso.ogc.event.ModellEvent)
   */
  public void onModellChange( final ModellEvent modellEvent )
  {
    if( m_viewer != null )
    {
      m_viewer.getControl().getDisplay().asyncExec( new Runnable()
      {

        public void run()
        {
          if( !m_viewer.getControl().isDisposed() )
            m_viewer.refresh();
        }
      } );
    }
  }
}