package org.kalypso.ogc.gml.outline;

import org.deegree.graphics.sld.Rule;
import org.deegree.graphics.sld.UserStyle;
import org.deegree.model.feature.event.ModellEvent;
import org.deegree.model.feature.event.ModellEventListener;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.Viewer;
import org.kalypso.ogc.gml.IKalypsoLayer;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.KalypsoFeatureLayer;
import org.kalypso.ogc.gml.KalypsoUserStyle;
import org.kalypso.ogc.gml.mapmodel.IMapModell;

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
      if( styles != null )
      {
        final ThemeStyleTreeObject[] result = new ThemeStyleTreeObject[styles.length];
        for( int i = 0; i < styles.length; i++ )
          result[i] = new ThemeStyleTreeObject( theme, styles[i] );
        return result;
      }
    }
    else if( parentElement instanceof ThemeStyleTreeObject )
    {
      ThemeStyleTreeObject obj = (ThemeStyleTreeObject)parentElement;
      IKalypsoLayer layer = obj.getTheme().getLayer();
      if( !( layer instanceof KalypsoFeatureLayer ) )
        return null;
      
      final KalypsoUserStyle userStyle = obj.getStyle();
      final Rule[] rules = userStyle.getFeatureTypeStyles()[0].getRules();
      final RuleTreeObject[] result = new RuleTreeObject[rules.length];
      for( int i = 0; i < result.length; i++ )
        result[i] = new RuleTreeObject( rules[i], userStyle, layer );
      return result;
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
    if( element instanceof ThemeStyleTreeObject )
    {
      ThemeStyleTreeObject obj = (ThemeStyleTreeObject)element;
      UserStyle userStyle = obj.getStyle();
      if( userStyle.getFeatureTypeStyles()[0].getRules().length > 0 )
        return true;
    }
    return ( element instanceof IKalypsoTheme );
  }

  /**
   * @see org.eclipse.jface.viewers.IStructuredContentProvider#getElements(java.lang.Object)
   */
  public Object[] getElements( final Object inputElement )
  {
    final IMapModell mm = (IMapModell)inputElement;
    return mm == null ? null : mm.getAllThemes();
  }

  /**
   * @see org.eclipse.jface.viewers.IContentProvider#dispose()
   */
  public void dispose()
  {
    if( m_viewer != null )
    {
      final IMapModell input = (IMapModell)m_viewer.getInput();
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
      ( (IMapModell)oldInput ).removeModellListener( this );

    m_viewer = viewer;

    if( newInput != null )
      ( (IMapModell)newInput ).addModellListener( this );
  }

  /**
   * @see org.deegree.model.feature.event.ModellEventListener#onModellChange(org.deegree.model.feature.event.ModellEvent)
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