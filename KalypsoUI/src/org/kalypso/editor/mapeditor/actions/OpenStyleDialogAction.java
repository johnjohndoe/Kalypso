package org.kalypso.editor.mapeditor.actions;

import java.awt.Color;

import org.deegree.graphics.sld.LineSymbolizer;
import org.deegree.graphics.sld.Stroke;
import org.deegree.graphics.sld.Symbolizer;
import org.deegree.model.feature.FeatureType;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredViewer;
import org.kalypso.eclipse.jface.action.FullAction;
import org.kalypso.editor.mapeditor.ThemeStyleTreeObject;
import org.kalypso.ogc.event.ModellEvent;
import org.kalypso.ogc.gml.KalypsoFeatureLayer;
import org.kalypso.ogc.gml.KalypsoUserStyle;
import org.kalypso.util.list.IListManipulator;

/**
 * @author belger
 */
public class OpenStyleDialogAction extends FullAction implements ISelectionChangedListener
{
  private final StructuredViewer m_viewer;

  private IListManipulator m_listManipulator;

  public OpenStyleDialogAction( final String text, final ImageDescriptor image,
      final String tooltipText, final StructuredViewer structuredViewer,
      final IListManipulator listManip )
  {
    super( text, image, tooltipText );

    m_viewer = structuredViewer;
    m_listManipulator = listManip;

    m_viewer.addSelectionChangedListener( this );

    refresh();
  }

  public void dispose()
  {
    m_viewer.removeSelectionChangedListener( this );
  }

  private static boolean test = true;

  /**
   * @see org.eclipse.jface.action.Action#run()
   */
  public void run()
  {
    Object o = ( (IStructuredSelection)m_viewer.getSelection() ).getFirstElement();
    if( o instanceof ThemeStyleTreeObject )
    {
      KalypsoFeatureLayer layer = ( (ThemeStyleTreeObject)o ).getTheme().getLayer();
      FeatureType ft = layer.getFeatureType();
      System.out.println( "FeatureType:" + ft );

      KalypsoUserStyle kalypsoStyle = ( (ThemeStyleTreeObject)o ).getStyle();
      Symbolizer symbolizer = kalypsoStyle.getFeatureTypeStyles()[0].getRules()[0].getSymbolizers()[0];
      if( symbolizer instanceof LineSymbolizer )
      {
        Stroke stroke = ( (LineSymbolizer)symbolizer ).getStroke();
        if( test )
        {
          test = !test;
          stroke.setStroke( Color.RED );
        }
        else
        {
          test = !test;
          stroke.setStroke( Color.GREEN );
        }
        kalypsoStyle.fireModellEvent( new ModellEvent( ModellEvent.STYLE_CHANGE ) );
      }
    }

    // 	((IStructuredSelection)m_viewer.getSelection()).
    //	m_viewer.g getContentProvider().
    //  System.out.println(o.getClass().toString());
    //  		// m_listManipulator.moveElementDown(
    // ((IStructuredSelection)m_viewer.getSelection()).getFirstElement() );
  }

  /**
   * @see org.eclipse.jface.viewers.ISelectionChangedListener#selectionChanged(org.eclipse.jface.viewers.SelectionChangedEvent)
   */
  public void selectionChanged( final SelectionChangedEvent event )
  {
    refresh();
  }

  private void refresh()
  {
    boolean bEnable = false;

    final IStructuredSelection s = (IStructuredSelection)m_viewer.getSelection();
    if( !s.isEmpty() )
    {
      final Object[] elements = ( (IStructuredContentProvider)m_viewer.getContentProvider() )
          .getElements( m_viewer.getInput() );

      bEnable = ( elements[elements.length - 1] != s.getFirstElement() );
    }

    setEnabled( bEnable );
  }
}