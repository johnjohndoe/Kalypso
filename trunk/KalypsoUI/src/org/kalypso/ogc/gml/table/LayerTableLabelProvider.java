package org.kalypso.ogc.gml.table;

import org.deegree.model.feature.Feature;
import org.eclipse.jface.viewers.ILabelProviderListener;
import org.eclipse.jface.viewers.ITableLabelProvider;
import org.eclipse.swt.graphics.Image;
import org.kalypso.ogc.gml.table.celleditors.AbstractFeatureCellEditor;

/**
 * @author Belger
 */
public class LayerTableLabelProvider implements ITableLabelProvider
{
  private final LayerTableViewer m_viewer;

  public LayerTableLabelProvider( final LayerTableViewer layerTable )
  {
    m_viewer = layerTable;
  }
  
  /**
   * @see org.eclipse.jface.viewers.IBaseLabelProvider#dispose()
   */
  public void dispose()
  {
    // nix zu disposen  
  }
  
  /**
   * @see org.eclipse.jface.viewers.ITableLabelProvider#getColumnImage(java.lang.Object, int)
   */
  public Image getColumnImage( Object element, int columnIndex )
  {
    return null;
  }

  /**
   * @see org.eclipse.jface.viewers.ITableLabelProvider#getColumnText(java.lang.Object, int)
   */
  public String getColumnText( final Object element, final int columnIndex )
  {
    // Extrawurscht, wenn Tabelle leer, da trotzdem mit index 0 aufgerufen wird
    if( m_viewer.getColumnCount() == 0 )
      return "";
    
    final Feature feature = (Feature)element;

    final AbstractFeatureCellEditor cellEditor = (AbstractFeatureCellEditor)m_viewer.getCellEditors()[columnIndex];
    if( cellEditor == null )
      return "<kein Renderer vorhanden>";
    
    return cellEditor.renderLabel( feature );
  }

  /**
   * @see org.eclipse.jface.viewers.IBaseLabelProvider#addListener(org.eclipse.jface.viewers.ILabelProviderListener)
   */
  public void addListener( final ILabelProviderListener listener )
  {
    // TODO  Listener informieren, wenn sich der Wert eines Features ge?ndert hat?
  }

  /**
   * @see org.eclipse.jface.viewers.IBaseLabelProvider#isLabelProperty(java.lang.Object, java.lang.String)
   */
  public boolean isLabelProperty( final Object element, final String property )
  {
    return false;
  }

  /**
   * @see org.eclipse.jface.viewers.IBaseLabelProvider#removeListener(org.eclipse.jface.viewers.ILabelProviderListener)
   */
  public void removeListener( final ILabelProviderListener listener )
  {
    //  TODO
  }

}
