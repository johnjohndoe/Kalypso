package org.kalypso.editor.tableeditor.layerTable;

import org.deegree.model.feature.FeatureTypeProperty;
import org.eclipse.jface.viewers.ILabelProviderListener;
import org.eclipse.jface.viewers.ITableLabelProvider;
import org.eclipse.swt.graphics.Image;
import org.kalypso.ogc.gml.KalypsoFeature;

/**
 * @author bce
 */
public class LayerTableLabelProvider implements ITableLabelProvider
{
  private LayerTable m_layerTable;

  public LayerTableLabelProvider( final LayerTable layerTable )
  {
    m_layerTable = layerTable;
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
    final FeatureTypeProperty ftp = m_layerTable.getFeatureTypePropertyFromIndex(columnIndex);
    if( ftp == null )
      return "";
    
    final String name = ftp.getName();
    final Object property = ((KalypsoFeature)element).getProperty( name );
    return property == null ? "" : property.toString();
  }

  /**
   * @see org.eclipse.jface.viewers.IBaseLabelProvider#addListener(org.eclipse.jface.viewers.ILabelProviderListener)
   */
  public void addListener( final ILabelProviderListener listener )
  {
  //  
  }

  /**
   * @see org.eclipse.jface.viewers.IBaseLabelProvider#dispose()
   */
  public void dispose()
  {
  //  
  }

  /**
   * @see org.eclipse.jface.viewers.IBaseLabelProvider#isLabelProperty(java.lang.Object, java.lang.String)
   */
  public boolean isLabelProperty( Object element, String property )
  {
    return false;
  }

  /**
   * @see org.eclipse.jface.viewers.IBaseLabelProvider#removeListener(org.eclipse.jface.viewers.ILabelProviderListener)
   */
  public void removeListener( ILabelProviderListener listener )
  {
  //  
  }

}
