package org.kalypso.editor.tableeditor.layerTable;

import java.util.Properties;

import org.deegree.model.feature.Feature;
import org.deegree.model.feature.FeatureProperty;
import org.deegree.model.feature.FeatureType;
import org.deegree.model.feature.FeatureTypeProperty;
import org.deegree_impl.model.feature.FeatureFactory;
import org.eclipse.jface.viewers.ICellModifier;
import org.eclipse.swt.widgets.TableItem;
import org.kalypso.plugin.KalypsoGisPlugin;

/**
 * Property ist die Nummer der Spalte = Index in den FeatureTypeProperties
 * 
 * @author belger
 */
public class LayerTableCellModifier implements ICellModifier
{
  private final FeatureType m_type;
  
  private final Properties m_props = KalypsoGisPlugin.getDefault().getFeatureTypeCellEditorProperties();
  
  public LayerTableCellModifier( final FeatureType type )
  {
    m_type = type;
  }
  
  /**
   * @see org.eclipse.jface.viewers.ICellModifier#canModify(java.lang.Object, java.lang.String)
   */
  public boolean canModify( final Object element, final String property )
  {
    final FeatureTypeProperty ftp = getFeatureTypeProperty( property );
    return m_props.containsKey( ftp.getType() );
  }

  private FeatureTypeProperty getFeatureTypeProperty( final String property )
  {
    final int index = Integer.parseInt(property);
    
    return m_type.getProperties()[index];
  }

  /**
   * @see org.eclipse.jface.viewers.ICellModifier#getValue(java.lang.Object, java.lang.String)
   */
  public Object getValue( final Object element, final String property )
  {
    final int index = Integer.parseInt(property);
    return ((Feature)element).getProperty( index );
  }

  /**
   * @see org.eclipse.jface.viewers.ICellModifier#modify(java.lang.Object, java.lang.String, java.lang.Object)
   */
  public void modify( final Object element, final String property, final Object value )
  {
    final FeatureProperty fp = FeatureFactory.createFeatureProperty( getFeatureTypeProperty(property).getName(), value );
    
    // TODO: post Command and send event to Layer -> TODO: react to event in LayerTable or wherever
    ((Feature)((TableItem)element).getData()).setProperty( fp );
  }

}
