package org.kalypso.ogc.gml.table;

import java.util.Map;

import org.eclipse.jface.viewers.ICellModifier;
import org.eclipse.swt.widgets.TableItem;
import org.kalypso.ogc.gml.KalypsoFeature;
import org.kalypso.ogc.gml.IKalypsoLayer;
import org.kalypso.ogc.gml.command.ModifyFeatureCommand;
import org.kalypso.util.command.ICommand;

/**
 * Property ist der Name der FeatureTypeProperty der entsprechenden Spalte
 * 
 * @author belger
 */
public class LayerTableCellModifier implements ICellModifier
{
  private final LayerTableViewer m_viewer;
  
  public LayerTableCellModifier( final LayerTableViewer viewer )
  {
    m_viewer = viewer;
  }
  
  /**
   * @see org.eclipse.jface.viewers.ICellModifier#canModify(java.lang.Object, java.lang.String)
   */
  public boolean canModify( final Object element, final String property )
  {
    return m_viewer.isEditable( property );
  }

  /**
   * Gibt das Feature zurück
   * 
   * @see org.eclipse.jface.viewers.ICellModifier#getValue(java.lang.Object, java.lang.String)
   */
  public Object getValue( final Object element, final String property )
  {
    return element;
  }

  /**
   * value ist eine map: propertyname -> neuer Wert
   * 
   * @see org.eclipse.jface.viewers.ICellModifier#modify(java.lang.Object, java.lang.String, java.lang.Object)
   */
  public void modify( final Object element, final String property, final Object map )
  {
    final TableItem tableItem = (TableItem)element;
    final KalypsoFeature  feature = (KalypsoFeature)tableItem.getData();
    final IKalypsoLayer layer = m_viewer.getTheme().getLayer();
    
    final ICommand command = new ModifyFeatureCommand( layer, feature, (Map)map );
    m_viewer.postCommand( command, null );
   }
}
