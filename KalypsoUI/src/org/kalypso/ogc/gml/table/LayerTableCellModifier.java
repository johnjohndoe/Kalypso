package org.kalypso.ogc.gml.table;

import java.util.Map;

import org.deegree.model.feature.Feature;
import org.eclipse.jface.viewers.ICellModifier;
import org.eclipse.swt.widgets.TableItem;
import org.kalypso.ogc.gml.PoolableKalypsoFeatureTheme;
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
   * Gibt das Feature zur?ck
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
    final Feature  feature = (Feature)tableItem.getData();
    final PoolableKalypsoFeatureTheme theme = m_viewer.getTheme();
    
    final ICommand command = new ModifyFeatureCommand( theme.getFeatureTheme().getWorkspace(), feature, (Map)map );
    m_viewer.postCommand( command, null );
   }
}
