package org.kalypso.editor.tableeditor.layerTable;

import org.deegree.model.feature.FeatureType;
import org.deegree.model.feature.FeatureTypeProperty;
import org.eclipse.jface.viewers.ICellModifier;
import org.eclipse.swt.widgets.TableItem;
import org.kalypso.ogc.command.ModifyFeatureCommand;
import org.kalypso.ogc.gml.KalypsoFeatureLayer;
import org.kalypso.ogc.sort.DisplayContext;
import org.kalypso.util.command.ICommandManager;

/**
 * Property ist der Name der FeatureTypeProperty der entsprechenden Spalte
 * 
 * @author belger
 */
public class LayerTableCellModifier implements ICellModifier
{
  private final FeatureType m_type;
  
  private final LayerTableModel m_modell;

  private final ICommandManager m_commandManager;
  
  public LayerTableCellModifier( final ICommandManager commandManager, final LayerTableModel modell, final FeatureType type )
  {
    m_modell = modell;
    m_commandManager = commandManager;
    m_type = type;
  }
  
  /**
   * @see org.eclipse.jface.viewers.ICellModifier#canModify(java.lang.Object, java.lang.String)
   */
  public boolean canModify( final Object element, final String property )
  {
    final FeatureTypeProperty ftp = getFeatureTypeProperty( property );
    return m_modell.isEditable( ftp );
  }

  private FeatureTypeProperty getFeatureTypeProperty( final String property )
  {
    return m_type.getProperty( property );
  }

  /**
   * @see org.eclipse.jface.viewers.ICellModifier#getValue(java.lang.Object, java.lang.String)
   */
  public Object getValue( final Object element, final String property )
  {
    return ((DisplayContext)element).getFeature().getProperty( property );
  }

  /**
   * @see org.eclipse.jface.viewers.ICellModifier#modify(java.lang.Object, java.lang.String, java.lang.Object)
   */
  public void modify( final Object element, final String property, final Object value )
  {
    final DisplayContext dc = (DisplayContext)((TableItem)element).getData();
    final KalypsoFeatureLayer layer = m_modell.getLayer();
    m_commandManager.postCommand( new ModifyFeatureCommand( layer, dc, property, value ), null );
   }

}
