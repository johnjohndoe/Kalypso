package org.kalypso.editor.tableeditor.layerTable;

import org.deegree.model.feature.FeatureType;
import org.deegree.model.feature.FeatureTypeProperty;
import org.eclipse.core.runtime.jobs.ISchedulingRule;
import org.eclipse.jface.viewers.ICellModifier;
import org.eclipse.swt.widgets.TableItem;
import org.kalypso.ogc.command.ModifyFeatureCommand;
import org.kalypso.ogc.gml.KalypsoFeature;
import org.kalypso.ogc.gml.KalypsoFeatureLayer;
import org.kalypso.util.command.CommandJob;
import org.kalypso.util.command.ICommand;
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

  private final ISchedulingRule m_schedulingRule;
  
  public LayerTableCellModifier( final ICommandManager commandManager, final ISchedulingRule schedulingRule, final LayerTableModel modell, final FeatureType type )
  {
    m_modell = modell;
    m_commandManager = commandManager;
    m_type = type;
    m_schedulingRule = schedulingRule;
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
    return ((KalypsoFeature)element).getProperty( property );
  }

  /**
   * @see org.eclipse.jface.viewers.ICellModifier#modify(java.lang.Object, java.lang.String, java.lang.Object)
   */
  public void modify( final Object element, final String property, final Object value )
  {
    final TableItem tableItem = (TableItem)element;
    final KalypsoFeature  feature = (KalypsoFeature)tableItem.getData();
    final KalypsoFeatureLayer layer = m_modell.getLayer();
    
    final ICommand command = new ModifyFeatureCommand( layer, feature, property, value );
    new CommandJob( command, m_commandManager, m_schedulingRule, null, CommandJob.POST );
   }

}
