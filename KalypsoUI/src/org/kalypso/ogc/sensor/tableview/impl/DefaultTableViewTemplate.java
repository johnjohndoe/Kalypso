package org.kalypso.ogc.sensor.tableview.impl;

import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.status.KalypsoStatusUtils;
import org.kalypso.ogc.sensor.tableview.ITableViewColumn;
import org.kalypso.ogc.sensor.tableview.ITableViewTemplate;
import org.kalypso.ogc.sensor.tableview.rules.RenderingRule;
import org.kalypso.ogc.sensor.template.ITemplateEventListener;
import org.kalypso.ogc.sensor.template.TemplateEvent;
import org.kalypso.util.runtime.IVariableArguments;

/**
 * @author schlienger
 */
public class DefaultTableViewTemplate implements ITableViewTemplate
{
  private final TableViewTemplate m_template;

  public DefaultTableViewTemplate(  )
  {
    m_template = new TableViewTemplate();
  }

  public void setObservation( final IObservation obs, final boolean editableColumns, final IVariableArguments args )
  {
    m_template.removeAllColumns();
    
    final IAxis[] axes = obs.getAxisList();

    // one column less than the number of axis
    for( int i = 1; i < axes.length; i++ )
    {
      // ignore axis if it is a kalypso status axis
      if( !KalypsoStatusUtils.isStatusAxis( axes[i] ) )
      {
        final TableViewColumn col = new TableViewColumn( axes[i].getLabel() + " - " + axes[i].getUnit(),
            obs, editableColumns, 50, axes[0].getLabel(), axes[i].getLabel(), args );

        m_template.addColumn( col );
      }
    }
  }
  
  public void addColumn( ITableViewColumn column )
  {
    m_template.addColumn( column );
  }

  public void addTemplateEventListener( ITemplateEventListener l )
  {
    m_template.addTemplateEventListener( l );
  }

  public void fireTemplateChanged( TemplateEvent evt )
  {
    m_template.fireTemplateChanged( evt );
  }

  public ITableViewColumn[] getColumns()
  {
    return m_template.getColumns();
  }

  public void removeColumn( ITableViewColumn column )
  {
    m_template.removeColumn( column );
  }

  public void removeTemplateEventListener( ITemplateEventListener l )
  {
    m_template.removeTemplateEventListener( l );
  }

  public String toString()
  {
    return m_template.toString();
  }

  public void addRule( RenderingRule rule )
  {
    m_template.addRule( rule );
  }

  public RenderingRule[] findRules( int mask )
  {
    return m_template.findRules( mask );
  }

  public void removeRule( RenderingRule rule )
  {
    m_template.removeRule( rule );
  }

  public void removeAllColumns()
  {
    m_template.removeAllColumns();
  }
}