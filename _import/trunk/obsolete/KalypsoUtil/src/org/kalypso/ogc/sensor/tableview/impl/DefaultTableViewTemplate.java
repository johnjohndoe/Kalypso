package org.kalypso.ogc.sensor.tableview.impl;

import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.status.KalypsoStatusUtils;
import org.kalypso.ogc.sensor.tableview.ITableViewColumn;
import org.kalypso.ogc.sensor.tableview.ITableViewTemplate;
import org.kalypso.ogc.sensor.tableview.rules.RenderingRule;
import org.kalypso.ogc.sensor.template.ITemplateEventListener;
import org.kalypso.ogc.sensor.template.TemplateEvent;

/**
 * @author schlienger
 */
public class DefaultTableViewTemplate implements ITableViewTemplate
{
  private final TableViewTemplate m_template;

  public DefaultTableViewTemplate( final IObservation obs, final boolean editableColumns )
  {
    m_template = new TableViewTemplate();

    IAxis[] axes = obs.getAxisList();

    // one column less than the number of axis
    for( int i = 1; i < axes.length; i++ )
    {
      // ignore axis if it is a kalypso status axis
      if( !KalypsoStatusUtils.isStatusAxis( axes[i] ) )
      {
        TableViewColumn col = new TableViewColumn( axes[i].getLabel() + " - " + axes[i].getUnit(),
            obs, editableColumns, 50, axes[0].getLabel(), axes[i].getLabel() );

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

  public boolean equals( Object obj )
  {
    return m_template.equals( obj );
  }

  public void fireTemplateChanged( TemplateEvent evt )
  {
    m_template.fireTemplateChanged( evt );
  }

  public ITableViewColumn[] getColumns()
  {
    return m_template.getColumns();
  }

  public int hashCode()
  {
    return m_template.hashCode();
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