package org.kalypso.ogc.sensor.tableview;

import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IObservation;

/**
 * A default implementation that works directly with one IObsevartion. That is, it delivers
 * the columns of the observation, without using any 'real' template.
 * <p>
 * The ITableViewColumn that can be build by this class are constructed using the first
 * axis of the observation as sharedAxis.
 * 
 * @author schlienger
 */
public class DefaultTableViewTemplate implements ITableViewTemplate
{
  private final IObservation m_obs;
  private ITableViewColumn[] m_cols = null;

  public DefaultTableViewTemplate( final IObservation obs )
  {
    m_obs = obs;
  }

  /**
   * @see org.kalypso.ogc.sensor.tableview.ITableViewTemplate#getColumns()
   */
  public ITableViewColumn[] getColumns()
  {
    if( m_cols == null )
    {
      IAxis[] axes = m_obs.getAxisList();
      m_cols = new ITableViewColumn[ axes.length ];
      
      for( int i = 1; i < axes.length; i++ )
        m_cols[i] = new DefaultTableViewColumn( axes[i].getLabel() + " - " + axes[i].getUnit(), m_obs, axes[0], axes[i] );
    }
    
    return m_cols;
  }
}
