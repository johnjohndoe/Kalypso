package org.kalypso.ogc.sensor.tableview;

import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IObservation;

/**
 * A default implementation that works directly with one IObsevartion. That is, it delivers
 * the columns of the observation, without using any 'real' template.
 * <p>
 * The ITableViewColumn that can be build by this class are constructed using the first
 * axis of the observation as sharedAxis.
 * <p>
 * Example: if the observation has 3 axis (A1, A2, A3), the default template creates 2 columns 
 * for A2 and A3, using A1 as a shared axis.
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
      
      // one column less than the number of axis
      m_cols = new ITableViewColumn[ axes.length - 1];
      
      for( int i = 0; i < m_cols.length; i++ )
        m_cols[i] = new DefaultTableViewColumn( axes[i + 1].getLabel() + " - " + axes[i + 1].getUnit(), m_obs, axes[0], axes[i + 1] );
    }
    
    return m_cols;
  }
}
