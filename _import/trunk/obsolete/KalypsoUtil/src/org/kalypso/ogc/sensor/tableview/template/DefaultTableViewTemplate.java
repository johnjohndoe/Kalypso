package org.kalypso.ogc.sensor.tableview.template;

import java.util.ArrayList;

import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.status.KalypsoStatusUtils;
import org.kalypso.ogc.sensor.tableview.ITableViewColumn;
import org.kalypso.ogc.sensor.tableview.ITableViewTemplate;
import org.kalypso.ogc.sensor.template.AbstractTemplateAdapter;

/**
 * A default implementation that works directly with one IObsevartion. That is,
 * it delivers the columns of the observation, without using any 'real'
 * template.
 * <p>
 * The ITableViewColumn that can be build by this class are constructed using
 * the first axis of the observation as sharedAxis.
 * <p>
 * Example: if the observation has 3 axis (A1, A2, A3), the default template
 * creates 2 columns for A2 and A3, using A1 as a shared axis.
 * 
 * @author schlienger
 */
public class DefaultTableViewTemplate extends AbstractTemplateAdapter implements ITableViewTemplate
{
  private final IObservation m_obs;

  private final ArrayList m_cols = new ArrayList();

  public DefaultTableViewTemplate( final IObservation obs )
  {
    m_obs = obs;

    loadColumns();
  }

  private void loadColumns()
  {
    IAxis[] axes = m_obs.getAxisList();

    // one column less than the number of axis
    for( int i = 1; i < axes.length; i++ )
    {
      // ignore axis if it is a kalypso status axis
      if( !KalypsoStatusUtils.isStatusAxis( axes[i] ) )
        m_cols.add( new DefaultTableViewColumn( axes[i].getLabel() + " - "
            + axes[i].getUnit(), m_obs, axes[0], axes[i] ) );
    }

    fireTemplateLoaded();
  }

  /**
   * @see org.kalypso.ogc.sensor.tableview.ITableViewTemplate#getRules()
   */
  public Rules getRules()
  {
    return new Rules();
  }

  /**
   * @see org.kalypso.ogc.sensor.tableview.ITableViewTemplate#getColumns()
   */
  public ITableViewColumn[] getColumns()
  {
    return (ITableViewColumn[])m_cols.toArray( new ITableViewColumn[0] );
  }
}