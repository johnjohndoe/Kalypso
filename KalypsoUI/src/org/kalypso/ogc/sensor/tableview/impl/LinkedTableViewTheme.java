package org.kalypso.ogc.sensor.tableview.impl;

import java.util.Iterator;

import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.ObservationUtilities;
import org.kalypso.template.obstableview.TypeColumn;
import org.kalypso.template.obstableview.TypeObservation;

/**
 * LinkedTableViewTheme
 * 
 * @author schlienger
 */
public class LinkedTableViewTheme extends DefaultTableViewTheme
{
  private TypeObservation m_tobs;

  /**
   * Constructor. Columns are created once the observation is set.
   * 
   * @param tobs
   */
  public LinkedTableViewTheme( final TypeObservation tobs )
  {
    m_tobs = tobs;
  }

  /**
   * @see org.kalypso.ogc.sensor.tableview.impl.DefaultTableViewTheme#setObservation(org.kalypso.ogc.sensor.IObservation)
   */
  public void setObservation( IObservation obs )
  {
    super.setObservation( obs );
    
    final IAxis keyAxis = ObservationUtilities.findAxisByKey( obs.getAxisList() )[0];
    
    for( Iterator itCols = m_tobs.getColumn().iterator(); itCols.hasNext(); )
    {
      final TypeColumn tcol = (TypeColumn) itCols.next();

      final IAxis valueAxis = ObservationUtilities.findAxisByName( obs.getAxisList(), tcol.getAxis() );
      
      DefaultTableViewColumn column = new DefaultTableViewColumn( tcol.getAxis(), tcol.isEditable(), tcol.getWidth(), keyAxis, valueAxis, this );
      
      addColumn( column );
    }
  }
}
