package org.kalypso.ogc.sensor.diagview.impl;

import java.util.Properties;

import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.ObservationUtilities;
import org.kalypso.ogc.sensor.diagview.IDiagramAxis;
import org.kalypso.ogc.sensor.status.KalypsoStatusUtils;
import org.kalypso.ogc.sensor.timeseries.TimeserieConstants;

/**
 * A default <code>IDiagramTemplate</code> that works directly with an <code>IObservation</code>.
 * 
 * @author schlienger
 */
public class ObservationDiagramTemplate extends DefaultDiagramTemplate
{
  private static final String ID_DATE_AXIS = "d";
  private static final String ID_VALUE_AXIS = "v";
  
  private final DiagramAxis m_dateAxis;

  private final DiagramAxis m_valueAxis;

  public ObservationDiagramTemplate()
  {
    super( "", "", true );
    
    m_dateAxis = new DiagramAxis( ID_DATE_AXIS, "xs:date", TimeserieConstants.TYPE_DATE, "", IDiagramAxis.DIRECTION_HORIZONTAL,
        IDiagramAxis.POSITION_BOTTOM, false );
    m_valueAxis = new DiagramAxis( ID_VALUE_AXIS, "xs:double", "", "", IDiagramAxis.DIRECTION_VERTICAL,
        IDiagramAxis.POSITION_LEFT, false );

    addAxis( m_dateAxis );
    addAxis( m_valueAxis );
  }

  /**
   * Sets the observation used by this template.
   * 
   * @param obs
   */
  public void setObservation( final IObservation obs  )
  {
    removeAllCurves();
    setTitle( obs.getName() );
    
    final IAxis[] valueAxis = ObservationUtilities.findAxisByClass( obs.getAxisList(), Number.class );
    final IAxis[] keyAxes = ObservationUtilities.findAxisByKey( obs.getAxisList() );

    if( keyAxes.length == 0 )
      return;
    
    final IAxis dateAxis = keyAxes[0];
    
    for( int i = 0; i < valueAxis.length; i++ )
    {
      if( !KalypsoStatusUtils.isStatusAxis( valueAxis[i] ) )
      {
        final Properties mappings = new Properties();
        mappings.setProperty( dateAxis.getName(), "d" );
        mappings.setProperty( valueAxis[i].getName(), "v" );

        final DiagramCurve curve = new DiagramCurve( valueAxis[i].getName(), obs, mappings, this );

        addCurve( curve );
      }
    }
  }
}