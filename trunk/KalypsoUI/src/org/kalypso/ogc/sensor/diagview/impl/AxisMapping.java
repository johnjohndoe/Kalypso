package org.kalypso.ogc.sensor.diagview.impl;

import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.diagview.IAxisMapping;
import org.kalypso.ogc.sensor.diagview.IDiagramAxis;

/**
 * Default implementation of <code>IAxisMapping</code>.
 * 
 * @author schlienger
 *
 */
public class AxisMapping implements IAxisMapping
{
  private final IAxis m_oAxis;
  private final IDiagramAxis m_dAxis;

  public AxisMapping( final IAxis oAxis, final IDiagramAxis dAxis )
  {
    m_oAxis = oAxis;
    m_dAxis = dAxis;
  }

  /**
   * @see org.kalypso.ogc.sensor.diagview.IAxisMapping#getObservationAxis()
   */
  public IAxis getObservationAxis()
  {
    return m_oAxis;
  }

  /**
   * @see org.kalypso.ogc.sensor.diagview.IAxisMapping#getDiagramAxis()
   */
  public IDiagramAxis getDiagramAxis()
  {
    return m_dAxis;
  }
  
  /**
   * @see java.lang.Object#toString()
   */
  public String toString( )
  {
    return m_oAxis + " - " + m_dAxis;
  }
  
//  /**
//   * Convenience method that builds a properties object and sets its property-entries
//   * so that they are adequate to the given mappings.
//   * 
//   * @param mappings
//   * @return properties
//   */
//  public static Properties saveAsProperties( IAxisMapping[] mappings )
//  {
//    Properties props = new Properties();
//    
//    for( int i = 0; i < mappings.length; i++ )
//      props.setProperty( mappings[i].getObservationAxis().getName(), mappings[i].getDiagramAxis().getIdentifier() );
//    
//    return props;
//  }
}
