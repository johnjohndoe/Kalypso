package org.kalypso.ogc.sensor.filter.filters.valuecomp;

import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.util.parser.ParserException;

/**
 * CompBetween
 * 
 * @author schlienger
 */
public class CompBetween extends AbstractValueComp
{
  private Object m_valueFrom;
  private Object m_valueTo;
  private boolean m_modeFromIncl;
  private boolean m_modeToIncl;

  /**
   * @param axes
   * @param axisType
   * @param valueFrom
   * @param modeFromInclusive
   * @param valueTo
   * @param modeToInclusive
   * @throws ParserException
   */
  public CompBetween( final IAxis[] axes, final String axisType, final String valueFrom, final boolean modeFromInclusive, final String valueTo, final boolean modeToInclusive ) throws ParserException
  {
    super( axes, axisType );
    
    m_valueFrom = m_parser.parse( valueFrom );
    m_valueTo = m_parser.parse( valueTo );
    m_modeFromIncl = modeFromInclusive;
    m_modeToIncl = modeToInclusive;
  }

  /**
   * @see org.kalypso.ogc.sensor.filter.filters.valuecomp.AbstractValueComp#internalValidates(java.lang.Object)
   */
  public boolean internalValidates( final Object element ) throws ParserException
  {
    if( m_parser.compare( element, m_valueFrom ) < 0 )
      return false;
    
    if( m_parser.compare( element, m_valueTo ) > 0 )
      return false;

    if( !m_modeFromIncl && m_parser.compare( element, m_valueFrom ) <= 0 )
      return false;

    if( !m_modeToIncl && m_parser.compare( element, m_valueTo ) >= 0 )
      return false;
    
    return true;
  }
}
