package org.kalypso.ogc.sensor.filter.filters.valuecomp;

import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.util.parser.ParserException;

/**
 * CompSmaller
 * 
 * @author schlienger
 */
public class CompSmaller extends AbstractValueComp
{
  private final Object m_value;
  private boolean m_modeInclusive;
  
  /**
   * @param axes
   * @param axisType
   * @param value
   * @param modeInclusive
   * @throws ParserException
   */
  public CompSmaller( final IAxis[] axes, final String axisType, final String value, final boolean modeInclusive ) throws ParserException
  {
    super( axes, axisType );
    
    m_value = m_parser.parse( value );
    m_modeInclusive = modeInclusive;
  }

  /**
   * @see org.kalypso.ogc.sensor.filter.filters.valuecomp.AbstractValueComp#internalValidates(java.lang.Object)
   */
  public boolean internalValidates( final Object element ) throws ParserException
  {
    if( m_modeInclusive )
      return m_parser.compare( element, m_value ) <= 0;
    
    return m_parser.compare( element, m_value ) < 0;
  }
}
