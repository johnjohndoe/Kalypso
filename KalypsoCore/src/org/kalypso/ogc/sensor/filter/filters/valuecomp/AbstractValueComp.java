package org.kalypso.ogc.sensor.filter.filters.valuecomp;

import java.lang.reflect.UndeclaredThrowableException;

import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.ObservationUtilities;
import org.kalypso.util.parser.IParser;
import org.kalypso.util.parser.ParserException;
import org.kalypso.util.parser.ParserFactory;


/**
 * AbstractValueComp
 * 
 * @author schlienger
 */
public abstract class AbstractValueComp implements IValueComp
{
  protected final String m_axisType;
  protected final IAxis m_axis;
  protected final IParser m_parser;

  /**
   * Constructor
   * 
   * @param axes
   * @param axisType
   */
  public AbstractValueComp( final IAxis[] axes, final String axisType )
  {
    m_axisType = axisType;
    
    m_axis = ObservationUtilities.findAxisByType( axes, axisType );

    m_parser = ParserFactory.createParser( m_axis.getDataClass() );
  }
  
  /**
   * @see org.kalypso.ogc.sensor.filter.filters.valuecomp.IValueComp#validates(java.lang.Object)
   */
  public boolean validates( Object element )
  {
    try
    {
      return internalValidates( element );
    }
    catch( ParserException e )
    {
      e.printStackTrace();
      
      throw new UndeclaredThrowableException( e );
    }
  }

  /**
   * @param element
   * @return true if comparison validates
   * @throws ParserException
   * 
   * @see org.kalypso.ogc.sensor.filter.filters.valuecomp.IValueComp#validates(java.lang.Object)
   */
  protected abstract boolean internalValidates( Object element ) throws ParserException;
}