package org.kalypso.ogc.sensor.zml.values;

import java.util.List;
import java.util.StringTokenizer;
import java.util.Vector;

import org.kalypso.util.parser.IParser;
import org.kalypso.util.parser.ParserException;
import org.kalypso.zml.AxisType.ValueArrayType;

/**
 * @author schlienger
 */
public class ZmlArrayValues implements IZmlValues
{
  private final List m_values;

  public ZmlArrayValues( final ValueArrayType va, final IParser parser ) throws ParserException
  {
    final StringTokenizer stok = new StringTokenizer( va.getValue(), va.getSeparator() );

    m_values = new Vector( stok.countTokens() );

    while( stok.hasMoreElements() )
    {
      final String token = stok.nextToken();
      final Object obj = parser.parse( token );
      m_values.add( obj );
    }
  }

  /**
   * @see org.kalypso.ogc.sensor.zml.values.IZmlValues#getElement(int)
   */
  public Object getElement( int index )
  {
    return m_values.get( index );
  }

  /**
   * @see org.kalypso.ogc.sensor.zml.values.IZmlValues#setElement(int,
   *      java.lang.Object)
   */
  public void setElement( int index, Object element )
  {
    m_values.set( index, element );
  }

  /**
   * @see org.kalypso.ogc.sensor.zml.values.IZmlValues#getCount()
   */
  public int getCount()
  {
    return m_values.size();
  }

  /**
   * @see org.kalypso.ogc.sensor.zml.values.IZmlValues#indexOf(java.lang.Object)
   */
  public int indexOf( final Object obj )
  {
    return m_values.indexOf( obj );
  }
}