package org.kalypso.ogc.sensor.zml;

import java.text.ParseException;
import java.util.List;
import java.util.StringTokenizer;
import java.util.Vector;

import org.kalypso.java.reflect.ValueObjectFactory;
import org.kalypso.zml.AxisType;

/**
 * @author schlienger
 *
 */
public class ValueArrayLoader implements IZmlValuesLoader
{
  private final AxisType.ValueArrayType m_va;
  
  public ValueArrayLoader( AxisType.ValueArrayType va )
  {
    m_va = va;
  }

  /**
   * @throws ParseException
   * @see org.kalypso.ogc.sensor.zml.IZmlValuesLoader#load(org.kalypso.ogc.sensor.zml.ZmlAxis)
   */
  public List load( ZmlAxis axis ) throws ParseException
  {
    StringTokenizer stok = new StringTokenizer( m_va.getValue(), m_va.getSeparator() );

    Vector v = new Vector( stok.countTokens() );
    
    Class eltClass = axis.getDataClass();
    
    while( stok.hasMoreElements() )
      v.add( ValueObjectFactory.createObjectWithStringValue( eltClass, stok.nextToken() ) );
    
    return v;
  }
}
