package org.kalypso.util.conversion.units;

import junit.framework.TestCase;

/**
 * UnitConversionTest
 * 
 * @author schlienger
 */
public class UnitConversionTest extends TestCase
{
  public void testKelvinCelsiusConverter()
  {
    final KelvinCelsiusConverter conv = KelvinCelsiusConverter.getInstance();
    
    final double k = 2345;
    final double c = 2071.85;
    
    assertTrue( Double.compare( conv.convert(k), c ) == 0 );
    assertTrue( Double.compare( conv.reverse(c), k ) == 0 );
    assertTrue( Double.compare( conv.convert( conv.reverse(c ) ), c) == 0 );
  }
  
  public void testNoConverter()
  {
    final NoConverter conv = NoConverter.getInstance();
    
    final double value = 235234.2342;
    assertTrue( Double.compare( conv.convert( value ), value ) == 0);
    assertTrue( Double.compare( conv.reverse( value ), value ) == 0);
    assertTrue( Double.compare( conv.convert( conv.reverse(value ) ), value) == 0 );
  }
  
  public void testSIConverter()
  {
    final SIConverter conv1 = new SIConverter( "m", "cm" );
    assertTrue( Double.compare( conv1.convert(1), 100 ) == 0 );
    assertTrue( Double.compare( conv1.reverse(1), .01 ) == 0 );

    final SIConverter conv2 = new SIConverter( "m", "dm" );
    assertTrue( Double.compare( conv2.convert(1), 10 ) == 0 );
    assertTrue( Double.compare( conv2.reverse(1), .1 ) == 0 );
    
    final SIConverter conv3 = new SIConverter( "m", "mm" );
    assertTrue( Double.compare( conv3.convert(1), 1000 ) == 0 );
    assertTrue( Double.compare( conv3.reverse(1), .001 ) == 0 );
  }
}
