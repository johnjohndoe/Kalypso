package org.kalypso.util.parser.impl.test;

import junit.framework.TestCase;

import org.kalypso.util.parser.ParserException;
import org.kalypso.util.parser.impl.DoubleParser;

/**
 * @author belger
 */
public class DoubleParserTest extends TestCase
{

  public void testParse()
  {
    try
    {                     
      test(".101",0.101);
      test("10.10",10.1);
      test("10",10);
    }
    catch( Exception e )
    {
      e.printStackTrace();
    }
  }

  private void test(String value,double result) throws ParserException
  {
    DoubleParser parser = new DoubleParser();
    Double object = (Double)parser.parse(value);
    
    System.out.println(value+" -> "+object.getClass().getName()+": "+object.toString());
    String string = parser.toStringInternal(object);
    System.out.println("= "+string+"\n\n");
    assertTrue(object.doubleValue()==result);   
  }
}
