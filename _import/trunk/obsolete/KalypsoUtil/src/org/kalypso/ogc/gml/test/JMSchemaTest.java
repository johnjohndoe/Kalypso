package org.kalypso.ogc.gml.test;

import java.io.BufferedReader;
import java.io.InputStreamReader;

import org.deegree.model.feature.FeatureType;
import org.deegree.model.feature.FeatureTypeProperty;
import org.kalypso.ogc.gml.EnumerationFeatureTypeProperty;
import org.kalypso.ogc.gml.JMSchema;
import org.kalypso.util.xml.XMLTools;

import junit.framework.TestCase;

/**
 * @author doemming
 */
public class JMSchemaTest extends TestCase
{

  public void testJMSchema()
  {
    try
    {
      JMSchema jmSchema =
        new JMSchema(
          XMLTools.getAsDOM(
        //      getClass().getResourceAsStream("point.xsd")));
              
              getClass().getResourceAsStream("kalypsoNA.xsd")));
   //           getClass().getResourceAsStream("KalypsoSpree.xsd")));
              
              FeatureType[] ftps = jmSchema.getFeatureTypes();
      String result = toString(0,ftps);
      System.out.println(result);
      BufferedReader reader=new BufferedReader(new InputStreamReader(getClass().getResourceAsStream("kalypsoNA.txt")));
      String goal="";
      String buffer;
      while((buffer=reader.readLine())!=null)
       goal=goal+buffer;
      assertEquals(
        goal.replaceAll("\\s", ""),
        result.replaceAll("\\s", ""));
    }
    catch (Exception e)
    {
      e.printStackTrace();
      fail(e.getMessage());
    }
  }
  private String toString(int indent, FeatureType[] fts)
  {
    String result = "";

    for (int i = 0; i < fts.length; i++)
    {
      result = result + toString(indent,fts[i]);
    }

    return result;
  }

  private String toString(int indent, FeatureType ft)
  {
    String result =
      getIndent(indent)+"FeatureType: "
        + ft.getName()
        + toString(indent + 1, ft.getProperties());
    FeatureType[] childs = ft.getChildren();

    if (childs.length > 0)
      result = result + getIndent(indent+1)+"childs:" + toString(indent + 1, childs);

    return result;
  }

  private String toString(int indent, FeatureTypeProperty[] ftps)
  {
    String result = "";

    for (int i = 0; i < ftps.length; i++)
    {
      result = result + toString(indent + 1, ftps[i]);
    }

    return result;
  }

  private String toString(int indent, FeatureTypeProperty ftp)
  {
    String result =
      (
    getIndent(indent)
          + ftp.getName()
          + "                                                    ")
            .substring(
        0,
        indent+40)
        + ftp.getType();

    if (ftp instanceof EnumerationFeatureTypeProperty)
      result =
        result
          + toString(indent + 1, (EnumerationFeatureTypeProperty) ftp);

    return result;
  }

  private String toString(int indent, EnumerationFeatureTypeProperty eftp)
  {
    String result = "  Enumeration [";
    Object[] enum = eftp.getEnumeration();

    for (int i = 0; i < enum.length; i++)
      result = result + " " + enum[i].toString();

    return result + "]";
  }
  private String getIndent(int indent)
  {
    String result = "";
    String space = "  ";
    while (indent-- > 0)
      result = result + space;
    return "\n"+result;

  }
}
