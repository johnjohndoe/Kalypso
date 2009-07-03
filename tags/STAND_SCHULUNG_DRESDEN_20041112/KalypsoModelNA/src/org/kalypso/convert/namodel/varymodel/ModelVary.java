package org.kalypso.convert.namodel.varymodel;

import javax.xml.transform.TransformerException;

import org.w3c.dom.Document;
import org.w3c.dom.NodeList;

/**
 * @author doemming
 */
public class ModelVary
{
  public ModelVary()
  {
  // do not instanciate
  }

  public static void initializeModel( Document doc, CalContext[] contexts )
      throws TransformerException
  {
    for( int i = 0; i < contexts.length; i++ )
      initializeModell( doc, contexts[i] );
  }

  public static void initializeModell( Document doc, CalContext calContext )
      throws TransformerException
  {
    String value = Double.toString(calContext.getInitialValue());
    XMLServiceTools.setParameter( calContext.getxPaths(), value, doc );
  }

  public static void varyModell( Document doc, double[] values, CalContext[] contexts )
      throws TransformerException
  {
    for( int i = 0; i < contexts.length; i++ )
      varyModell( doc, values[i], contexts[i] );
  }

  public static void varyModell( Document doc, double value, CalContext calContext )
      throws TransformerException
  {
    String mode = calContext.getMode();
    if( CalContext.MODE_FACTOR.equals( mode ) )
      setParameter_Factor( calContext.getxPaths(), value, doc );
    else if( CalContext.MODE_OFFSET.equals( mode ) )
      XMLServiceTools.setParameter_Offset( calContext.getxPaths(), value, doc );
    else
      // mode direct
      XMLServiceTools
          .setParameter( calContext.getxPaths(), ( new Double( value ) ).toString(), doc );
  }

  public static void setParameter_Factor( String[] querys, double value, Document myDom )
      throws TransformerException
  {
    for( int n = 0; n < querys.length; n++ )
    {
      String query = querys[n];
      System.out.println( "Query: " + query );
      NodeList nl = XMLServiceTools.getXPath( query, myDom );
      for( int i = 0; i < nl.getLength(); i++ )
      {
        String nodeValue = ( nl.item( i ) ).getNodeValue();
        double setValue = value * Double.parseDouble( nodeValue );
        ( nl.item( i ) ).setNodeValue( String.valueOf( setValue ) );
      }
    }
  }
}

