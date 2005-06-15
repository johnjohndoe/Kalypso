package org.kalypsodeegree_impl.extension;

import java.net.URL;

import org.kalypso.java.net.IUrlResolver;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree.ogcbasic.CommonNamespaces;
import org.kalypsodeegree_impl.gml.schema.XMLHelper;
import org.kalypsodeegree_impl.model.geometry.GM_Envelope_Impl;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.Text;

/**
 * @author doemming
 */
public class GMLBoundingShapeTypeHandler implements ITypeHandler
{

  /**
   * @see org.kalypsodeegree_impl.extension.ITypeHandler#getClassName()
   */
  public String getClassName()
  {
    return GM_Envelope.class.getName().toString();
  }

  private static String m_nameSpaceURI=XMLHelper.GMLSCHEMA_NS+":"+"BoundingShapeType";
  /**
   * @see org.kalypsodeegree_impl.extension.ITypeHandler#getTypeName()
   */
  public String getTypeName()
  {
    return m_nameSpaceURI;
  }

  /**
   * @see org.kalypsodeegree_impl.extension.ITypeHandler#marshall(java.lang.Object, org.w3c.dom.Node, java.net.URL)
   */
  public void marshall( final Object object, final Node node ,URL context )
  {
    final GM_Envelope envelope = (GM_Envelope)object;

    // TODO: bloed: warum muss man hier manuell die ganzen prefixes setzen
    // sollte das nicht der XML Schreiber erledigen??
    final Document doc = node.getOwnerDocument();
    final Element box = doc.createElementNS( CommonNamespaces.GMLNS, "Box" );
    box.setPrefix( "gml" );
    final Element coord1 = doc.createElementNS( CommonNamespaces.GMLNS, "coord" );
    coord1.setPrefix( "gml" );
    final Element coord2 = doc.createElementNS( CommonNamespaces.GMLNS, "coord" );
    coord2.setPrefix( "gml" );
    final Element X1 = doc.createElementNS( CommonNamespaces.GMLNS, "X" );
    X1.setPrefix( "gml" );
    final Element Y1 = doc.createElementNS( CommonNamespaces.GMLNS, "Y" );
    Y1.setPrefix( "gml" );
    final Element X2 = doc.createElementNS( CommonNamespaces.GMLNS, "X" );
    X2.setPrefix( "gml" );
    final Element Y2 = doc.createElementNS( CommonNamespaces.GMLNS, "Y" );
    Y2.setPrefix( "gml" );
    
    double x1 = 0.0;
    double y1 = 0.0;
    double x2 = 0.0;
    double y2 = 0.0;
    
    if( envelope != null )
    {
      final GM_Position min = envelope.getMin();
      final GM_Position max = envelope.getMax();
      
      x1 = min.getX();
      y1 = min.getY();
      
      x2 = max.getX();
      y2 = max.getY();
    }

    final Text x1text = doc.createTextNode( "" + x1 );
    final Text y1text = doc.createTextNode( "" + y1 );
    final Text x2text = doc.createTextNode( "" + x2 );
    final Text y2text = doc.createTextNode( "" + y2 );

    node.appendChild( box );
    box.appendChild( coord1 );
    box.appendChild( coord2 );
    
    coord1.appendChild( X1 );
    coord1.appendChild( Y1 );

    coord2.appendChild( X2 );
    coord2.appendChild( Y2 );
    
    X1.appendChild( x1text );
    Y1.appendChild( y1text );
    X2.appendChild( x2text );
    Y2.appendChild( y2text );
    
    /*
     *       Coordinates can be included in a single string, but there is no 
        facility for validating string content. The value of the 'cs' attribute 
        is the separator for coordinate values, and the value of the 'ts' 
        attribute gives the tuple separator (a single space by default); the 
        default values may be changed to reflect local usage.
      </documentation>
    </annotation>
    <simpleContent>
      <extension base="string">
        <attribute name="decimal" type="string" use="optional" default="."/>
        <attribute name="cs" type="string" use="optional" default=","/>
        <attribute name="ts" type="string" use="optional" default="&#x20;"/>
     */
  }

  /**
   * @see org.kalypsodeegree_impl.extension.ITypeHandler#unmarshall(org.w3c.dom.Node, java.net.URL)
   */
  public Object unmarshall( Node node ,URL context, IUrlResolver urlResolver )
  {
  	return new GM_Envelope_Impl(  );
  }

  /**
   * @see org.kalypsodeegree_impl.extension.ITypeHandler#getShortname()
   */
  public String getShortname()
  {
    return "Bounding Shape";
  }

}
