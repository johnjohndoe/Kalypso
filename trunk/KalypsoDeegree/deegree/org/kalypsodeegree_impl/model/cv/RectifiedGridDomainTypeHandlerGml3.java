package org.kalypsodeegree_impl.model.cv;

import java.net.URL;
import java.util.Vector;

import javax.xml.namespace.QName;

import org.kalypso.commons.xml.NS;
import org.kalypso.contribs.java.net.IUrlResolver;
import org.kalypso.gmlschema.types.AbstractOldFormatMarshallingTypeHandlerAdapter;
import org.kalypso.gmlschema.types.TypeRegistryException;
import org.kalypsodeegree.model.coverage.GridRange;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree_impl.model.geometry.AdapterBindingToValue;
import org.kalypsodeegree_impl.model.geometry.AdapterBindingToValue_GML2x;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;


/**
 * Modified Nadja Peiler's RectifiedGridDomainTypeHandler for the GML3 Namespace.
 * <p>
 * We are using this true handler instead of using java-binding, because the java binding classes are not correct.
 * The binding classes do not support getting/setting of the grid-domain.
 * </p>
 * TypeHandler for GridDomain of RectifiedGridCoverage
 * 
 * @author Dejan Antanaskovic
 */
public class RectifiedGridDomainTypeHandlerGml3 extends AbstractOldFormatMarshallingTypeHandlerAdapter
{
  public static final QName TYPENAME = new QName( NS.GML3, "RectifiedGridDomainType" );
  
  public RectifiedGridDomainTypeHandlerGml3(){}

  /**
   * @see org.kalypsodeegree_impl.extension.IMarshallingTypeHandler#getClassName()
   */
  public Class getValueClass( )
  {
    return RectifiedGridDomain.class;
  }

  /**
   * @see org.kalypsodeegree_impl.extension.IMarshallingTypeHandler#getTypeName()
   */
  public QName getTypeName( )
  {
    return TYPENAME;
  }

  /**
   * @see org.kalypsodeegree_impl.extension.IMarshallingTypeHandler#marshall(java.lang.Object, org.w3c.dom.Node,
   *      java.net.URL)
   */
  @Override
  public void marshall( Object object, Node node, URL context ) throws TypeRegistryException
  {
    final RectifiedGridDomain gridDomain = (RectifiedGridDomain) object;
    Document ownerDocument = node.getOwnerDocument();

    Element e_rectifiedGrid = ownerDocument.createElementNS( NS.GML3, "gml:RectifiedGrid" );
    e_rectifiedGrid.setAttribute( "dimension", "2" );

    Element e_limits = ownerDocument.createElementNS( NS.GML3, "gml:limits" );
    Element e_gridEnvelope = ownerDocument.createElementNS( NS.GML3, "gml:GridEnvelope" );
    GridRange gridRange = gridDomain.getGridRange();
    double[] lows = gridRange.getLow();
    String stringLows = new String( (new Double( lows[0] )).intValue() + " " + (new Double( lows[1] )).intValue() );
    double[] highs = gridRange.getHigh();
    String stringHighs = new String( (new Double( highs[0] )).intValue() + " " + (new Double( highs[1] )).intValue() );
    Element e_low = ownerDocument.createElementNS( NS.GML3, "gml:low" );
    e_low.appendChild( ownerDocument.createTextNode( stringLows ) );
    Element e_high = ownerDocument.createElementNS( NS.GML3, "gml:high" );
    e_high.appendChild( ownerDocument.createTextNode( stringHighs ) );
    e_gridEnvelope.appendChild( e_low );
    e_gridEnvelope.appendChild( e_high );
    e_limits.appendChild( e_gridEnvelope );
    e_rectifiedGrid.appendChild( e_limits );

    Element e_origin = ownerDocument.createElementNS( NS.GML3, "gml:origin" );
    GM_Point origin;
    try
    {
      origin = gridDomain.getOrigin( null );
    }
    catch( Exception e1 )
    {
      throw new TypeRegistryException( "origin error", e1 );
    }
    Element e_point = ownerDocument.createElementNS( NS.GML3, "gml:Point" );
    try
    {
      e_point.setAttribute( "srsName", origin.getCoordinateSystem().getName() );
    }
    catch( Exception e )
    {
      System.out.println( e );
    }
    Element e_coordinates = ownerDocument.createElementNS( NS.GML3, "gml:pos" );
    //e_coordinates.setAttribute( "cs", "," );
    //e_coordinates.setAttribute( "decimal", "." );
    //e_coordinates.setAttribute( "ts", " " );
    String stringOrigin = new String( origin.getX() + " " + origin.getY() );
    e_coordinates.appendChild( ownerDocument.createTextNode( stringOrigin ) );
    e_point.appendChild( e_coordinates );
    e_origin.appendChild( e_point );
    e_rectifiedGrid.appendChild( e_origin );

    double[] offset = gridDomain.getOffset();
    Element e_offsetVector1 = ownerDocument.createElementNS( NS.GML3, "gml:offsetVector" );
    String offsetVector1 = new String( "0.0" + " " + offset[1] );
    e_offsetVector1.appendChild( ownerDocument.createTextNode( offsetVector1 ) );
    e_rectifiedGrid.appendChild( e_offsetVector1 );
    Element e_offsetVector2 = ownerDocument.createElementNS( NS.GML3, "gml:offsetVector" );
    String offsetVector2 = new String( offset[0] + " " + "0.0" );
    e_offsetVector2.appendChild( ownerDocument.createTextNode( offsetVector2 ) );
    e_rectifiedGrid.appendChild( e_offsetVector2 );

    ((Element) node).appendChild( e_rectifiedGrid );
  }

  /**
   * @see org.kalypsodeegree_impl.extension.IMarshallingTypeHandler#unmarshall(org.w3c.dom.Node, java.net.URL,
   *      org.kalypso.contribs.java.net.IUrlResolver)
   */
  @Override
  public Object unmarshall( Node node, URL context, IUrlResolver urlResolver )
  {
    Node node_rg = ((Element) node).getElementsByTagNameNS( NS.GML3, "RectifiedGrid" ).item( 0 );

    Node node_limits = ((Element) node_rg).getElementsByTagNameNS( NS.GML3, "limits" ).item( 0 );
    Node node_gridEnv = ((Element) node_limits).getElementsByTagNameNS( NS.GML3, "GridEnvelope" ).item( 0 );
    Node n_low = ((Element) node_gridEnv).getElementsByTagNameNS( NS.GML3, "low" ).item( 0 );
    String[] lows = n_low.getFirstChild().getNodeValue().trim().split( " " );
    double[] low = new double[lows.length];
    for( int i = 0; i < low.length; i++ )
      low[i] = Double.parseDouble( lows[i] );

    System.out.println( low[0] + " " + low[1] );

    Node n_high = ((Element) node_gridEnv).getElementsByTagNameNS( NS.GML3, "high" ).item( 0 );
    String[] highs = n_high.getFirstChild().getNodeValue().trim().split( " " );
    double[] high = new double[highs.length];
    for( int i = 0; i < high.length; i++ )
      high[i] = Double.parseDouble( highs[i] );

    System.out.println( high[0] + " " + high[1] );

    GridRange gridRange = new GridRange_Impl( low, high );

    Node n_origin = ((Element) node_rg).getElementsByTagNameNS( NS.GML3, "origin" ).item( 0 );
    Node n_point = ((Element) n_origin).getElementsByTagNameNS( NS.GML3, "Point" ).item( 0 );
    try
    {
      AdapterBindingToValue adapter = new AdapterBindingToValue_GML2x();
      GM_Point origin = (GM_Point) adapter.wrapFromNode( n_point );
      // GMLGeometry gmlGeom = GMLFactory.createGMLGeometry( (Element) n_point );
      // GM_Point origin = (GM_Point) GMLAdapter.wrap( gmlGeom );
      System.out.println( "OriginX: " + origin.getX() + ", OriginY: " + origin.getY() );
      System.out.println( "CoordinateSystem: " + origin.getCoordinateSystem().getName() );

      NodeList nl_offSetVector = ((Element) node_rg).getElementsByTagNameNS( NS.GML3, "offsetVector" );
      Vector<double[]> offSetVectors = new Vector<double[]>();
      for( int i = 0; i < nl_offSetVector.getLength(); i++ )
      {
        Node n_offsetVector = nl_offSetVector.item( i );
        String[] vectorCoos = n_offsetVector.getFirstChild().getNodeValue().trim().split( " " );
        double[] vectorCoo = new double[vectorCoos.length];
        for( int n = 0; n < vectorCoo.length; n++ )
        {
          vectorCoo[n] = Double.parseDouble( vectorCoos[n] );
          // System.out.println(n+": "+vectorCoo[n]);
        }
        offSetVectors.addElement( vectorCoo );
      }
      double[] offset = new double[2];
      double[] p1 = offSetVectors.get( 0 );
      offset[1] = p1[1];
      double[] p2 = offSetVectors.get( 1 );
      offset[0] = p2[0];
      System.out.println( "OffSetX: " + p2[0] + ", OffSetY: " + p1[1] );

      RectifiedGridDomain gridDomain = new RectifiedGridDomain( origin, offset, gridRange );
      return gridDomain;
    }
    catch( Exception e )
    {
      System.out.println( e );
      return null;
    }
  }

  /**
   * @see org.kalypsodeegree_impl.extension.IMarshallingTypeHandler#getShortname()
   */
  public String getShortname( )
  {
    return "rectifiedGridDomain";
  }

  /**
   * @throws CloneNotSupportedException
   * @see org.kalypsodeegree_impl.extension.IMarshallingTypeHandler#cloneObject(java.lang.Object)
   */
  public Object cloneObject( Object objectToClone, final String gmlVersion ) throws CloneNotSupportedException
  {
    throw new CloneNotSupportedException( "Clone is not supported!" );
  }

  /**
   * @see org.kalypsodeegree_impl.extension.IMarshallingTypeHandler#parseType(java.lang.String)
   */
  public Object parseType( final String text )
  {
    throw new UnsupportedOperationException();
  }

  /**
   * @see org.kalypso.gmlschema.types.ITypeHandler#isGeometry()
   */
  public boolean isGeometry( )
  {
    // TODO check this
    return false;
  }

}