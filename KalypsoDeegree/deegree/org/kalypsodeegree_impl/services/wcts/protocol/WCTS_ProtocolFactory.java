/*----------------    FILE HEADER  ------------------------------------------

 This file is part of deegree.
 Copyright (C) 2001 by:
 EXSE, Department of Geography, University of Bonn
 http://www.giub.uni-bonn.de/exse/
 lat/lon Fitzke/Fretter/Poth GbR
 http://www.lat-lon.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 Andreas Poth
 lat/lon Fitzke/Fretter/Poth GbR
 Meckenheimer Allee 176
 53115 Bonn
 Germany
 E-Mail: poth@lat-lon.de

 Jens Fitzke
 Department of Geography
 University of Bonn
 Meckenheimer Allee 166
 53115 Bonn
 Germany
 E-Mail: jens.fitzke@uni-bonn.de

 
 ---------------------------------------------------------------------------*/
package org.deegree_impl.services.wcts.protocol;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.Reader;
import java.io.StringReader;
import java.util.HashMap;

import org.deegree.gml.GMLException;
import org.deegree.gml.GMLGeometry;
import org.deegree.services.OGCWebServiceException;
import org.deegree.services.OGCWebServiceRequest;
import org.deegree.services.wcts.protocol.DescribeTransformationRequest;
import org.deegree.services.wcts.protocol.DescribeTransformationResponse;
import org.deegree.services.wcts.protocol.GetCapabilitiesRequest;
import org.deegree.services.wcts.protocol.GetCapabilitiesResponse;
import org.deegree.services.wcts.protocol.IsTransformableRequest;
import org.deegree.services.wcts.protocol.IsTransformableResponse;
import org.deegree.services.wcts.protocol.TransformRequest;
import org.deegree.services.wcts.protocol.TransformResponse;
import org.deegree.services.wcts.protocol.TransformationSequence;
import org.deegree.xml.Marshallable;
import org.deegree.xml.XMLTools;
import org.deegree_impl.gml.GMLFactory;
import org.deegree_impl.model.cs.Adapters;
import org.deegree_impl.model.cs.ConvenienceCSFactory;
import org.deegree_impl.model.cs.CoordinateSystem;
import org.deegree_impl.tools.Debug;
import org.opengis.cs.CS_CoordinateSystem;
import org.opengis.ct.CT_MathTransform;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;
import org.xml.sax.SAXException;

/**
 * Factory class for creating WCTS_Protocol classes from a XML document that's
 * conform to the WCTS-specification.
 * 
 * <p>
 * ----------------------------------------------------------------------
 * </p>
 * 
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 * @version 2002-07-22
 */
public final class WCTS_ProtocolFactory
{
  /**
   * factory method for creating a Request where the type of the Request is
   * chosen and initialized.
   * 
   * @param Reader
   *          the post-request
   */
  public static synchronized OGCWebServiceRequest createRequest( String id, Reader reader )
  {
    OGCWebServiceRequest req = null;

    try
    {
      String postrequest = null;

      StringBuffer sb = new StringBuffer( 2000 );
      BufferedReader br = new BufferedReader( reader );

      while( ( postrequest = br.readLine() ) != null )
      {
        sb.append( postrequest.trim() );
      }

      br.close();

      postrequest = sb.toString();

      reader = new StringReader( postrequest );

      //String auslesen zwischen <Request> und </Request>
      if( postrequest.indexOf( "<GetCapabilities" ) >= 0 )
      {
        req = createGetCapabilitiesRequest( id, null );
      }
      else if( postrequest.indexOf( "<Transformable" ) >= 0 )
      {
        req = createIsTransformableRequest( id, null, reader );
      }
      else if( postrequest.indexOf( "<DescribeTransformation" ) >= 0 )
      {
        req = createDescribeTransformationRequest( id, null, reader );
      }
      else if( postrequest.indexOf( "<Transform" ) >= 0 )
      {
        req = createTransformRequest( id, null, reader );
      }

      reader.close();
    }
    catch( Exception ex )
    {
      System.out.println( "WCTS_ProtocolFactory: " + ex );
    }

    return req;
  }

  /**
   * factory method for creating a <tt>Capabilities</tt> Request
   */
  public static synchronized GetCapabilitiesRequest createGetCapabilitiesRequest( String id,
      HashMap vendorSpecificParameter ) throws Exception
  {
    Debug.debugMethodBegin();

    GetCapabilitiesRequest createGetCapabilitiesRequest = createGetCapabilitiesRequest( id,
        vendorSpecificParameter );

    Debug.debugMethodEnd();

    return createGetCapabilitiesRequest;
  }

  /**
   * @see createGetCapabilitiesResponse
   */
  public static GetCapabilitiesResponse createGetCapabilitiesResponse(
      OGCWebServiceRequest request, OGCWebServiceException exception, String capabilities )
      throws Exception
  {
    Debug.debugMethodBegin();

    Document doc = null;

    if( exception != null )
    {
      StringReader reader = new StringReader( ( (Marshallable)exception ).exportAsXML() );

      try
      {
        doc = XMLTools.parse( reader );
      }
      catch( Exception e )
      {
        System.out.println( e );
      }
    }

    GetCapabilitiesResponse getcapabilitiesresponse = new GetCapabilitiesResponse_Impl( request,
        doc, capabilities );

    Debug.debugMethodEnd();

    return getcapabilitiesresponse;
  }

  /**
   * factory method for creating a <tt>Transformable</tt> object from a file
   * that contains a WCTS-specification conform XML transformable-request
   * document.
   */
  public static synchronized IsTransformableRequest createIsTransformableRequest( String id,
      HashMap vendorSpecificParameter, Reader reader ) throws Exception
  {
    Debug.debugMethodBegin( "WCTS_ProtocolFactory", "createIsTransformableRequest" );

    Document doc = XMLTools.parse( reader );
    IsTransformableRequest istransformableRequest = createIsTransformableRequest( id,
        vendorSpecificParameter, doc );

    Debug.debugMethodEnd();

    return istransformableRequest;
  }

  /**
   * factory method for creating a <tt>Transformable</tt> object from a
   * WCTS-specification conform XML transformable-request document
   */
  public static synchronized IsTransformableRequest createIsTransformableRequest( String id,
      HashMap vendorSpecificParameter, Document doc ) throws Exception
  {
    Debug.debugMethodBegin( "WCTS_ProtocolFactory", "createIsTransformableRequest" );

    Element root = doc.getDocumentElement();

    //get version-attribute
    String version = "1.0.0";

    // get SourceCRS section
    Element element = XMLTools.getNamedChild( root, "SourceCRS" );

    if( element == null )
    {
      throw new Exception( "no source CRS defined" );
    }

    CS_CoordinateSystem sourceCRS = getSourceCRS( element );

    // get DestinationCRS section
    element = XMLTools.getNamedChild( root, "DestinationCRS" );

    if( element == null )
    {
      throw new Exception( "no destination CRS defined" );
    }

    CS_CoordinateSystem destinationCRS = getDestinationCRS( element );

    // create Transformable object
    IsTransformableRequest istransformableRequest = new IsTransformableRequest_Impl( version, id,
        vendorSpecificParameter, sourceCRS, destinationCRS );

    Debug.debugMethodEnd();

    return istransformableRequest;
  }

  /**
   * gets the SourceCRS out of the Transformable element
   */
  public static CS_CoordinateSystem getSourceCRS( Element element ) throws java.rmi.RemoteException
  {
    Debug.debugMethodBegin( "WCTS_ProtocolFactory", "getSourceCRS" );

    Element temp = XMLTools.getNamedChild( element, "CoordinateReferenceSystem" );
    temp = XMLTools.getNamedChild( temp, "Identifier" );

    Element codecs = XMLTools.getNamedChild( temp, "code" );

    String code = codecs.getFirstChild().getNodeValue();

    codecs = XMLTools.getNamedChild( temp, "codeSpace" );

    String codespace = codecs.getFirstChild().getNodeValue();

    String sourceCRS = codespace + ":" + code;

    CS_CoordinateSystem sourceCRS_CS = String2CS_CoordinateSystem( sourceCRS );

    Debug.debugMethodEnd();

    return sourceCRS_CS;
  }

  /**
   * gets the DestinationCRS out of the Transformable element
   */
  public static CS_CoordinateSystem getDestinationCRS( Element element )
      throws java.rmi.RemoteException
  {
    Debug.debugMethodBegin( "WCTS_ProtocolFactory", "getDestinationCRS" );

    Element temp = XMLTools.getNamedChild( element, "CoordinateReferenceSystem" );
    temp = XMLTools.getNamedChild( temp, "Identifier" );

    Element codecs = XMLTools.getNamedChild( temp, "code" );

    String code = codecs.getFirstChild().getNodeValue();

    codecs = XMLTools.getNamedChild( temp, "codeSpace" );

    String codespace = codecs.getFirstChild().getNodeValue();

    String destinationCRS = codespace + ":" + code;

    CS_CoordinateSystem destinationCRS_CS = String2CS_CoordinateSystem( destinationCRS );

    Debug.debugMethodEnd();

    return destinationCRS_CS;
  }

  /**
   * creates an CS_CoordinateSystem from a String.
   */
  private static CS_CoordinateSystem String2CS_CoordinateSystem( String string )
      throws java.rmi.RemoteException
  {
    Debug.debugMethodBegin( "WCTS_ProtocolFactory", "String2CS_CoordinateSystem" );

    ConvenienceCSFactory fac = ConvenienceCSFactory.getInstance();
    CoordinateSystem cs = fac.getCSByName( string );
    Adapters adapter = Adapters.getDefault();
    CS_CoordinateSystem cs_ = adapter.export( cs );
    cs = adapter.wrap( cs_ );

    Debug.debugMethodEnd();
    return cs_;
  }

  /**
   * factory method for creating a <tt>Transformable</tt> object from a file
   * that contains a WCTS-specification conform XML transformable-response
   * document.
   */
  public static synchronized IsTransformableResponse createIsTransformableResponse(
      OGCWebServiceRequest request, OGCWebServiceException exception, boolean istransformable )
      throws Exception
  {
    Debug.debugMethodBegin( "WCTS_ProtocolFactory", "createIsTransformableResponse" );

    Document doc = null;

    if( exception != null )
    {
      StringReader reader = new StringReader( ( (Marshallable)exception ).exportAsXML() );

      try
      {
        doc = XMLTools.parse( reader );
      }
      catch( Exception e )
      {
        System.out.println( e );
      }
    }

    IsTransformableResponse istransformableResponse = new IsTransformableResponse_Impl( request,
        doc, istransformable );

    Debug.debugMethodEnd();

    return istransformableResponse;
  }

  /**
   * factory method for creating a <tt>Transform</tt> object from a file that
   * contains a WCTS-specification conform XML transform-request document.
   */
  public static synchronized TransformRequest createTransformRequest( String id,
      HashMap vendorSpecificParameter, Reader reader ) throws IOException, SAXException, Exception
  {
    Debug.debugMethodBegin( "WCTS_ProtocolFactory", "createTransformRequest" );

    Document doc = XMLTools.parse( reader );
    TransformRequest transformrequest = createTransformRequest( id, vendorSpecificParameter, doc );

    Debug.debugMethodEnd();

    return transformrequest;
  }

  /**
   * factory method for creating a <tt>Transform</tt> object from a
   * WCTS-specification conform XML transform-request document
   */
  public static synchronized TransformRequest createTransformRequest( String id,
      HashMap vendorSpecificParameter, Document doc ) throws Exception
  {
    Debug.debugMethodBegin( "WCTS_ProtocolFactory", "createTransformRequest(doc)" );

    Element root = doc.getDocumentElement();

    // get Version Attribute
    String version = "1.0.0";

    // gets the InputFormat element
    Element element = XMLTools.getNamedChild( root, "InputFormat" );
    String inputformat = null;

    if( element != null )
    {
      inputformat = getInputFormat( element );
    }

    // gets the OutputFormat element
    element = XMLTools.getNamedChild( root, "OutputFormat" );

    String outputformat = null;

    if( element != null )
    {
      outputformat = getOutputFormat( element );
    }

    // gets the SourceCRS element
    element = XMLTools.getNamedChild( root, "SourceCRS" );

    CS_CoordinateSystem sourceCRS = null;

    if( element != null )
    {
      sourceCRS = getSourceCRS( element );
    }

    // gets the DestinationCRS element
    element = XMLTools.getNamedChild( root, "DestinationCRS" );

    CS_CoordinateSystem destinationCRS = getDestinationCRS( element );

    /*
     * // gets the TransformationSequence element element =
     * XMLTools.getNamedChild(root, "TransformationSequence");
     * TransformationSequence[] transformationSequence = null; if (element !=
     * null) { transformationSequence = getTransformationSequence(element); }
     */

    // gets the Data element (1 to unbounded)
    NodeList nl = root.getElementsByTagName( "Data" );
    GMLGeometry[] data = getData( nl );

    TransformRequest transformrequest = new TransformRequest_Impl( version, id,
        vendorSpecificParameter, inputformat, outputformat, sourceCRS, destinationCRS, null, data );

    Debug.debugMethodEnd();

    return transformrequest;
  }

  /**
   * gets the InputFormat element
   */
  public static String getInputFormat( Element element )
  {
    Debug.debugMethodBegin( "WCTS_ProtocolFactory", "getInputFormat" );

    String name = XMLTools.getAttrValue( element, "name" );
    Debug.debugMethodEnd();
    return name;
  }

  /**
   * gets the OutputFormat element
   */
  public static String getOutputFormat( Element element )
  {
    Debug.debugMethodBegin( "WCTS_ProtocolFactory", "getOutputFormat" );

    String name = XMLTools.getAttrValue( element, "name" );
    Debug.debugMethodEnd();
    return name;
  }

  /**
   * gets TransformationSequence element
   */
  public static TransformationSequence[] getTransformationSequence( Element element )
  {
    Debug.debugMethodBegin( "WCTS_ProtocolFactory", "getTransformationSequence" );

    // searching for Identifier
    NodeList identifier = element.getElementsByTagName( "Identifier" );
    int identifiergetlength = identifier.getLength();

    Element codeandcodespace = null;
    String codevalue = null;
    String codespacevalue = null;

    TransformationSequence transformationsequence = null;
    TransformationSequence[] tf_array = new TransformationSequence[identifiergetlength];

    for( int i = 0; i < identifiergetlength; i++ )
    {
      // searches for code
      codeandcodespace = XMLTools.getNamedChild( identifier.item( i ), "code" );
      codevalue = codeandcodespace.getFirstChild().getNodeValue();

      // searches for codeSpace
      codeandcodespace = XMLTools.getNamedChild( identifier.item( i ), "codeSpace" );
      codespacevalue = codeandcodespace.getFirstChild().getNodeValue();

      // creates a new TransformationSequence-object with the two
      // strings code and codespace
      transformationsequence = new TransformationSequence_Impl( codevalue, codespacevalue );

      // gives this object on the position 0 of the nodelist to the array
      // of TransformationSequences.
      tf_array[i] = transformationsequence;
    }

    Debug.debugMethodEnd();
    return tf_array;
  }

  /**
   * gets the Data element
   */
  public static GMLGeometry[] getData( NodeList nl ) throws GMLException
  {
    Debug.debugMethodBegin( "WCTS_ProtocolFactory", "getData" );

    int nodelistlength = nl.getLength();
    Element element = null;
    GMLGeometry gmlgeo = null;
    GMLGeometry[] gmlgeo_array = new GMLGeometry[nodelistlength];

    for( int i = 0; i < nodelistlength; i++ )
    {
      // gets the first (item(0)) GMLGeometry
      element = XMLTools.getFirstElement( nl.item( i ) );

      // creates a GMLGeometry
      gmlgeo = GMLFactory.createGMLGeometry( element );
      gmlgeo_array[i] = gmlgeo;
    }

    Debug.debugMethodEnd();
    return gmlgeo_array;
  }

  /**
   * factory method for creating a <tt>Transform</tt> object from a file that
   * contains a WCTS-specification conform XML transform-response document.
   */
  public static synchronized TransformResponse createTransformResponse(
      OGCWebServiceRequest request, OGCWebServiceException exception, GMLGeometry[] data )
  {
    Debug.debugMethodBegin( "WCTS_ProtocolFactory", "createTransformResponse(doc)" );

    Document doc = null;

    if( exception != null )
    {
      StringReader reader = new StringReader( ( (Marshallable)exception ).exportAsXML() );

      try
      {
        doc = XMLTools.parse( reader );
      }
      catch( Exception e )
      {
        System.out.println( e );
      }
    }

    TransformResponse transformresponse = new TransformResponse_Impl( request, doc, data );

    Debug.debugMethodEnd();
    return transformresponse;
  }

  /**
   * factory method to create DescribeTransformation object
   */
  public static synchronized DescribeTransformationRequest createDescribeTransformationRequest(
      String id, HashMap vendorSpecificParameter, Reader reader ) throws Exception
  {
    Debug.debugMethodBegin( "WCTS_ProtocolFactory", "createDescribeTransformationRequest" );

    Document doc = XMLTools.parse( reader );
    DescribeTransformationRequest describeTransformationRequest = createDescribeTransformationRequest(
        id, vendorSpecificParameter, doc );

    Debug.debugMethodEnd();
    return describeTransformationRequest;
  }

  /**
   * factory method for creating a <tt>DescribeTransformationRequest</tt>
   * object from a WCTS-specification conform XML describetransformation-request
   * document
   */
  public static synchronized DescribeTransformationRequest createDescribeTransformationRequest(
      String id, HashMap vendorSpecificParameter, Document doc ) throws Exception
  {
    Debug.debugMethodBegin( "WCTS_ProtocolFactory", "createDescribeTransformationRequest" );

    Element ele = doc.getDocumentElement();
    String version = "1.0.0";

    Element element = XMLTools.getNamedChild( ele, "Format" );

    String format = getFormat( element );

    element = XMLTools.getNamedChild( ele, "SourceCRS" );

    CS_CoordinateSystem sourceCS = getSourceCRS( element );

    element = XMLTools.getNamedChild( ele, "DestinationCRS" );

    CS_CoordinateSystem destinationCRS = getDestinationCRS( element );

    DescribeTransformationRequest createDescribeTransformationRequest = new DescribeTransformationRequest_Impl(
        version, id, vendorSpecificParameter, format, sourceCS, destinationCRS );

    Debug.debugMethodEnd();
    return createDescribeTransformationRequest;
  }

  /**
   * gets the format element
   */
  public static String getFormat( Element element )
  {
    Debug.debugMethodBegin( "WCTS_ProtocolFactory", "getFormat" );

    String getFormat = XMLTools.getAttrValue( element, "Format" );

    Debug.debugMethodEnd();
    return getFormat;
  }

  /**
   * DescribeTransformationResponse
   */
  public static synchronized DescribeTransformationResponse createDescribeTransformationResponse(
      OGCWebServiceRequest request, OGCWebServiceException exception,
      CT_MathTransform[] parameterizedTransformation ) throws Exception
  {
    Debug.debugMethodBegin( "WCTS_ProtocolFactory", "createDescribeTransformationResponse" );

    Document doc = null;

    if( exception != null )
    {
      StringReader reader = new StringReader( ( (Marshallable)exception ).exportAsXML() );

      try
      {
        doc = XMLTools.parse( reader );
      }
      catch( Exception e )
      {
        System.out.println( e );
      }
    }

    int numberOfTransformations = 0;

    if( parameterizedTransformation != null )
    {
      numberOfTransformations = parameterizedTransformation.length;
    }

    DescribeTransformationResponse createDescribeTransformationResponse = new DescribeTransformationResponse_Impl(
        request, doc, parameterizedTransformation, numberOfTransformations );

    Debug.debugMethodEnd();
    return createDescribeTransformationResponse;
  }

  /**
   * replaces occurences of a string fragment within a string by a new string.
   * 
   * @param target
   *          is the original string
   * @param from
   *          is the string to be replaced
   * @param to
   *          is the string which will used to replace
   * @param all
   *          if it's true all occurences of the string to be replaced will be
   *          replaced. else only the first occurence will be replaced.
   */
  public String replace( String target, String from, String to, boolean all )
  {
    int start = target.indexOf( from );

    if( start == -1 )
    {
      return target;
    }

    int lf = from.length();
    char[] targetChars = target.toCharArray();
    StringBuffer buffer = new StringBuffer( target.length() );
    int copyFrom = 0;

    while( start != -1 )
    {
      buffer.append( targetChars, copyFrom, start - copyFrom );
      buffer.append( to );
      copyFrom = start + lf;
      start = target.indexOf( from, copyFrom );

      if( !all )
      {
        start = -1;
      }
    }

    buffer.append( targetChars, copyFrom, targetChars.length - copyFrom );

    return buffer.toString();
  }
}