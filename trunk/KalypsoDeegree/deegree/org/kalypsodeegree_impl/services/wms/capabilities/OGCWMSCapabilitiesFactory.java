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
package org.deegree_impl.services.wms.capabilities;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Reader;
import java.io.StringReader;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Set;
import java.util.TreeSet;

import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import org.deegree.model.geometry.GM_Envelope;
import org.deegree.model.geometry.GM_Position;
import org.deegree.ogcbasic.ContactAddress;
import org.deegree.ogcbasic.ContactInformation;
import org.deegree.ogcbasic.ContactPersonPrimary;
import org.deegree.services.capabilities.CException;
import org.deegree.services.capabilities.DCPType;
import org.deegree.services.capabilities.MetadataURL;
import org.deegree.services.capabilities.Service;
import org.deegree.services.wms.capabilities.Attribution;
import org.deegree.services.wms.capabilities.AuthorityURL;
import org.deegree.services.wms.capabilities.Capability;
import org.deegree.services.wms.capabilities.DataURL;
import org.deegree.services.wms.capabilities.Dimension;
import org.deegree.services.wms.capabilities.Extent;
import org.deegree.services.wms.capabilities.FeatureListURL;
import org.deegree.services.wms.capabilities.Format;
import org.deegree.services.wms.capabilities.Identifier;
import org.deegree.services.wms.capabilities.Layer;
import org.deegree.services.wms.capabilities.LayerBoundingBox;
import org.deegree.services.wms.capabilities.LegendURL;
import org.deegree.services.wms.capabilities.LogoURL;
import org.deegree.services.wms.capabilities.Operation;
import org.deegree.services.wms.capabilities.Request;
import org.deegree.services.wms.capabilities.ScaleHint;
import org.deegree.services.wms.capabilities.Style;
import org.deegree.services.wms.capabilities.StyleSheetURL;
import org.deegree.services.wms.capabilities.StyleURL;
import org.deegree.services.wms.capabilities.UserDefinedSymbolization;
import org.deegree.services.wms.capabilities.WMSCapabilities;
import org.deegree.xml.ElementList;
import org.deegree.xml.XMLParsingException;
import org.deegree.xml.XMLTools;
import org.deegree_impl.model.geometry.GeometryFactory;
import org.deegree_impl.ogcbasic.ContactAddress_Impl;
import org.deegree_impl.ogcbasic.ContactInformation_Impl;
import org.deegree_impl.ogcbasic.ContactPersonPrimary_Impl;
import org.deegree_impl.services.capabilities.CException_Impl;
import org.deegree_impl.services.capabilities.DCPType_Impl;
import org.deegree_impl.services.capabilities.HTTP_Impl;
import org.deegree_impl.services.capabilities.MetadataURL_Impl;
import org.deegree_impl.services.capabilities.Service_Impl;
import org.deegree_impl.tools.StringExtend;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.xml.sax.SAXException;

/**
 * Factory class for creating WMS capability classes from WMS capabilities XML
 * documents that comply to the OGC WMS 1.1.1 specification.
 * <p>
 * 
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 * @author <a href="mailto:schaefer@lat-lon.de">Axel Schaefer </a>
 * @author <a href="mailto:mschneider@lat-lon.de">Markus Schneider </a>
 * @version $Revision$ $Date$
 */
public class OGCWMSCapabilitiesFactory
{
  protected String ogcNS = "http://www.opengis.net/ogc";

  protected String wfsNS = "http://www.opengis.net/wfs";

  protected String wmsNS = null;

  protected String xlnNS = "http://www.w3.org/1999/xlink";

  protected WMSCapabilities capabilities = null;

  protected String version = null;

  // used to check for reuse of layer names
  protected Set collectedLayers = new TreeSet();

  /**
   * Creates a <tt>WMSCapabilities</tt> -instance from the given URL.
   * <p>
   * 
   * @param URL
   *          location of capabilities document
   * @throws XMLParsingException
   *           if a syntactic or semantic error in the XML document is
   *           encountered
   * @return the constructed <tt>WMSCapabilities</tt> -instance
   */
  public WMSCapabilities createCapabilities( URL url ) throws XMLParsingException
  {
    try
    {
      Reader reader = new InputStreamReader( url.openStream() );
      capabilities = createCapabilities( reader );
    }
    catch( IOException e )
    {
      throw new XMLParsingException( "IOException encountered while parsing "
          + "WMSCapabilities-Document: " + e.getMessage(), e );
    }

    return capabilities;
  }

  /**
   * Creates a <tt>WMSCapabilities</tt> -instance from the given Reader.
   * <p>
   * 
   * @param reader
   *          provides the XML document
   * @throws XMLParsingException
   *           if a syntactic or semantic error in the XML document is
   *           encountered
   * @return the constructed <tt>WMSCapabilities</tt> -instance
   */
  public WMSCapabilities createCapabilities( Reader reader ) throws XMLParsingException
  {
    try
    {
      BufferedReader br = new BufferedReader( reader );
      StringBuffer sb = new StringBuffer( 50000 );
      String line = null;
      while( ( line = br.readLine() ) != null )
      {
        sb.append( line );
      }
      br.close();
      reader.close();
      //AVD:
      System.out.println( "WMS_Capabilities:" + sb );
      int k = sb.indexOf( "<WMT_MS_Capabilities" );
      StringReader sr = new StringReader( sb.substring( k, sb.length() ) );
      Document doc = XMLTools.parse( sr );
      capabilities = createCapabilities( doc.getDocumentElement() );
    }
    catch( IOException e )
    {
      throw new XMLParsingException( "IOException encountered while parsing "
          + "WMSCapabilities-Document: " + e.getMessage() );
    }
    catch( SAXException e )
    {
      throw new XMLParsingException( "SAXException encountered while parsing "
          + "WMSCapabilities-Document: " + e.getMessage() );
    }

    return capabilities;
  }

  /**
   * Creates a <tt>WMSCapabilities</tt> -instance according to the contents of
   * the DOM-subtree starting at the given 'WMT_MS_Capabilities'-
   * <tt>Element</tt>.
   * <p>
   * 
   * @param element
   *          the 'WMT_MS_Capabilities'- <tt>Element</tt>
   * @throws XMLParsingException
   *           if a syntactic or semantic error in the DOM-subtree is
   *           encountered
   * @return the constructed <tt>WMSCapabilities</tt> -instance
   */
  protected WMSCapabilities createCapabilities( Element element ) throws XMLParsingException
  {
    // required: 'version'-attribute
    version = XMLTools.getRequiredAttrValue( "version", element );

    // optional: 'updateSequence'-attribute
    String updateSequence = XMLTools.getAttrValue( "updateSequence", element );

    // required: <Service>
    Element el = XMLTools.getRequiredChildByName( "Service", wmsNS, element );
    Service service = createService( el );

    // required: <Capability>
    Capability capability = createCapability( XMLTools.getRequiredChildByName( "Capability", wmsNS,
        element ) );

    return new WMSCapabilities_Impl( version, updateSequence, service, capability, null );
  }

  /**
   * Creates a <tt>Service</tt> -instance according to the contents of the
   * DOM-subtree starting at the given 'Service'- <tt>Element</tt>.
   * <p>
   * 
   * @param element
   *          the 'Service'- <tt>Element</tt>
   * @throws XMLParsingException
   *           if a syntactic or semantic error in the DOM-subtree is
   *           encountered
   * @return the constructed <tt>Service</tt> -instance
   */
  protected Service createService( Element element ) throws XMLParsingException
  {
    // required: <Name>
    String name = XMLTools.getRequiredStringValue( "Name", wmsNS, element );

    // required: <Title>
    String title = XMLTools.getRequiredStringValue( "Title", wmsNS, element );

    // optional: <Abstract>
    String abstractString = XMLTools.getStringValue( "Abstract", wmsNS, element, null );

    // optional: <KeywordList>
    String[] keywords = null;
    Element keywordListElement = XMLTools.getChildByName( "KeywordList", wmsNS, element );

    if( keywordListElement != null )
    {
      ElementList nl = XMLTools.getChildElementsByName( "Keyword", wmsNS, keywordListElement );
      keywords = new String[nl.getLength()];

      for( int i = 0; i < keywords.length; i++ )
      {
        keywords[i] = XMLTools.getStringValue( nl.item( i ) );
      }
    }

    // optional: <OnlineResource>
    URL onlineResource = null;
    if( version.equals( "1.0.0" ) )
    {
      try
      {
        String tmp = XMLTools.getStringValue( "OnlineResource", wmsNS, element, null );
        if( tmp != null )
        {
          onlineResource = new URL( tmp );
        }
      }
      catch( Exception e )
      {
        throw new XMLParsingException( e.toString() );
      }
    }
    else
    {
      onlineResource = createOnlineResource( XMLTools.getChildByName( "OnlineResource", wmsNS,
          element ) );
    }

    // optional: <ContactInformation>
    ContactInformation contactInformation = null;
    Element contactInformationElement = XMLTools.getChildByName( "ContactInformation", wmsNS,
        element );

    if( contactInformationElement != null )
    {
      contactInformation = createContactInformation( contactInformationElement );
    }

    // optional: <Fees>
    String fees = XMLTools.getStringValue( "Fees", wmsNS, element, null );

    // optional: <AccessConstraints>
    String accessConstraints = XMLTools.getStringValue( "AccessConstraints", wmsNS, element, null );

    return new Service_Impl( name, title, abstractString, keywords, onlineResource,
        contactInformation, fees, accessConstraints );
  }

  /**
   * Creates an <tt>URL</tt> -instance according to the contents of the
   * DOM-subtree starting at the given 'OnlineResource'- <tt>Element</tt>.
   * <p>
   * 
   * @param element
   *          the 'OnlineResource'- <tt>Element</tt>
   * @throws XMLParsingException
   *           if a syntactic or semantic error in the DOM-subtree is
   *           encountered
   * @return the constructed <tt>URL</tt> -instance
   */
  protected URL createOnlineResource( Element element ) throws XMLParsingException
  {
    URL url = null;

    if( element != null )
    {
      // required: href-Attribute (in <OnlineResource>)
      String href = XMLTools.getRequiredAttrValue( "href", xlnNS, element );

      try
      {
        url = new URL( href );
      }
      catch( MalformedURLException e )
      {
        throw new XMLParsingException( "Value ('" + href + "') of attribute 'href' of "
            + "element 'OnlineResource' does not denote a valid URL: " + e.getMessage() );
      }
    }

    return url;
  }

  /**
   * Creates a <tt>ContactInformation</tt> -instance according to the contents
   * of the DOM-subtree starting at the given 'ContactInformation'-
   * <tt>Element</tt>.
   * <p>
   * 
   * @param element
   *          the 'ContactInformation'- <tt>Element</tt>
   * @throws XMLParsingException
   *           if a syntactic or semantic error in the DOM-subtree is
   *           encountered
   * @return the constructed <tt>ContactInformation</tt> -instance
   */
  protected ContactInformation createContactInformation( Element element )
      throws XMLParsingException
  {
    // optional: <ContactPersonPrimary>
    ContactPersonPrimary contactPersonPrimary = null;
    Element contactPersonPrimaryElement = XMLTools.getChildByName( "ContactPersonPrimary", wmsNS,
        element );

    if( contactPersonPrimaryElement != null )
    {
      contactPersonPrimary = createContactPersonPrimary( contactPersonPrimaryElement );
    }

    // optional: <ContactPosition>
    String contactPosition = XMLTools.getStringValue( "ContactPosition", wmsNS, element, null );

    // optional: <ContactAddress>
    ContactAddress contactAddress = null;
    Element contactAddressElement = XMLTools.getChildByName( "ContactAddress", wmsNS, element );

    if( contactAddressElement != null )
    {
      contactAddress = createContactAddress( contactAddressElement );
    }

    // optional: <ContactVoiceTelephone>
    String contactVoiceTelephone = XMLTools.getStringValue( "ContactVoiceTelephone", wmsNS,
        element, null );

    // optional: <ContactFacsimileTelephone>
    String contactFacsimileTelephone = XMLTools.getStringValue( "ContactFacsimileTelephone", wmsNS,
        element, null );

    // optional: <ContactElectronicMailAddress>
    String contactElectronicMailAddress = XMLTools.getStringValue( "ContactElectronicMailAddress",
        wmsNS, element, null );

    return new ContactInformation_Impl( contactPosition, contactVoiceTelephone,
        contactFacsimileTelephone, contactElectronicMailAddress, contactPersonPrimary,
        contactAddress );
  }

  /**
   * Creates a <tt>ContactPersonPrimary</tt> -instance according to the
   * contents of the DOM-subtree starting at the given 'ContactPersonPrimary'-
   * <tt>Element</tt>.
   * <p>
   * 
   * @param element
   *          the 'ContactPersonPrimary'- <tt>Element</tt>
   * @throws XMLParsingException
   *           if a syntactic or semantic error in the DOM-subtree is
   *           encountered
   * @return the constructed <tt>ContactPersonPrimary</tt> -instance
   */
  protected ContactPersonPrimary createContactPersonPrimary( Element element )
      throws XMLParsingException
  {
    // required: <ContactPerson>
    String contactPerson = XMLTools.getRequiredStringValue( "ContactPerson", wmsNS, element );

    // required: <ContactOrganization>
    String contactOrganization = XMLTools.getRequiredStringValue( "ContactOrganization", wmsNS,
        element );

    return new ContactPersonPrimary_Impl( contactPerson, contactOrganization );
  }

  /**
   * Creates a <tt>ContactAddress</tt> -instance according to the contents of
   * the DOM-subtree starting at the given 'ContactAddress'- <tt>Element</tt>.
   * <p>
   * 
   * @param element
   *          the 'ContactAddress'- <tt>Element</tt>
   * @throws XMLParsingException
   *           if a syntactic or semantic error in the DOM-subtree is
   *           encountered
   * @return the constructed <tt>ContactAddress</tt> -instance
   */
  protected ContactAddress createContactAddress( Element element ) throws XMLParsingException
  {
    // required: <AddressType>
    String addressType = XMLTools.getRequiredStringValue( "AddressType", wmsNS, element );

    // required: <Address>
    String address = XMLTools.getRequiredStringValue( "Address", wmsNS, element );

    // required: <City>
    String city = XMLTools.getRequiredStringValue( "City", wmsNS, element );

    // required: <StateOrProvince>
    String stateOrProvince = XMLTools.getRequiredStringValue( "StateOrProvince", wmsNS, element );

    // required: <PostCode>
    String postCode = XMLTools.getRequiredStringValue( "PostCode", wmsNS, element );

    // required: <Country>
    String country = XMLTools.getRequiredStringValue( "Country", wmsNS, element );

    return new ContactAddress_Impl( addressType, address, city, stateOrProvince, postCode, country );
  }

  /**
   * Creates a <tt>Capability</tt> -instance according to the contents of the
   * DOM-subtree starting at the given 'Capability'- <tt>Element</tt>.
   * <p>
   * 
   * @param element
   *          the 'Capability'- <tt>Element</tt>
   * @throws XMLParsingException
   *           if a syntactic or semantic error in the DOM-subtree is
   *           encountered
   * @return the constructed <tt>Capability</tt> -instance
   */
  protected Capability createCapability( Element element ) throws XMLParsingException
  {
    // required: <Request>
    Request request = createRequest( XMLTools.getRequiredChildByName( "Request", wmsNS, element ) );

    // required: <Exception>
    CException exception = createException( XMLTools.getRequiredChildByName( "Exception", wmsNS,
        element ) );

    // optional: <VendorSpecificCapabilities>
    Document vendor = null;
    Element vendorElement = XMLTools.getChildByName( "VendorSpecificCapabilities", wmsNS, element );

    if( vendorElement != null )
    {
      vendor = createVendorSpecificCapabilities( vendorElement );
    }

    // optional: <UserDefinedSymbolization>
    UserDefinedSymbolization userSymbolization = null;
    Element userSymbolizationElement = XMLTools.getChildByName( "UserDefinedSymbolization", wmsNS,
        element );

    if( userSymbolizationElement != null )
    {
      userSymbolization = createUserDefinedSymbolization( userSymbolizationElement );
    }

    // optional: <Layer>
    Layer layer = null;
    Element layerElement = XMLTools.getChildByName( "Layer", wmsNS, element );

    if( layerElement != null )
    {
      layer = createLayer( layerElement );
    }

    return new Capability_Impl( request, exception, vendor, userSymbolization, layer );
  }

  /**
   * Creates a <tt>Request</tt> -instance according to the contents of the
   * DOM-subtree starting at the given 'Request'- <tt>Element</tt>.
   * <p>
   * 
   * @param element
   *          the 'Request'- <tt>Element</tt>
   * @throws XMLParsingException
   *           if a syntactic or semantic error in the DOM-subtree is
   *           encountered
   * @return the constructed <tt>Request</tt> -instance
   */
  protected Request createRequest( Element element ) throws XMLParsingException
  {
    boolean getCapabilities = false;
    boolean getMap = false;

    ArrayList operationList = new ArrayList();
    ElementList elementList = XMLTools.getChildElements( element );

    for( int i = 0; i < elementList.getLength(); i++ )
    {
      Element operationElement = elementList.item( i );
      String name = operationElement.getLocalName();

      if( name.equals( "GetCapabilities" )
          || ( version.equals( "1.0.0" ) && name.equals( "Capabilities" ) ) )
      {
        operationList.add( createOperation( operationElement ) );
        getCapabilities = true;
      }
      else if( name.equals( "GetMap" ) || ( version.equals( "1.0.0" ) && name.equals( "Map" ) ) )
      {
        operationList.add( createOperation( operationElement ) );
        getMap = true;
      }
      else if( name.equals( "GetFeatureInfo" ) || name.equals( "DescribeLayer" )
          || name.equals( "GetLegendGraphic" ) || name.equals( "GetStyles" )
          || name.equals( "PutStyles" )
          || ( version.equals( "1.0.0" ) && name.equals( "FeatureInfo" ) ) )
      {
        operationList.add( createOperation( operationElement ) );
      }
      else
      {
        throw new XMLParsingException( "Element '" + name + "' in element 'Request' is not "
            + "a known Operation!" );
      }
    }

    if( !getCapabilities )
    {
      throw new XMLParsingException( "Mandatory element 'GetCapabilities' of element 'Request' "
          + "is missing!" );
    }

    if( !getMap )
    {
      throw new XMLParsingException( "Mandatory element 'GetMap' of element 'Request' "
          + "is missing!" );
    }

    Operation[] operations = (Operation[])operationList
        .toArray( new Operation[operationList.size()] );
    return new Request_Impl( operations );
  }

  /**
   * Creates an <tt>Operation</tt> -instance according to the contents of the
   * DOM-subtree starting at the given <tt>Element</tt>.
   * <p>
   * 
   * @param element
   *          the <tt>Element</tt> that describes an <tt>Operation</tt>
   * @throws XMLParsingException
   *           if a syntactic or semantic error in the DOM-subtree is
   *           encountered
   * @return the constructed <tt>Operation</tt> -instance
   */
  protected Operation createOperation( Element element ) throws XMLParsingException
  {
    // use node name as name of the Operation to be defined
    String name = element.getNodeName();

    // optional: <Format>(s)
    ElementList nodelist = XMLTools.getChildElementsByName( "Format", wmsNS, element );
    Format[] formats = new Format[nodelist.getLength()];

    for( int i = 0; i < nodelist.getLength(); i++ )
    {
      String s = XMLTools.getStringValue( nodelist.item( i ) );
      formats[i] = new Format_Impl( s, null );
    }

    // optional: <DCPType>(s)
    nodelist = XMLTools.getChildElementsByName( "DCPType", wmsNS, element );

    DCPType[] dcpTypes = new DCPType[nodelist.getLength()];

    for( int i = 0; i < nodelist.getLength(); i++ )
    {
      dcpTypes[i] = createDCPType( nodelist.item( i ) );
    }

    return new Operation_Impl( name, formats, dcpTypes );
  }

  /**
   * Creates a <tt>DCPType</tt> -instance according to the contents of the
   * DOM-subtree starting at the given 'DCPType'- <tt>Element</tt>.
   * <p>
   * 
   * @param element
   *          the 'DCPType'- <tt>Element</tt>
   * @throws XMLParsingException
   *           if a syntactic or semantic error in the DOM-subtree is
   *           encountered
   * @return the constructed <tt>DCPType</tt> -instance
   */
  protected DCPType createDCPType( Element element ) throws XMLParsingException
  {
    // required: <HTTP>
    Element httpElement = XMLTools.getRequiredChildByName( "HTTP", wmsNS, element );

    // optional: <Get>s
    ElementList nodelist = XMLTools.getChildElementsByName( "Get", wmsNS, httpElement );
    URL[] getURLs = new URL[nodelist.getLength()];

    for( int i = 0; i < nodelist.getLength(); i++ )
    {
      if( version.equals( "1.0.0" ) )
      {
        String tmp = XMLTools.getAttrValue( "onlineResource", nodelist.item( i ) );
        try
        {
          getURLs[i] = new URL( tmp );
        }
        catch( Exception e )
        {
          e.printStackTrace();
          throw new XMLParsingException( e.toString() );
        }
      }
      else
      {
        Element onlineResourceElement = XMLTools.getChildByName( "OnlineResource", wmsNS, nodelist
            .item( i ) );
        getURLs[i] = createOnlineResource( onlineResourceElement );
      }
    }

    // optional: <Post>s
    nodelist = XMLTools.getChildElementsByName( "Post", wmsNS, httpElement );

    URL[] postURLs = new URL[nodelist.getLength()];

    for( int i = 0; i < nodelist.getLength(); i++ )
    {
      Element onlineResourceElement = XMLTools.getChildByName( "OnlineResource", wmsNS, nodelist
          .item( i ) );
      postURLs[i] = createOnlineResource( onlineResourceElement );
    }

    return new DCPType_Impl( new HTTP_Impl( getURLs, postURLs ) );
  }

  /**
   * Creates a <tt>CException</tt> -instance according to the contents of the
   * DOM-subtree starting at the given 'Exception'- <tt>Element</tt>.
   * <p>
   * 
   * @param element
   *          the 'Exception'- <tt>Element</tt>
   * @throws XMLParsingException
   *           if a syntactic or semantic error in the DOM-subtree is
   *           encountered
   * @return the constructed <tt>CException</tt> -instance
   */
  protected CException createException( Element element ) throws XMLParsingException
  {
    // required: <Format>(s)
    ElementList nodelist = XMLTools.getChildElementsByName( "Format", wmsNS, element );
    String[] formats = new String[nodelist.getLength()];

    if( formats.length == 0 )
    {
      throw new XMLParsingException( "Element 'Exception' requires at least one 'Format'-element!" );
    }

    for( int i = 0; i < nodelist.getLength(); i++ )
    {
      formats[i] = XMLTools.getStringValue( nodelist.item( i ) );
    }

    return new CException_Impl( formats );
  }

  /**
   * Creates a <tt>Document</tt> -instance according to the contents of the
   * DOM-subtree starting at the given 'VendorSpecificCapabilities'-
   * <tt>Element</tt>.
   * <p>
   * 
   * @param element
   *          the 'VendorSpecificCapabilities'- <tt>Element</tt>
   * @throws XMLParsingException
   *           if a syntactic or semantic error in the DOM-subtree is
   *           encountered
   * @return the constructed <tt>Document</tt> -instance
   */
  protected Document createVendorSpecificCapabilities( Element element ) throws XMLParsingException
  {
    Document doc = null;

    try
    {
      javax.xml.parsers.DocumentBuilder parser = DocumentBuilderFactory.newInstance()
          .newDocumentBuilder();
      doc = parser.newDocument();
      XMLTools.insertNodeInto( element, doc );
    }
    catch( ParserConfigurationException e )
    {
      throw new XMLParsingException( "Error building VendorSpecificCapabilities-document: "
          + e.getMessage() );
    }

    return doc;
  }

  /**
   * Creates a <tt>UserDefinedSymbolization</tt> -instance according to the
   * contents of the DOM-subtree starting at the given
   * 'UserDefinedSymbolization'- <tt>Element</tt>.
   * <p>
   * 
   * @param element
   *          the 'UserDefinedSymbolization'- <tt>Element</tt>
   * @throws XMLParsingException
   *           if a syntactic or semantic error in the DOM-subtree is
   *           encountered
   * @return the constructed <tt>UserDefinedSymbolization</tt> -instance
   */
  protected UserDefinedSymbolization createUserDefinedSymbolization( Element element )
      throws XMLParsingException
  {
    // optional: 'SupportSLD'-attribute
    boolean supportSLD = false;
    String attr = XMLTools.getAttrValue( element, "SupportSLD" );

    if( ( attr != null ) && attr.equals( "1" ) )
    {
      supportSLD = true;
    }

    // optional: 'UserLayer'-attribute
    boolean userLayer = false;
    attr = XMLTools.getAttrValue( element, "UserLayer" );

    if( ( attr != null ) && attr.equals( "1" ) )
    {
      userLayer = true;
    }

    // optional: 'UserStyle'-attribute
    boolean userStyle = false;
    attr = XMLTools.getAttrValue( element, "UserStyle" );

    if( ( attr != null ) && attr.equals( "1" ) )
    {
      userStyle = true;
    }

    // optional: 'RemoteWFS'-attribute
    boolean remoteWFS = false;
    attr = XMLTools.getAttrValue( element, "RemoteWFS" );

    if( ( attr != null ) && attr.equals( "1" ) )
    {
      remoteWFS = true;
    }

    return new UserDefinedSymbolization_Impl( supportSLD, userLayer, remoteWFS, userStyle );
  }

  /**
   * Creates a <tt>Layer</tt> -instance according to the contents of the
   * DOM-subtree starting at the given 'Layer'- <tt>Element</tt>.
   * <p>
   * 
   * @param element
   *          the 'Layer'- <tt>Element</tt>
   * @throws XMLParsingException
   *           if a syntactic or semantic error in the DOM-subtree is
   *           encountered
   * @return the constructed <tt>Layer/tt>-instance
   */
  protected Layer createLayer( Element element ) throws XMLParsingException
  {
    // optional: 'queryable'-attribute
    boolean queryable = false;
    String attr = XMLTools.getAttrValue( element, "queryable" );

    if( ( attr != null ) && attr.equals( "1" ) )
    {
      queryable = true;
    }

    // optional: 'cascaded'-attribute
    int cascaded = -1;
    attr = XMLTools.getAttrValue( element, "cascaded" );

    if( attr != null )
    {
      try
      {
        cascaded = Integer.parseInt( attr );
      }
      catch( NumberFormatException e )
      {
        throw new XMLParsingException( "Given value ('" + attr + "') for attribute 'cascaded' of "
            + "element 'Layer' does not denote a valid integer value." );
      }
    }

    // optional: 'opaque'-attribute
    boolean opaque = false;
    attr = XMLTools.getAttrValue( element, "opaque" );

    if( ( attr != null ) && attr.equals( "1" ) )
    {
      opaque = true;
    }

    // optional: 'noSubsets'-attribute
    boolean noSubsets = false;
    attr = XMLTools.getAttrValue( element, "noSubsets" );

    if( ( attr != null ) && attr.equals( "1" ) )
    {
      noSubsets = true;
    }

    // optional: 'fixedWidth'-attribute
    int fixedWidth = -1;
    attr = XMLTools.getAttrValue( element, "fixedWidth" );

    if( attr != null )
    {
      try
      {
        fixedWidth = Integer.parseInt( attr );
      }
      catch( NumberFormatException e )
      {
        throw new XMLParsingException( "Given value ('" + attr
            + "') for attribute 'fixedWidth' of "
            + "element 'Layer' does not denote a valid integer value." );
      }
    }

    // optional: 'fixedHeight'-attribute
    int fixedHeight = -1;
    attr = XMLTools.getAttrValue( element, "fixedHeight" );

    if( attr != null )
    {
      try
      {
        fixedHeight = Integer.parseInt( attr );
      }
      catch( NumberFormatException e )
      {
        throw new XMLParsingException( "Given value ('" + attr
            + "') for attribute 'fixedHeight' of "
            + "element 'Layer' does not denote a valid integer value." );
      }
    }

    // optional: <Name>
    String name = XMLTools.getStringValue( "Name", wmsNS, element, null );

    // check if layer name has been used already
    if( name != null && !name.trim().equals( "" ) )
    {
      if( collectedLayers.contains( name ) )
      {
        throw new XMLParsingException( "Layer with name '" + name + "' is defined more than once!" );
      }
      else
      {
        collectedLayers.add( name );
      }
    }

    // required: <Title>
    String title = XMLTools.getRequiredStringValue( "Title", wmsNS, element );

    // optional: <Abstract>
    String abstractString = XMLTools.getStringValue( "Abstract", wmsNS, element, null );

    // optional: <KeywordList>
    String[] keywords = null;
    Element keywordListElement = XMLTools.getChildByName( "KeywordList", wmsNS, element );

    if( keywordListElement != null )
    {
      ElementList nl = XMLTools.getChildElementsByName( "Keyword", wmsNS, keywordListElement );
      keywords = new String[nl.getLength()];

      for( int i = 0; i < keywords.length; i++ )
      {
        keywords[i] = XMLTools.getStringValue( nl.item( i ) );
      }
    }

    // optional: <SRS>(s)
    ElementList nodelist = XMLTools.getChildElementsByName( "SRS", wmsNS, element );
    ArrayList srsL = new ArrayList( 100 );

    for( int i = 0; i < nodelist.getLength(); i++ )
    {
      String tmp = XMLTools.getStringValue( nodelist.item( i ) );
      if( tmp != null && !tmp.trim().equals( "" ) )
      {
        String[] crs = StringExtend.toArray( tmp, " ,;", true );
        for( int j = 0; j < crs.length; j++ )
        {
          srsL.add( crs[j] );
        }
      }
    }
    String[] srs = (String[])srsL.toArray( new String[srsL.size()] );

    // optional: <LatLonBoundingBox>
    GM_Envelope bbox = null;
    Element bboxElement = XMLTools.getChildByName( "LatLonBoundingBox", wmsNS, element );

    if( bboxElement != null )
    {
      bbox = createLatLonBoundingBox( bboxElement );
    }

    // optional: <BoundingBox>(s)
    nodelist = XMLTools.getChildElementsByName( "BoundingBox", wmsNS, element );

    LayerBoundingBox[] bboxes = new LayerBoundingBox[nodelist.getLength()];

    for( int i = 0; i < nodelist.getLength(); i++ )
    {
      bboxes[i] = createBoundingBox( nodelist.item( i ) );
    }

    // optional: <Dimension>(s)
    nodelist = XMLTools.getChildElementsByName( "Dimension", wmsNS, element );

    Dimension[] dimensions = new Dimension[nodelist.getLength()];

    for( int i = 0; i < nodelist.getLength(); i++ )
    {
      dimensions[i] = createDimension( nodelist.item( i ) );
    }

    // optional: <Extent>(s)
    nodelist = XMLTools.getChildElementsByName( "Extent", wmsNS, element );

    Extent[] extents = new Extent[nodelist.getLength()];

    for( int i = 0; i < nodelist.getLength(); i++ )
    {
      extents[i] = createExtent( nodelist.item( i ) );
    }

    // optional: <Attribution>
    Attribution attribution = null;
    Element attributionElement = XMLTools.getChildByName( "Attribution", wmsNS, element );

    if( attributionElement != null )
    {
      attribution = createAttribution( attributionElement );
    }

    // optional: <AuthorityURL>(s)
    nodelist = XMLTools.getChildElementsByName( "AuthorityURL", wmsNS, element );

    AuthorityURL[] authorityURLs = new AuthorityURL[nodelist.getLength()];

    for( int i = 0; i < nodelist.getLength(); i++ )
    {
      authorityURLs[i] = createAuthorityURL( nodelist.item( i ) );
    }

    // optional: <Identifier>(s)
    nodelist = XMLTools.getChildElementsByName( "Identifier", wmsNS, element );

    Identifier[] identifiers = new Identifier[nodelist.getLength()];

    for( int i = 0; i < nodelist.getLength(); i++ )
    {
      identifiers[i] = createIdentifier( nodelist.item( i ) );
    }

    // optional: <MetadataURL>(s)
    nodelist = XMLTools.getChildElementsByName( "MetadataURL", wmsNS, element );

    MetadataURL[] metadataURLs = new MetadataURL[nodelist.getLength()];

    for( int i = 0; i < nodelist.getLength(); i++ )
    {
      metadataURLs[i] = createMetadataURL( nodelist.item( i ) );
    }

    // optional: <DataURL>(s)
    nodelist = XMLTools.getChildElementsByName( "DataURL", wmsNS, element );

    DataURL[] dataURLs = new DataURL[nodelist.getLength()];

    for( int i = 0; i < nodelist.getLength(); i++ )
    {
      dataURLs[i] = createDataURL( nodelist.item( i ) );
    }

    // optional: <FeatureListURL>(s)
    nodelist = XMLTools.getChildElementsByName( "FeatureListURL", wmsNS, element );

    FeatureListURL[] featureListURLs = new FeatureListURL[nodelist.getLength()];

    for( int i = 0; i < nodelist.getLength(); i++ )
    {
      featureListURLs[i] = createFeatureListURL( nodelist.item( i ) );
    }

    // optional: <Style>(s)
    nodelist = XMLTools.getChildElementsByName( "Style", wmsNS, element );

    Style[] styles = new Style[nodelist.getLength()];

    if( nodelist.getLength() > 0 )
    {
      for( int i = 0; i < nodelist.getLength(); i++ )
      {
        styles[i] = createStyle( nodelist.item( i ) );
      }
    }

    // optional: <ScaleHint>
    ScaleHint scaleHint = null;
    Element scaleHintElement = XMLTools.getChildByName( "ScaleHint", wmsNS, element );

    if( scaleHintElement != null )
    {
      scaleHint = createScaleHint( scaleHintElement );
    }

    // optional: <Layer>(s)
    nodelist = XMLTools.getChildElementsByName( "Layer", wmsNS, element );

    Layer[] layers = new Layer[nodelist.getLength()];

    for( int i = 0; i < nodelist.getLength(); i++ )
    {
      layers[i] = createLayer( nodelist.item( i ) );
    }

    Layer layer = new Layer_Impl( queryable, cascaded, opaque, noSubsets, fixedWidth, fixedHeight,
        name, title, abstractString, bbox, attribution, scaleHint, keywords, srs, bboxes,
        dimensions, extents, authorityURLs, identifiers, metadataURLs, dataURLs, featureListURLs,
        styles, layers, null, null );

    // set the new layer as parent of its child layers
    if( layers != null )
    {
      for( int i = 0; i < layers.length; i++ )
      {
        ( (Layer_Impl)layers[i] ).setParent( layer );
      }
    }

    return layer;
  }

  /**
   * Creates an <tt>Attribution</tt> -instance according to the contents of
   * the DOM-subtree starting at the given 'Attribution'- <tt>Element</tt>.
   * <p>
   * 
   * @param element
   *          the 'Attribution'- <tt>Element</tt>
   * @throws XMLParsingException
   *           if a syntactic or semantic error in the DOM-subtree is
   *           encountered
   * @return the constructed <tt>Attribution</tt> -instance
   */
  protected Attribution createAttribution( Element element ) throws XMLParsingException
  {
    // optional: <Title>
    String title = XMLTools.getStringValue( "Title", wmsNS, element, null );

    // optional: <OnlineResource>
    URL onlineResource = null;
    Element onlineResourceElement = XMLTools.getChildByName( "OnlineResource", wmsNS, element );
    onlineResource = createOnlineResource( onlineResourceElement );

    // optional: <LogoURL>
    LogoURL logoURL = null;
    Element logoURLElement = XMLTools.getChildByName( "LogoURL", wmsNS, element );

    if( logoURLElement != null )
    {
      logoURL = createLogoURL( logoURLElement );
    }

    return new Attribution_Impl( title, onlineResource, logoURL );
  }

  /**
   * Creates a <tt>GM_Envelope</tt> -instance according to the contents of the
   * DOM-subtree starting at the given 'LatLonBoundingBox'- <tt>Element</tt>.
   * <p>
   * 
   * @param element
   *          the 'LatLonBoundingBox'- <tt>Element</tt>
   * @throws XMLParsingException
   *           if a syntactic or semantic error in the DOM-subtree is
   *           encountered
   * @return the constructed <tt>GM_Envelope</tt> -instance
   */
  protected GM_Envelope createLatLonBoundingBox( Element element ) throws XMLParsingException
  {
    // required: 'minx'-attribute
    double minx = 0.0;

    try
    {
      minx = Double.parseDouble( XMLTools.getRequiredAttrValue( "minx", element ) );
    }
    catch( NumberFormatException e )
    {
      throw new XMLParsingException( "Attribute 'minx' of Element 'LatLonBoundingBox' does not "
          + "denote a valid number value: " + e );
    }

    // required: 'miny'-attribute
    double miny = 0.0;

    try
    {
      miny = Double.parseDouble( XMLTools.getRequiredAttrValue( "miny", element ) );
    }
    catch( NumberFormatException e )
    {
      throw new XMLParsingException( "Attribute 'miny' of Element 'LatLonBoundingBox' does not "
          + "denote a valid number value: " + e );
    }

    // required: 'maxx'-attribute
    double maxx = 0.0;

    try
    {
      maxx = Double.parseDouble( XMLTools.getRequiredAttrValue( "maxx", element ) );
    }
    catch( NumberFormatException e )
    {
      throw new XMLParsingException( "Attribute 'maxx' of Element 'LatLonBoundingBox' does not "
          + "denote a valid number value: " + e );
    }

    // required: 'maxy'-attribute
    double maxy = 0.0;

    try
    {
      maxy = Double.parseDouble( XMLTools.getRequiredAttrValue( "maxy", element ) );
    }
    catch( NumberFormatException e )
    {
      throw new XMLParsingException( "Attribute 'maxy' of Element 'LatLonBoundingBox' does not "
          + "denote a valid number value: " + e );
    }

    GM_Position min = GeometryFactory.createGM_Position( minx, miny );
    GM_Position max = GeometryFactory.createGM_Position( maxx, maxy );

    return GeometryFactory.createGM_Envelope( min, max );
  }

  /**
   * Creates a <tt>LayerBoundingBox</tt> -instance according to the contents
   * of the DOM-subtree starting at the given 'BoundingBox'- <tt>Element</tt>.
   * <p>
   * 
   * @param element
   *          the 'BoundingBox'- <tt>Element</tt>
   * @throws XMLParsingException
   *           if a syntactic or semantic error in the DOM-subtree is
   *           encountered
   * @return the constructed <tt>LayerBoundingBox</tt> -instance
   */
  protected LayerBoundingBox createBoundingBox( Element element ) throws XMLParsingException
  {
    // required: 'SRS'-attribute
    String srs = XMLTools.getRequiredAttrValue( "SRS", element );

    // required: 'minx'-attribute
    double minx = 0.0;

    try
    {
      minx = Double.parseDouble( XMLTools.getRequiredAttrValue( "minx", element ) );
    }
    catch( NumberFormatException e )
    {
      throw new XMLParsingException( "Attribute 'minx' of Element 'LatLonBoundingBox' does not "
          + "denote a valid number value: " + e );
    }

    // required: 'miny'-attribute
    double miny = 0.0;

    try
    {
      miny = Double.parseDouble( XMLTools.getRequiredAttrValue( "miny", element ) );
    }
    catch( NumberFormatException e )
    {
      throw new XMLParsingException( "Attribute 'miny' of Element 'LatLonBoundingBox' does not "
          + "denote a valid number value: " + e );
    }

    // required: 'maxx'-attribute
    double maxx = 0.0;

    try
    {
      maxx = Double.parseDouble( XMLTools.getRequiredAttrValue( "maxx", element ) );
    }
    catch( NumberFormatException e )
    {
      throw new XMLParsingException( "Attribute 'maxx' of Element 'LatLonBoundingBox' does not "
          + "denote a valid number value: " + e );
    }

    // required: 'maxy'-attribute
    double maxy = 0.0;

    try
    {
      maxy = Double.parseDouble( XMLTools.getRequiredAttrValue( "maxy", element ) );
    }
    catch( NumberFormatException e )
    {
      throw new XMLParsingException( "Attribute 'maxy' of Element 'LatLonBoundingBox' does not "
          + "denote a valid number value: " + e );
    }

    // required: 'resx'-attribute
    double resx = 0.0;

    try
    {
      String tmp = XMLTools.getAttrValue( "resx", element );
      if( tmp != null )
      {
        resx = Double.parseDouble( tmp );
      }
    }
    catch( NumberFormatException e )
    {
      throw new XMLParsingException( "Attribute 'resx' of Element 'LatLonBoundingBox' does not "
          + "denote a valid number value: " + e );
    }

    // required: 'resy'-attribute
    double resy = 0.0;

    try
    {
      String tmp = XMLTools.getAttrValue( "resy", element );
      if( tmp != null )
      {
        resy = Double.parseDouble( tmp );
      }
    }
    catch( NumberFormatException e )
    {
      throw new XMLParsingException( "Attribute 'resy' of Element 'LatLonBoundingBox' does not "
          + "denote a valid number value: " + e );
    }

    GM_Position min = GeometryFactory.createGM_Position( minx, miny );
    GM_Position max = GeometryFactory.createGM_Position( maxx, maxy );
    return new LayerBoundingBox_Impl( min, max, srs, resx, resy );
  }

  /**
   * Creates a <tt>Dimension</tt> -instance according to the contents of the
   * DOM-subtree starting at the given 'Dimension'- <tt>Element</tt>.
   * <p>
   * 
   * @param element
   *          the 'Dimension'- <tt>Element</tt>
   * @throws XMLParsingException
   *           if a syntactic or semantic error in the DOM-subtree is
   *           encountered
   * @return the constructed <tt>Dimension</tt> -instance
   */
  protected Dimension createDimension( Element element ) throws XMLParsingException
  {
    // required: 'name'-attribute
    String name = XMLTools.getRequiredAttrValue( "name", element );

    // required: 'units'-attribute
    String units = XMLTools.getRequiredAttrValue( "units", element );

    // optional: 'unitSymbol'-attribute
    String unitSymbol = XMLTools.getAttrValue( "unitSymbol", element );
    return new Dimension_Impl( name, units, unitSymbol );
  }

  /**
   * Creates an <tt>Extent</tt> -instance according to the contents of the
   * DOM-subtree starting at the given 'Extent'- <tt>Element</tt>.
   * <p>
   * 
   * @param element
   *          the 'Extent'- <tt>Element</tt>
   * @throws XMLParsingException
   *           if a syntactic or semantic error in the DOM-subtree is
   *           encountered
   * @return the constructed <tt>Extent/tt>-instance
   */
  protected Extent createExtent( Element element ) throws XMLParsingException
  {
    // required: 'name'-attribute
    String name = XMLTools.getRequiredAttrValue( "name", element );

    // optional: 'default'-attribute
    String defaultString = XMLTools.getAttrValue( "default", element );

    // optional: 'nearestValue'-attribute
    boolean nearestValue = false;
    String nearestValueString = XMLTools.getAttrValue( "nearestValue", element );

    if( ( nearestValueString != null ) && nearestValueString.equals( "1" ) )
    {
      nearestValue = true;
    }

    return new Extent_Impl( name, defaultString, nearestValue );
  }

  /**
   * Creates a <tt>LogoURL</tt> -instance according to the contents of the
   * DOM-subtree starting at the given 'LogoURL'- <tt>Element</tt>.
   * <p>
   * 
   * @param element
   *          the 'LogoURL'- <tt>Element</tt>
   * @throws XMLParsingException
   *           if a syntactic or semantic error in the DOM-subtree is
   *           encountered
   * @return the constructed <tt>LogoURL</tt> -instance
   */
  protected LogoURL createLogoURL( Element element ) throws XMLParsingException
  {
    // required: 'width'-attribute
    int width = 0;

    try
    {
      width = Integer.parseInt( XMLTools.getRequiredAttrValue( "width", element ) );
    }
    catch( NumberFormatException e )
    {
      throw new XMLParsingException( "Attribute 'width' of Element 'LogoURL' does not "
          + "denote a valid integer value: " + e );
    }

    // required: 'height'-attribute
    int height = 0;

    try
    {
      height = Integer.parseInt( XMLTools.getRequiredAttrValue( "height", element ) );
    }
    catch( NumberFormatException e )
    {
      throw new XMLParsingException( "Attribute 'height' of Element 'LogoURL' does not "
          + "denote a valid integer value: " + e );
    }

    // required: <Format>
    String format = XMLTools.getRequiredStringValue( "Format", wmsNS, element );

    // required: <OnlineResource>
    URL onlineResource = createOnlineResource( XMLTools.getRequiredChildByName( "OnlineResource",
        wmsNS, element ) );

    return new LogoURL_Impl( width, height, format, onlineResource );
  }

  /**
   * Creates an <tt>AuthorityURL</tt> -instance according to the contents of
   * the DOM-subtree starting at the given 'AuthorityURL'- <tt>Element</tt>.
   * <p>
   * 
   * @param element
   *          the 'AuthorityURL'- <tt>Element</tt>
   * @throws XMLParsingException
   *           if a syntactic or semantic error in the DOM-subtree is
   *           encountered
   * @return the constructed <tt>AuthorityURL</tt> -instance
   */
  protected AuthorityURL createAuthorityURL( Element element ) throws XMLParsingException
  {
    // required: 'name'-attribute
    String name = XMLTools.getRequiredAttrValue( "name", element );

    // required: <OnlineResource>
    URL onlineResource = createOnlineResource( XMLTools.getRequiredChildByName( "OnlineResource",
        wmsNS, element ) );

    return new AuthorityURL_Impl( name, onlineResource );
  }

  /**
   * Creates an <tt>Identifier</tt> -instance according to the contents of the
   * DOM-subtree starting at the given 'Identifier'- <tt>Element</tt>.
   * <p>
   * 
   * @param element
   *          the 'Identifier'- <tt>Element</tt>
   * @throws XMLParsingException
   *           if a syntactic or semantic error in the DOM-subtree is
   *           encountered
   * @return the constructed <tt>Identifier</tt> -instance
   */
  protected Identifier createIdentifier( Element element ) throws XMLParsingException
  {
    // required: 'authority'-attribute
    String authority = XMLTools.getRequiredAttrValue( "authority", element );

    return new Identifier_Impl( XMLTools.getStringValue( element ), authority );
  }

  /**
   * Creates a <tt>MetadataURL</tt> -instance according to the contents of the
   * DOM-subtree starting at the given 'MetadataURL'- <tt>Element</tt>.
   * <p>
   * 
   * @param element
   *          the 'MetadataURL'- <tt>Element</tt>
   * @throws XMLParsingException
   *           if a syntactic or semantic error in the DOM-subtree is
   *           encountered
   * @return the constructed <tt>MetadataURL</tt> -instance
   */
  protected MetadataURL createMetadataURL( Element element ) throws XMLParsingException
  {
    // required: 'type'-attribute
    String type = XMLTools.getRequiredAttrValue( "type", element );

    if( !( type.equals( "TC211" ) || type.equals( "FGDC" ) ) )
    {
      throw new XMLParsingException( "Given value ('" + type
          + "') for attribute 'type' of element "
          + "'MetadataURL' is invalid: allowed values are 'TC211' " + "and 'FGDC'!" );
    }

    // required: <Format>
    String format = XMLTools.getRequiredStringValue( "Format", wmsNS, element );

    // required: <OnlineResource>
    URL onlineResource = createOnlineResource( XMLTools.getRequiredChildByName( "OnlineResource",
        wmsNS, element ) );

    return new MetadataURL_Impl( type, format, onlineResource );
  }

  /**
   * Creates a <tt>DataURL</tt> -instance according to the contents of the
   * DOM-subtree starting at the given 'DataURL'- <tt>Element</tt>.
   * <p>
   * 
   * @param element
   *          the 'DataURL'- <tt>Element</tt>
   * @throws XMLParsingException
   *           if a syntactic or semantic error in the DOM-subtree is
   *           encountered
   * @return the constructed <tt>DataURL</tt> -instance
   */
  protected DataURL createDataURL( Element element ) throws XMLParsingException
  {
    // required: <Format>
    String format = XMLTools.getRequiredStringValue( "Format", wmsNS, element );

    // required: <OnlineResource>
    URL onlineResource = createOnlineResource( XMLTools.getRequiredChildByName( "OnlineResource",
        wmsNS, element ) );

    return new DataURL_Impl( format, onlineResource );
  }

  /**
   * Creates a <tt>FeatureListURL</tt> -instance according to the contents of
   * the DOM-subtree starting at the given 'FeatureListURL'- <tt>Element</tt>.
   * <p>
   * 
   * @param element
   *          the 'FeatureListURL'- <tt>Element</tt>
   * @throws XMLParsingException
   *           if a syntactic or semantic error in the DOM-subtree is
   *           encountered
   * @return the constructed <tt>FeatureListURL</tt> -instance
   */
  protected FeatureListURL createFeatureListURL( Element element ) throws XMLParsingException
  {
    // required: <Format>
    String format = XMLTools.getRequiredStringValue( "Format", wmsNS, element );

    // required: <OnlineResource>
    URL onlineResource = createOnlineResource( XMLTools.getRequiredChildByName( "OnlineResource",
        wmsNS, element ) );

    return new FeatureListURL_Impl( format, onlineResource );
  }

  /**
   * Creates a <tt>Style</tt> -instance according to the contents of the
   * DOM-subtree starting at the given 'Style'- <tt>Element</tt>.
   * <p>
   * 
   * @param element
   *          the 'Style'- <tt>Element</tt>
   * @throws XMLParsingException
   *           if a syntactic or semantic error in the DOM-subtree is
   *           encountered
   * @return the constructed <tt>Style</tt> -instance
   */
  protected Style createStyle( Element element ) throws XMLParsingException
  {
    // required: <Name>
    String name = XMLTools.getRequiredStringValue( "Name", wmsNS, element );

    // required: <Title>
    String title = XMLTools.getStringValue( "Title", wmsNS, element, name );

    // optional: <Abstract>
    String abstractString = XMLTools.getStringValue( "Abstract", wmsNS, element, null );

    // optional: <LegendURL>(s)
    ElementList nodelist = XMLTools.getChildElementsByName( "LegendURL", wmsNS, element );
    LegendURL[] legendURLs = new LegendURL[nodelist.getLength()];

    for( int i = 0; i < nodelist.getLength(); i++ )
    {
      legendURLs[i] = createLegendURL( nodelist.item( i ) );
    }

    // optional: <StyleSheetURL>
    StyleSheetURL styleSheetURL = null;
    Element styleSheetURLElement = XMLTools.getChildByName( "StyleSheetURL", wmsNS, element );

    if( styleSheetURLElement != null )
    {
      styleSheetURL = createStyleSheetURL( styleSheetURLElement );
    }

    // optional: <StyleURL>
    StyleURL styleURL = null;
    Element styleURLElement = XMLTools.getChildByName( "StyleURL", wmsNS, element );

    if( styleURLElement != null )
    {
      styleURL = createStyleURL( styleURLElement );
    }

    return new Style_Impl( name, title, abstractString, legendURLs, styleSheetURL, styleURL, null );
  }

  /**
   * Creates a <tt>LegendURL</tt> -instance according to the contents of the
   * DOM-subtree starting at the given 'LegendURL'- <tt>Element</tt>.
   * <p>
   * 
   * @param element
   *          the 'LegendURL'- <tt>Element</tt>
   * @throws XMLParsingException
   *           if a syntactic or semantic error in the DOM-subtree is
   *           encountered
   * @return the constructed <tt>LegendURL</tt> -instance
   */
  protected LegendURL createLegendURL( Element element ) throws XMLParsingException
  {
    // required: 'width'-attribute
    int width = 0;

    try
    {
      width = Integer.parseInt( XMLTools.getRequiredAttrValue( "width", element ) );
    }
    catch( NumberFormatException e )
    {
      throw new XMLParsingException( "Attribute 'width' of Element 'LogoURL' does not "
          + "denote a valid integer value: " + e );
    }

    // required: 'height'-attribute
    int height = 0;

    try
    {
      height = Integer.parseInt( XMLTools.getRequiredAttrValue( "height", element ) );
    }
    catch( NumberFormatException e )
    {
      throw new XMLParsingException( "Attribute 'height' of Element 'LogoURL' does not "
          + "denote a valid integer value: " + e );
    }

    // required: <Format>
    String format = XMLTools.getRequiredStringValue( "Format", wmsNS, element );

    // required: <OnlineResource>
    URL onlineResource = createOnlineResource( XMLTools.getRequiredChildByName( "OnlineResource",
        wmsNS, element ) );

    return new LegendURL_Impl( width, height, format, onlineResource );
  }

  /**
   * Creates a <tt>StyleSheetURL</tt> -instance according to the contents of
   * the DOM-subtree starting at the given 'StyleSheetURL'- <tt>Element</tt>.
   * <p>
   * 
   * @param element
   *          the 'StyleSheetURL'- <tt>Element</tt>
   * @throws XMLParsingException
   *           if a syntactic or semantic error in the DOM-subtree is
   *           encountered
   * @return the constructed <tt>StyleSheetURL</tt> -instance
   */
  protected StyleSheetURL createStyleSheetURL( Element element ) throws XMLParsingException
  {
    // required: <Format>
    String format = XMLTools.getRequiredStringValue( "Format", wmsNS, element );

    // required: <OnlineResource>
    URL onlineResource = createOnlineResource( XMLTools.getRequiredChildByName( "OnlineResource",
        wmsNS, element ) );

    return new StyleSheetURL_Impl( format, onlineResource );
  }

  /**
   * Creates a <tt>StyleURL</tt> -instance according to the contents of the
   * DOM-subtree starting at the given 'StyleURL'- <tt>Element</tt>.
   * <p>
   * 
   * @param element
   *          the 'StyleURL'- <tt>Element</tt>
   * @throws XMLParsingException
   *           if a syntactic or semantic error in the DOM-subtree is
   *           encountered
   * @return the constructed <tt>StyleURL</tt> -instance
   */
  protected StyleURL createStyleURL( Element element ) throws XMLParsingException
  {
    // required: <Format>
    String format = XMLTools.getRequiredStringValue( "Format", wmsNS, element );

    // required: <OnlineResource>
    URL onlineResource = createOnlineResource( XMLTools.getRequiredChildByName( "OnlineResource",
        wmsNS, element ) );

    return new StyleURL_Impl( format, onlineResource );
  }

  /**
   * Creates a <tt>ScaleHint</tt> -instance according to the contents of the
   * DOM-subtree starting at the given 'ScaleHint'- <tt>Element</tt>.
   * <p>
   * 
   * @param element
   *          the 'ScaleHint'- <tt>Element</tt>
   * @throws XMLParsingException
   *           if a syntactic or semantic error in the DOM-subtree is
   *           encountered
   * @return the constructed <tt>ScaleHint</tt> -instance
   */
  protected ScaleHint createScaleHint( Element element ) throws XMLParsingException
  {
    // required: 'min'-attribute
    double min = 0.0;

    try
    {
      min = Double.parseDouble( XMLTools.getRequiredAttrValue( "min", element ) );
    }
    catch( NumberFormatException e )
    {
      throw new XMLParsingException( "Attribute 'min' of Element 'ScaleHint' does not "
          + "denote a valid number value: " + e );
    }

    // required: 'max'-attribute
    double max = 0.0;

    try
    {
      max = Double.parseDouble( XMLTools.getRequiredAttrValue( "max", element ) );
    }
    catch( NumberFormatException e )
    {
      throw new XMLParsingException( "Attribute 'max' of Element 'ScaleHint' does not "
          + "denote a valid number value: " + e );
    }

    return new ScaleHint_Impl( min, max );
  }

  public static void main( String[] args ) throws Exception
  {

    OGCWMSCapabilitiesFactory fac = new OGCWMSCapabilitiesFactory();
    fac.createCapabilities( new URL( "file:///c:/temp/MapGuideLiteView_capa.xml" ) );

  }
}