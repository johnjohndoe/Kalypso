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
package org.deegree_impl.services.wcts.capabilities;

import java.io.Reader;
import java.net.URL;
import java.util.ArrayList;

import org.deegree.ogcbasic.ContactAddress;
import org.deegree.ogcbasic.ContactInformation;
import org.deegree.ogcbasic.ContactPersonPrimary;
import org.deegree.services.capabilities.DCPType;
import org.deegree.services.capabilities.HTTP;
import org.deegree.services.capabilities.Service;
import org.deegree.services.wcts.capabilities.ActionType;
import org.deegree.services.wcts.capabilities.CTS_Capabilities;
import org.deegree.services.wcts.capabilities.Capability;
import org.deegree.services.wcts.capabilities.KnownCoordinateReferenceSystem;
import org.deegree.services.wcts.capabilities.KnownTransformationType;
import org.deegree.services.wcts.capabilities.WCTS_Request;
import org.deegree.xml.XMLTools;
import org.deegree_impl.ogcbasic.ContactAddress_Impl;
import org.deegree_impl.ogcbasic.ContactInformation_Impl;
import org.deegree_impl.ogcbasic.ContactPersonPrimary_Impl;
import org.deegree_impl.services.capabilities.DCPType_Impl;
import org.deegree_impl.services.capabilities.HTTP_Impl;
import org.deegree_impl.services.capabilities.Service_Impl;
import org.deegree_impl.tools.Debug;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

/**
 * Factory class for creating WCTS capability classes from a WCTS capabilities
 * XML document that's conform to the WCTS-specification.
 * 
 * <p>
 * ----------------------------------------------------------------------
 * </p>
 * 
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 * @version 2002-07-12
 */
public final class WCTSCapabilitiesFactory
{
  /**
   * factory method for creating a <tt>WCTSCapabilities</tt> object from a
   * file that contains a WCTS-specification conform XML capabilities document
   */
  public static synchronized CTS_Capabilities createCapabilities( Reader reader ) throws Exception
  {
    //Debug.level = Debug.ALL;
    Debug.debugMethodBegin( "WCTSCapabilitiesFactory", "createCapabilities" );

    Document doc = XMLTools.parse( reader );
    CTS_Capabilities capabilities = createCapabilities( doc );

    Debug.debugMethodEnd();

    return capabilities;
  }

  /**
   * factory method for creating a <tt>WCTSCapabilities</tt> object from a
   * WCTS-specification conform XML capabilities document
   */
  public static synchronized CTS_Capabilities createCapabilities( Document doc ) throws Exception
  {
    Debug.debugMethodBegin( "WCTSCapabilitiesFactory", "createCapabilities" );

    Element root = doc.getDocumentElement();

    //get general service informations
    String version = XMLTools.getAttrValue( root, "version" );
    String updateSequence = XMLTools.getAttrValue( root, "updateSequence" );
    // get service section
    Element element = XMLTools.getNamedChild( root, "Service" );
    // (Element)root.getElementsByTagName( "Service" ).item(0);
    Service service = getService( element );

    // get capability section
    element = XMLTools.getNamedChild( root, "Capability" );

    Capability capability = getCapability( element );

    // create capabilities object
    CTS_Capabilities capabilities = new CTS_Capabilities_Impl( version, updateSequence, service,
        capability );

    Debug.debugMethodEnd();

    return capabilities;
  }

  /**
   * returns an instance of an object that capsulates the service element of the
   * WCTS capabilities.
   */
  private static Service getService( Element serviceElement )
  {
    Debug.debugMethodBegin( "WFSCapabilitiesFactory", "getService" );

    // get service name
    Element element = XMLTools.getNamedChild( serviceElement, "Name" );
    String name = null;

    if( element != null )
    {
      name = element.getFirstChild().getNodeValue();
    }

    // get service title
    element = XMLTools.getNamedChild( serviceElement, "Title" );

    String title = null;

    if( element != null )
    {
      title = element.getFirstChild().getNodeValue();
    }

    // get service abstract
    element = XMLTools.getNamedChild( serviceElement, "Abstract" );

    String abstract_ = "";

    if( element != null )
    {
      abstract_ = element.getFirstChild().getNodeValue();
    }

    // get service keywords
    element = XMLTools.getNamedChild( serviceElement, "KeywordList" );

    String[] keywords = null;

    if( element != null )
    {
      keywords = getKeywordList( element );
    }

    // get service online resource
    element = XMLTools.getNamedChild( serviceElement, "OnlineResource" );

    String orc = XMLTools.getAttrValue( element, "href" );
    URL onlineResource = null;

    try
    {
      onlineResource = new URL( orc );
    }
    catch( Exception ex )
    {
      System.out.println( "getService: " + ex );
    }

    // getContactInformation
    element = XMLTools.getNamedChild( serviceElement, "ContactInformation" );

    ContactInformation contactinformation = getContactInformation( element );

    // get service fees
    element = XMLTools.getNamedChild( serviceElement, "Fees" );

    String fees = null;

    if( element != null )
    {
      fees = element.getFirstChild().getNodeValue();
    }

    // get service access constraints
    element = XMLTools.getNamedChild( serviceElement, "AccessConstraints" );

    String accessConstraints = null;

    if( element != null )
    {
      accessConstraints = element.getFirstChild().getNodeValue();
    }

    Service service = new Service_Impl( name, title, abstract_, keywords, onlineResource,
        contactinformation, fees, accessConstraints );

    Debug.debugMethodEnd();

    return service;
  }

  /**
   * returns getContactInformation
   */
  public static ContactInformation getContactInformation( Element contactElement )
  {
    Debug.debugMethodBegin( "WFSCapabilitiesFactory", "getContactInformation" );

    //ContactPersonPrimary
    Element element = XMLTools.getNamedChild( contactElement, "ContactPersonPrimary" );
    ContactPersonPrimary contactPersonPrimary = null;

    if( element != null )
    {
      contactPersonPrimary = getContactPersonPrimary( element );
    }

    //ContactPosition
    element = XMLTools.getNamedChild( contactElement, "ContactPosition" );

    String contactPosition = null;

    if( element != null )
    {
      contactPosition = element.getFirstChild().getNodeValue();
    }

    //ContactAddress
    element = XMLTools.getNamedChild( contactElement, "ContactAddress" );

    ContactAddress contactAddress = null;

    if( element != null )
    {
      contactAddress = getContactAddress( element );
    }

    //ContactVoiceTelephon
    element = XMLTools.getNamedChild( contactElement, "ContactVoiceTelephon" );

    String contactVoiceTelephone = null;

    if( element != null )
    {
      contactVoiceTelephone = element.getFirstChild().getNodeValue();
    }

    //ContactFacsimileTelephone
    element = XMLTools.getNamedChild( contactElement, "ContactFacsimileTelephone" );

    String contactFacsimileTelephone = null;

    if( element != null )
    {
      contactFacsimileTelephone = element.getFirstChild().getNodeValue();
    }

    //ContactElectronicMailAddress
    element = XMLTools.getNamedChild( contactElement, "ContactElectronicMailAddress" );

    String contactElectronicMailAddress = null;

    if( element != null )
    {
      contactElectronicMailAddress = element.getFirstChild().getNodeValue();
    }

    ContactInformation contactinformation = new ContactInformation_Impl( contactPosition,
        contactVoiceTelephone, contactFacsimileTelephone, contactElectronicMailAddress,
        contactPersonPrimary, contactAddress );

    Debug.debugMethodEnd();

    return contactinformation;
  }

  /**
   * returns ContactPersonPrimary
   */
  public static ContactPersonPrimary getContactPersonPrimary( Element contactPersonPrimaryElement )
  {
    Debug.debugMethodBegin( "WFSCapabilitiesFactory", "getContactPersonPrimary" );

    //ContactPerson
    Element element = XMLTools.getNamedChild( contactPersonPrimaryElement, "ContactPerson" );
    String contactperson = null;

    if( element != null )
    {
      contactperson = element.getFirstChild().getNodeValue();
    }

    //ContactOrganization
    element = XMLTools.getNamedChild( contactPersonPrimaryElement, "ContactOrganization" );

    String contactorganization = null;

    if( element != null )
    {
      contactorganization = element.getFirstChild().getNodeValue();
    }

    ContactPersonPrimary contactpersonprimary = new ContactPersonPrimary_Impl( contactperson,
        contactorganization );

    Debug.debugMethodEnd();

    return contactpersonprimary;
  }

  /**
   * gets ContactAddress
   */
  public static ContactAddress getContactAddress( Element contactadressElement )
  {
    Debug.debugMethodBegin( "WFSCapabilitiesFactory", "getContactAddress" );

    //AddressType
    Element element = XMLTools.getNamedChild( contactadressElement, "AddressType" );
    String addressType = null;

    if( element != null )
    {
      addressType = element.getFirstChild().getNodeValue();
    }

    //Address
    element = XMLTools.getNamedChild( contactadressElement, "Address" );

    String address = null;

    if( element != null )
    {
      address = element.getFirstChild().getNodeValue();
    }

    //City
    element = XMLTools.getNamedChild( contactadressElement, "City" );

    String city = null;

    if( element != null )
    {
      city = element.getFirstChild().getNodeValue();
    }

    //StateOrProvince
    element = XMLTools.getNamedChild( contactadressElement, "StateOrProvince" );

    String stateOrProvince = null;

    if( element != null )
    {
      stateOrProvince = element.getFirstChild().getNodeValue();
    }

    //PostCode
    element = XMLTools.getNamedChild( contactadressElement, "PostCode" );

    String postCode = null;

    if( element != null )
    {
      postCode = element.getFirstChild().getNodeValue();
    }

    //Country
    element = XMLTools.getNamedChild( contactadressElement, "Country" );

    String country = null;

    if( element != null )
    {
      country = element.getFirstChild().getNodeValue();
    }

    ContactAddress contactaddress = new ContactAddress_Impl( addressType, address, city,
        stateOrProvince, postCode, country );

    Debug.debugMethodEnd();

    return contactaddress;
  }

  /**
   * returns the keywords associated with the service
   */
  private static String[] getKeywordList( Element keywordlistElement )
  {
    Debug.debugMethodBegin( "WCTSCapabilitiesFactory", "getKeywordList" );

    String keyword = null;

    NodeList nodelist = keywordlistElement.getElementsByTagName( "Keyword" );

    String[] kw = new String[nodelist.getLength()];

    if( nodelist != null )
    {
      for( int i = 0; i < nodelist.getLength(); i++ )
      {
        keyword = nodelist.item( i ).getFirstChild().getNodeValue();
        kw[i] = keyword;
      }
    }

    Debug.debugMethodEnd();
    return kw;
  }

  /*
   * #######################################################################
   * #######################################################################
   * ########################### capability ################################
   * #######################################################################
   * #######################################################################
   */

  /**
   * returns an instance of an object that capsulates the capabilty element of
   * the WCTS capabilities.
   * <p>
   * Contains the two attributes <tt>userDefinedCoordinateSystems</tt> and
   * <tt>userDefinedTransformations</tt>
   */
  private static Capability getCapability( Element capElement ) throws Exception
  {
    Debug.debugMethodBegin( "WFSCapabilitiesFactory", "getCapability" );

    //get Capability attribute informations
    String userDefinedCoordinateSystems_String = XMLTools.getAttrValue( capElement,
        "userDefinedCoordinateSystems" );

    boolean userDefinedCoordinateSystems = false;

    if( userDefinedCoordinateSystems_String.equals( "true" )
        || userDefinedCoordinateSystems_String.equals( "1" ) )
    {
      userDefinedCoordinateSystems = true;
    }

    String userDefinedTransformations_String = XMLTools.getAttrValue( capElement,
        "userDefinedTransformations" );

    boolean userDefinedTransformations = false;

    if( userDefinedTransformations_String.equals( "true" )
        || userDefinedTransformations_String.equals( "0" ) )
    {
      userDefinedTransformations = true;
    }

    // gets the request. 1 time.
    Element element = XMLTools.getNamedChild( capElement, "Request" );
    WCTS_Request request = getRequest( element );

    // gets the KnownTransformationType which is present at least 1 time.
    NodeList nl = capElement.getElementsByTagName( "KnownTransformationType" );

    if( ( nl == null ) || ( nl.getLength() == 0 ) )
    {
      throw new Exception( "no KnownTransformationType defined!" );
    }

    KnownTransformationType ktt = null;
    KnownTransformationType[] ktt_array = new KnownTransformationType[nl.getLength()];

    for( int i = 0; i < ktt_array.length; i++ )
    {
      ktt = getKnownTransformationType( (Element)nl.item( i ) );
      ktt_array[i] = ktt;
    }

    // gets the KnownCoordinateReferenceSystem which must be present
    // two times at least, to get a transformation.
    NodeList nlkcrs = capElement.getElementsByTagName( "KnownCoordinateReferenceSystem" );

    if( ( nlkcrs == null ) || ( nlkcrs.getLength() < 2 ) )
    {
      throw new Exception( "not enough KnownCoordinateReferenceSystem defined!" );
    }

    KnownCoordinateReferenceSystem kcrs = null;
    KnownCoordinateReferenceSystem[] kcrs_array = new KnownCoordinateReferenceSystem[nlkcrs
        .getLength()];

    for( int i = 0; i < kcrs_array.length; i++ )
    {
      kcrs = getKnownCoordinateReferenceSystem( (Element)nlkcrs.item( i ) );
      kcrs_array[i] = kcrs;
    }

    // get the VendorSpecificCapabilities, which is only a string.
    // NOT IMPLEMENTED. NOT NEEDED, BECAUSE NO STANDARDS.
    Capability capability = new Capability_Impl( userDefinedCoordinateSystems,
        userDefinedTransformations, request, ktt_array, kcrs_array, null );

    Debug.debugMethodEnd();

    return capability;
  }

  /**
   * returns the request associated with the capability
   */
  private static WCTS_Request getRequest( Element requestElement )
  {
    Debug.debugMethodBegin( "WCTSCapabilitiesFactory", "getRequest" );

    Element element = XMLTools.getNamedChild( requestElement, "GetCapabilities" );
    ActionType getCapabilities = getGetCapabilities( element );

    element = XMLTools.getNamedChild( requestElement, "Transform" );

    ActionType transform = getTransform( element );

    element = XMLTools.getNamedChild( requestElement, "IsTransformable" );

    ActionType isTransformable = getIsTransformable( element );

    element = XMLTools.getNamedChild( requestElement, "DescribeTransformation" );

    ActionType describeTransformation = getDescribeTransformation( element );

    WCTS_Request request = new WCTS_Request_Impl( getCapabilities, transform, isTransformable,
        describeTransformation );
    Debug.debugMethodEnd();
    return request;
  }

  /**
   * gets the KnownTransformationType which is present at least 1 time.
   */
  private static KnownTransformationType getKnownTransformationType( Element kttElement )
  {
    Debug.debugMethodBegin( "WCTSCapabilitiesFactory", "getKnownTransformationType" );

    // Name
    Element element = XMLTools.getNamedChild( kttElement, "Authority" );
    String authority = null;

    if( element != null )
    {
      authority = element.getFirstChild().getNodeValue();
    }

    // Abstract
    element = XMLTools.getNamedChild( kttElement, "Code" );

    String code = null;

    if( element != null )
    {
      code = element.getFirstChild().getNodeValue();
    }

    KnownTransformationType knowntransformationtype = new KnownTransformationType_Impl( authority,
        code );
    Debug.debugMethodEnd();

    return knowntransformationtype;
  }

  /**
   * gets the Knowncoordinatereferencesystemelement
   */
  private static KnownCoordinateReferenceSystem getKnownCoordinateReferenceSystem(
      Element kcrsElement )
  {
    Debug.debugMethodBegin( "WCTSCapabilitiesFactory", "getKnownCoordinateReferenceSystem" );

    // Authority
    Element element = XMLTools.getNamedChild( kcrsElement, "Authority" );
    String authority = null;

    if( element != null )
    {
      authority = element.getFirstChild().getNodeValue();
    }

    // Code
    element = XMLTools.getNamedChild( kcrsElement, "Code" );

    String code = null;

    if( element != null )
    {
      code = element.getFirstChild().getNodeValue();
    }

    KnownCoordinateReferenceSystem knowncoordinatereferencesystem = new KnownCoordinateReferenceSystem_Impl(
        authority, code );
    Debug.debugMethodEnd();

    return knowncoordinatereferencesystem;
  }

  /**
   * gets the GetCapabilitiesElement
   */
  private static ActionType getGetCapabilities( Element getCapabilitiesElement )
  {
    Debug.debugMethodBegin( "WCTSCapabilitiesFactory", "getGetCapabilities" );

    ActionType getcapabilities = getActionType( getCapabilitiesElement );

    Debug.debugMethodEnd();
    return getcapabilities;
  }

  /**
   * gets the TransformElement
   */
  private static ActionType getTransform( Element getTransformElement )
  {
    Debug.debugMethodBegin( "WCTSCapabilitiesFactory", "getTransform" );

    ActionType gettransform = getActionType( getTransformElement );

    Debug.debugMethodEnd();
    return gettransform;
  }

  /**
   * gets the IsTransformableElement
   */
  private static ActionType getIsTransformable( Element getIsTransformableElement )
  {
    Debug.debugMethodBegin( "WCTSCapabilitiesFactory", "getIsTransformable" );

    ActionType getistransformable = getActionType( getIsTransformableElement );

    Debug.debugMethodEnd();
    return getistransformable;
  }

  /**
   * Gets the &lt;getDescribeTransformation&gt;
   */
  private static ActionType getDescribeTransformation( Element getDescribeTransformationElement )
  {
    Debug.debugMethodBegin( "WCTSCapabilitiesFactory", "getDescribeTransformation" );

    ActionType getdescribetransformation = getActionType( getDescribeTransformationElement );

    Debug.debugMethodEnd();
    return getdescribetransformation;
  }

  /**
   * All four elements: <tt>GetCapabilities, Transform,
   * IsTransformable, DescribeTransformation</tt>
   * are built in the same way. This method uses this to summarize the
   * ChildNodes.
   */
  private static ActionType getActionType( Element element )
  {
    Debug.debugMethodBegin( "WCTSCapabilitiesFactory", "getActionType" );

    //Format
    NodeList formatslist = element.getElementsByTagName( "Format" );

    String format = "";
    String[] format_array = new String[formatslist.getLength()];

    for( int i = 0; i < format_array.length; i++ )
    {
      format = formatslist.item( i ).getFirstChild().getNodeValue();
      format_array[i] = format;
    }

    //DCPType
    NodeList nl = element.getElementsByTagName( "DCPType" );
    DCPType[] dCPType = getDCPType( nl );

    ActionType actiontype = new ActionType_Impl( format_array, dCPType );

    Debug.debugMethodEnd();

    return actiontype;
  }

  /**
   * Gets the &lt;getDCPType&gt;
   */
  private static DCPType[] getDCPType( NodeList nl )
  {
    Debug.debugMethodBegin( "WCTSCapabilitiesFactory", "getDCPType" );

    ArrayList list = new ArrayList();

    for( int k = 0; k < nl.getLength(); k++ )
    {
      Element dcpElement = (Element)nl.item( k );
      Element httpElement = (Element)dcpElement.getElementsByTagName( "HTTP" ).item( 0 );

      HTTP http = null;

      try
      {
        NodeList getL = httpElement.getElementsByTagName( "Get" );
        URL[] getOR = new URL[getL.getLength()];

        for( int i = 0; i < getL.getLength(); i++ )
        {
          String u = XMLTools.getAttrValue( XMLTools.getFirstElement( getL.item( i ) ), "href" );
          getOR[i] = new URL( u );
        }

        NodeList postL = httpElement.getElementsByTagName( "Post" );
        URL[] postOR = new URL[postL.getLength()];

        for( int i = 0; i < postL.getLength(); i++ )
        {
          String u = XMLTools.getAttrValue( XMLTools.getFirstElement( postL.item( i ) ), "href" );
          postOR[i] = new URL( u );
        }

        http = new HTTP_Impl( getOR, postOR );
      }
      catch( Exception e )
      {
        System.out.println( "getDCPType: " + e );
      }

      list.add( new DCPType_Impl( http ) );
    }

    Debug.debugMethodEnd();
    return (DCPType[])list.toArray( new DCPType[list.size()] );
  }

}