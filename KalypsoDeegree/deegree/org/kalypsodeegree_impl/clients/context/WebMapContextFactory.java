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
package org.deegree_impl.clients.context;

import java.awt.Rectangle;
import java.io.FileReader;
import java.io.IOException;
import java.io.Reader;
import java.net.URL;

import org.deegree.clients.context.Frontend;
import org.deegree.clients.context.GUIArea;
import org.deegree.clients.context.Module;
import org.deegree.clients.context.ModuleConfiguration;
import org.deegree.graphics.sld.FeatureTypeStyle;
import org.deegree.graphics.sld.StyledLayerDescriptor;
import org.deegree.model.geometry.GM_Point;
import org.deegree.ogcbasic.BaseURL;
import org.deegree.ogcbasic.CommonNamespaces;
import org.deegree.ogcbasic.ContactAddress;
import org.deegree.ogcbasic.ContactInformation;
import org.deegree.ogcbasic.ContactPersonPrimary;
import org.deegree.tools.Parameter;
import org.deegree.tools.ParameterList;
import org.deegree.xml.ElementList;
import org.deegree.xml.XMLParsingException;
import org.deegree.xml.XMLTools;
import org.deegree_impl.clients.wmsclient.configuration.MapOperationFactor;
import org.deegree_impl.graphics.sld.SLDFactory;
import org.deegree_impl.model.cs.Adapters;
import org.deegree_impl.model.cs.ConvenienceCSFactory;
import org.deegree_impl.model.cs.CoordinateSystem;
import org.deegree_impl.model.geometry.GeometryFactory;
import org.deegree_impl.ogcbasic.BaseURL_Impl;
import org.deegree_impl.ogcbasic.ContactAddress_Impl;
import org.deegree_impl.ogcbasic.ContactInformation_Impl;
import org.deegree_impl.ogcbasic.ContactPersonPrimary_Impl;
import org.deegree_impl.ogcbasic.ImageURL;
import org.deegree_impl.tools.Debug;
import org.deegree_impl.tools.ParameterList_Impl;
import org.deegree_impl.tools.Parameter_Impl;
import org.deegree_impl.tools.StringExtend;
import org.opengis.cs.CS_CoordinateSystem;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.Text;
import org.xml.sax.SAXException;

/**
 * Factory class for creating an instance of a web map Context (
 * <tt>ViewContext</tt>). The factory is able to parse deegree specific
 * extensions (General and Layer) as well as standard web map context documents.
 * 
 * @version $Revision$
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 */
public class WebMapContextFactory
{

  /**
   * creates an instance of a ViewContext from the web map context document read
   * from the file idenfied by the passed name
   * 
   * @param fileName
   *          name of a file containing a web map context document
   * 
   * @return @throws
   *         IOException
   * @throws ContextException
   */
  public static ViewContext createViewContext( String fileName ) throws IOException,
      XMLParsingException, ContextException
  {
    Debug.debugMethodBegin();

    Reader reader = new FileReader( fileName );
    ViewContext vc = createViewContext( reader );

    Debug.debugMethodEnd();
    return vc;
  }

  /**
   * creates an instance of a ViewContext from the web map context document read
   * from the passed Reader
   * 
   * @param reader
   *          reader enabling access to a web map context document
   * 
   * @return @throws
   *         IOException
   * @throws ContextException
   */
  public static ViewContext createViewContext( Reader reader ) throws IOException,
      XMLParsingException, ContextException
  {
    Debug.debugMethodBegin();

    Document doc = null;

    try
    {
      doc = XMLTools.parse( reader );
    }
    catch( SAXException se )
    {
      throw new XMLParsingException( "Couldn't create ViewContext", se );
    }

    Node root = doc.getDocumentElement();

    // general section
    Element element = XMLTools.getRequiredChildByName( "General", CommonNamespaces.CNTXTNS, root );
    General general = createGeneral( element );

    //Layer (List) section
    element = XMLTools.getRequiredChildByName( "LayerList", CommonNamespaces.CNTXTNS, root );

    LayerList layerList = createLayerList( element );

    ViewContext vc = new ViewContext( general, layerList );

    Debug.debugMethodEnd();
    return vc;
  }

  /**
   * creates an instance of a class encapsulating the general context
   * informations
   * 
   * @param element
   *          <General>
   * 
   * @return instance of <tt>General</tt>
   * 
   * @throws XMLParsingException
   */
  private static General createGeneral( Element element ) throws XMLParsingException
  {
    Debug.debugMethodBegin();

    // <Window>
    Element elem = XMLTools.getChildByName( "Window", CommonNamespaces.CNTXTNS, element );
    Rectangle rect = createWindow( elem );

    // <BoundingBox>
    elem = XMLTools.getRequiredChildByName( "BoundingBox", CommonNamespaces.CNTXTNS, element );
    GM_Point[] bbox = createBoundingBox( elem );

    // <Title>
    String title = XMLTools.getRequiredStringValue( "Title", CommonNamespaces.CNTXTNS, element );

    // <KeywordList>
    elem = XMLTools.getChildByName( "KeywordList", CommonNamespaces.CNTXTNS, element );
    String[] keywords = createKeywords( elem );

    //<Abstract>
    String abstract_ = XMLTools
        .getStringValue( "Abstract", CommonNamespaces.CNTXTNS, element, null );

    //<LogoURL>
    elem = XMLTools.getChildByName( "LogoURL", CommonNamespaces.CNTXTNS, element );
    ImageURL logoURL = createImageURL( elem );

    //<DescriptionURL>
    elem = XMLTools.getChildByName( "DescriptionURL", CommonNamespaces.CNTXTNS, element );
    BaseURL descriptionURL = createBaseURL( elem );

    // <ContactInformation>
    elem = XMLTools.getChildByName( "ContactInformation", CommonNamespaces.CNTXTNS, element );
    ContactInformation contact = createContactInformation( elem );

    // <Extension>
    elem = XMLTools.getChildByName( "Extension", CommonNamespaces.CNTXTNS, element );
    GeneralExtension extension = createGeneralExtension( elem );

    General general = null;
    try
    {
      general = new General( title, abstract_, rect, contact, bbox, descriptionURL, logoURL,
          keywords, extension );
    }
    catch( Exception e )
    {
      throw new XMLParsingException( "", e );
    }

    Debug.debugMethodEnd();
    return general;
  }

  /**
   * creates a <tt>Rectangle<tt> (Window) instance from the passed Element.
   *
   * @param element <Window>
   *
   * @return instance of <tt>Rectangle</tt>
   *
   * @throws XMLParsingException
   */
  private static Rectangle createWindow( Element element ) throws XMLParsingException
  {
    Debug.debugMethodBegin();

    Rectangle rect = null;

    if( element != null )
    {
      String tmp = XMLTools.getRequiredAttrValue( "width", element );
      int width = Integer.parseInt( tmp );
      tmp = XMLTools.getRequiredAttrValue( "height", element );

      int height = Integer.parseInt( tmp );
      rect = new Rectangle( width, height );
    }

    Debug.debugMethodEnd();
    return rect;
  }

  /**
   * creates a <tt>GM_Envelope </tt> from the passed Element
   * 
   * @param element
   *          <BoundingBox>
   * 
   * @return instance of <tt>GM_Envelope</tt>
   * 
   * @throws XMLParsingException
   */
  private static GM_Point[] createBoundingBox( Element element ) throws XMLParsingException
  {
    Debug.debugMethodBegin();

    String srs = XMLTools.getRequiredAttrValue( "SRS", element );
    CoordinateSystem cs = ConvenienceCSFactory.getInstance().getCSByName( srs );
    Adapters adapter = Adapters.getDefault();
    CS_CoordinateSystem crs = adapter.export( cs );
    String tmp = XMLTools.getRequiredAttrValue( "minx", element );
    double minx = Double.parseDouble( tmp );
    tmp = XMLTools.getRequiredAttrValue( "miny", element );

    double miny = Double.parseDouble( tmp );
    tmp = XMLTools.getRequiredAttrValue( "maxx", element );

    double maxx = Double.parseDouble( tmp );
    tmp = XMLTools.getRequiredAttrValue( "maxy", element );

    double maxy = Double.parseDouble( tmp );

    GM_Point[] points = new GM_Point[2];
    points[0] = GeometryFactory.createGM_Point( minx, miny, crs );
    points[1] = GeometryFactory.createGM_Point( maxx, maxy, crs );

    Debug.debugMethodEnd();
    return points;
  }

  /**
   * creates an array of keywords (String) from the passed Keyword list
   * 
   * @param element
   *          <KeywordList>
   * 
   * @return array of Strings
   * 
   * @throws XMLParsingException
   */
  private static String[] createKeywords( Element element ) throws XMLParsingException
  {
    Debug.debugMethodBegin();

    ElementList el = XMLTools.getChildElementsByName( "Keyword", CommonNamespaces.CNTXTNS, element );
    String[] keywords = new String[el.getLength()];

    for( int i = 0; i < keywords.length; i++ )
    {
      keywords[i] = XMLTools.getStringValue( el.item( i ) );
    }

    Debug.debugMethodEnd();
    return keywords;
  }

  /**
   * creates an instance of an ImageURL that is used for <LogoURL>and LegendURL
   * 
   * @param element
   *          <LogoURL>or <LegendURL>
   * 
   * @return instance of <tt>ImageURL</tt>
   * 
   * @throws XMLParsingException
   */
  private static ImageURL createImageURL( Element element ) throws XMLParsingException
  {
    Debug.debugMethodBegin();

    ImageURL imageURL = null;

    if( element != null )
    {
      String tmp = XMLTools.getAttrValue( element, "width" );
      int width = -1;
      if( tmp != null )
      {
        width = Integer.parseInt( tmp );
      }
      tmp = XMLTools.getAttrValue( element, "height" );
      int height = -1;
      if( tmp != null )
      {
        height = Integer.parseInt( tmp );
      }
      String format = XMLTools.getAttrValue( element, "format" );

      Element elem = XMLTools.getRequiredChildByName( "OnlineResource", CommonNamespaces.CNTXTNS,
          element );
      URL onlineResource = createOnlineResource( elem );

      imageURL = new ImageURL( width, height, format, onlineResource );
    }

    Debug.debugMethodEnd();
    return imageURL;
  }

  /**
   * creates an instance of an URL described by a <OnlineResource>element
   * 
   * @param element
   *          <OnlineResource>
   * 
   * @return instance of <tt>URL</tt>
   * 
   * @throws XMLParsingException
   */
  private static URL createOnlineResource( Element element ) throws XMLParsingException
  {
    Debug.debugMethodBegin();

    URL onlineResource = null;

    if( element != null )
    {
      String type = XMLTools.getAttrValue( element, "type" );

      if( type == null )
      {
        type = XMLTools.getAttrValue( element, CommonNamespaces.XLNNS, "type" );
      }

      if( ( type != null ) && !type.equals( "simple" ) )
      {
        throw new XMLParsingException( "unknown type of online resource: " + type );
      }

      String tmp = XMLTools.getAttrValue( element, "href" );

      if( tmp == null )
      {
        tmp = XMLTools.getAttrValue( element, CommonNamespaces.XLNNS, "href" );
      }

      try
      {
        onlineResource = new URL( tmp );
      }
      catch( Exception e )
      {
        throw new XMLParsingException( "couldn't create online resource", e );
      }
    }

    Debug.debugMethodEnd();
    return onlineResource;
  }

  /**
   * creates an instance of BaseURL that is used for <DescriptionURL>
   * 
   * @param element
   *          <DescriptionURL>
   * 
   * @return instance of <tt>BaseURL</tt>
   * 
   * @throws XMLParsingException
   */
  private static BaseURL createBaseURL( Element element ) throws XMLParsingException
  {
    Debug.debugMethodBegin();

    BaseURL baseURL = null;

    if( element != null )
    {
      String format = XMLTools.getAttrValue( element, "format" );

      Element elem = XMLTools.getRequiredChildByName( "OnlineResource", CommonNamespaces.CNTXTNS,
          element );
      URL onlineResource = createOnlineResource( elem );

      baseURL = new BaseURL_Impl( format, onlineResource );
    }

    Debug.debugMethodEnd();
    return baseURL;
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
  private static ContactInformation createContactInformation( Element element )
      throws XMLParsingException
  {
    Debug.debugMethodBegin();

    ContactInformation contact = null;

    if( element != null )
    {
      // optional: <ContactPersonPrimary>
      ContactPersonPrimary contactPersonPrimary = null;
      Element contactPersonPrimaryElement = XMLTools.getChildByName( "ContactPersonPrimary",
          CommonNamespaces.CNTXTNS, element );

      if( contactPersonPrimaryElement != null )
      {
        contactPersonPrimary = createContactPersonPrimary( contactPersonPrimaryElement );
      }

      // optional: <ContactPosition>
      String contactPosition = XMLTools.getStringValue( "ContactPosition",
          CommonNamespaces.CNTXTNS, element, null );

      // optional: <ContactAddress>
      ContactAddress contactAddress = null;
      Element contactAddressElement = XMLTools.getChildByName( "ContactAddress",
          CommonNamespaces.CNTXTNS, element );

      if( contactAddressElement != null )
      {
        contactAddress = createContactAddress( contactAddressElement );
      }

      // optional: <ContactVoiceTelephone>
      String contactVoiceTelephone = XMLTools.getStringValue( "ContactVoiceTelephone",
          CommonNamespaces.CNTXTNS, element, null );

      // optional: <ContactFacsimileTelephone>
      String contactFacsimileTelephone = XMLTools.getStringValue( "ContactFacsimileTelephone",
          CommonNamespaces.CNTXTNS, element, null );

      // optional: <ContactElectronicMailAddress>
      String contactElectronicMailAddress = XMLTools.getStringValue(
          "ContactElectronicMailAddress", CommonNamespaces.CNTXTNS, element, null );

      contact = new ContactInformation_Impl( contactPosition, contactVoiceTelephone,
          contactFacsimileTelephone, contactElectronicMailAddress, contactPersonPrimary,
          contactAddress );
    }

    Debug.debugMethodEnd();
    return contact;
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
  private static ContactPersonPrimary createContactPersonPrimary( Element element )
      throws XMLParsingException
  {
    Debug.debugMethodBegin();
    // required: <ContactPerson>
    String contactPerson = XMLTools.getRequiredStringValue( "ContactPerson",
        CommonNamespaces.CNTXTNS, element );

    // required: <ContactOrganization>
    String contactOrganization = XMLTools.getRequiredStringValue( "ContactOrganization",
        CommonNamespaces.CNTXTNS, element );

    Debug.debugMethodEnd();
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
  private static ContactAddress createContactAddress( Element element ) throws XMLParsingException
  {
    Debug.debugMethodBegin();
    // required: <AddressType>
    String addressType = XMLTools.getRequiredStringValue( "AddressType", CommonNamespaces.CNTXTNS,
        element );

    // required: <Address>
    String address = XMLTools.getRequiredStringValue( "Address", CommonNamespaces.CNTXTNS, element );

    // required: <City>
    String city = XMLTools.getRequiredStringValue( "City", CommonNamespaces.CNTXTNS, element );

    // required: <StateOrProvince>
    String stateOrProvince = XMLTools.getRequiredStringValue( "StateOrProvince",
        CommonNamespaces.CNTXTNS, element );

    // required: <PostCode>
    String postCode = XMLTools.getRequiredStringValue( "PostCode", CommonNamespaces.CNTXTNS,
        element );

    // required: <Country>
    String country = XMLTools.getRequiredStringValue( "Country", CommonNamespaces.CNTXTNS, element );

    Debug.debugMethodEnd();
    return new ContactAddress_Impl( addressType, address, city, stateOrProvince, postCode, country );
  }

  /**
   * creates an instance of a class encapsulating the deegree specific
   * extensions of the general section of a web map context document
   * 
   * @param element
   *          <Extension>
   * 
   * @return instance of <tt>GeneralExtension</tt>
   * 
   * @throws XMLParsingException
   */
  private static GeneralExtension createGeneralExtension( Element element )
      throws XMLParsingException
  {
    Debug.debugMethodBegin();

    GeneralExtension ge = null;

    if( element != null )
    {
      // <IOSetiings>
      Element elem = XMLTools.getRequiredChildByName( "IOSettings", CommonNamespaces.DGCNTXTNS,
          element );
      IOSettings ioSettings = createIOSettings( elem );
      // <Frontend>
      elem = XMLTools.getRequiredChildByName( "Frontend", CommonNamespaces.DGCNTXTNS, element );
      Frontend frontend = createFrontend( elem );
      // <MapParameter>
      elem = XMLTools.getRequiredChildByName( "MapParameter", CommonNamespaces.DGCNTXTNS, element );
      MapParameter mapParameter = createMapParameter( elem );

      ge = new GeneralExtension( ioSettings, frontend, mapParameter );
    }

    Debug.debugMethodEnd();
    return ge;
  }

  /**
   * creates an instance of a class encapsulating the frontend (GUI) description
   * of a deegree map client
   * 
   * @param element
   *          <Frontend>
   * 
   * @return instance of <tt>Frontend</tt>
   * 
   * @throws XMLParsingException
   */
  private static Frontend createFrontend( Element element ) throws XMLParsingException
  {
    Debug.debugMethodBegin();

    String scope = XMLTools.getRequiredAttrValue( "scope", null, element );
    // <Controller>
    String controller = XMLTools.getRequiredStringValue( "Controller", CommonNamespaces.DGCNTXTNS,
        element );
    // <Style>
    String style = XMLTools.getStringValue( "Style", CommonNamespaces.DGCNTXTNS, element, null );
    // <Buttons>
    String buttons = XMLTools.getStringValue( "Buttons", CommonNamespaces.DGCNTXTNS, element, null );
    // <CommonJS>
    Element elem = XMLTools.getChildByName( "CommonJS", CommonNamespaces.DGCNTXTNS, element );
    String[] commonJS = createCommonJS( elem );
    // <West>
    elem = XMLTools.getChildByName( "West", CommonNamespaces.DGCNTXTNS, element );
    GUIArea west = createGUIArea( elem );
    // <East>
    elem = XMLTools.getChildByName( "East", CommonNamespaces.DGCNTXTNS, element );
    GUIArea east = createGUIArea( elem );
    // <North>
    elem = XMLTools.getChildByName( "North", CommonNamespaces.DGCNTXTNS, element );
    GUIArea north = createGUIArea( elem );
    // <South>
    elem = XMLTools.getChildByName( "South", CommonNamespaces.DGCNTXTNS, element );
    GUIArea south = createGUIArea( elem );
    // <Center>
    elem = XMLTools.getChildByName( "Center", CommonNamespaces.DGCNTXTNS, element );
    GUIArea center = createGUIArea( elem );
    // <Header>
    String header = XMLTools.getStringValue( "Header", CommonNamespaces.DGCNTXTNS, element, null );
    // <Footer>
    String footer = XMLTools.getStringValue( "Footer", CommonNamespaces.DGCNTXTNS, element, null );

    Frontend frontend = new JSPFrontend( controller, west, east, south, north, center, style,
        buttons, commonJS, header, footer );

    Debug.debugMethodEnd();
    return frontend;
  }

  /**
   * creates a list of javascript pages (names) that contains javascript objects
   * and methods that are used by more than one module
   * 
   * @param element
   *          <CommonJS>
   * 
   * @return instance of <tt>String[]</tt>
   * 
   * @throws XMLParsingException
   */
  private static String[] createCommonJS( Element element ) throws XMLParsingException
  {
    Debug.debugMethodBegin();

    String[] commonJS = null;
    if( element != null )
    {
      ElementList el = XMLTools
          .getChildElementsByName( "Name", CommonNamespaces.DGCNTXTNS, element );
      commonJS = new String[el.getLength()];
      for( int i = 0; i < commonJS.length; i++ )
      {
        commonJS[i] = XMLTools.getStringValue( el.item( i ) );
      }
    }

    Debug.debugMethodEnd();
    return commonJS;
  }

  /**
   * creates an instance of a class encapsulating the GUI description of one
   * region of the GUI
   * 
   * @param element
   *          <West>; <East>; <South>; <North>or <Center>
   * 
   * @return instance of <tt>GUIArea</tt>
   * 
   * @throws XMLParsingException
   */
  private static GUIArea createGUIArea( Element element ) throws XMLParsingException
  {
    Debug.debugMethodBegin();

    GUIArea gui = null;
    if( element != null )
    {
      String tmp = XMLTools.toLocalName( element.getNodeName() );
      int area = 0;
      if( tmp.equals( "West" ) )
      {
        area = GUIArea.WEST;
      }
      else if( tmp.equals( "East" ) )
      {
        area = GUIArea.EAST;
      }
      else if( tmp.equals( "South" ) )
      {
        area = GUIArea.SOUTH;
      }
      else if( tmp.equals( "North" ) )
      {
        area = GUIArea.NORTH;
      }
      else if( tmp.equals( "Center" ) )
      {
        area = GUIArea.CENTER;
      }

      // hidden
      tmp = XMLTools.getAttrValue( element, "hidden" );
      boolean hidden = "1".equals( tmp ) || "true".equals( tmp );
      // <Module>
      ElementList el = XMLTools.getChildElementsByName( "Module", CommonNamespaces.DGCNTXTNS,
          element );
      Module[] modules = new Module[el.getLength()];
      for( int i = 0; i < modules.length; i++ )
      {
        modules[i] = createModule( el.item( i ) );
      }
      gui = new GUIArea_Impl( area, hidden, modules );
    }

    Debug.debugMethodEnd();
    return gui;
  }

  /**
   * creates an instance of a class encapsulating module informations
   * 
   * @param element
   *          <Module>
   * 
   * @return instance of <tt>Module</tt>
   * 
   * @throws XMLParsingException
   */
  private static Module createModule( Element element ) throws XMLParsingException
  {
    Debug.debugMethodBegin();

    // hidden
    String tmp = XMLTools.getAttrValue( element, "hidden" );
    boolean hidden = tmp.equals( "1" ) || tmp.equals( "true" );
    // <Name>
    String name = XMLTools.getRequiredStringValue( "Name", CommonNamespaces.DGCNTXTNS, element );
    // <Content>
    String content = XMLTools.getRequiredStringValue( "Content", CommonNamespaces.DGCNTXTNS,
        element );
    // <ModuleConfiguration>
    Element elem = XMLTools.getChildByName( "ModuleConfiguration", CommonNamespaces.DGCNTXTNS,
        element );
    ModuleConfiguration mc = createModuleConfiguration( elem );
    // <ParameterList>
    elem = XMLTools.getChildByName( "ParameterList", CommonNamespaces.DGCNTXTNS, element );
    ParameterList paramList = createParameterList( elem );

    String type = XMLTools.getAttrValue( element, "type" );

    // width and height of a Module are optional
    // if not set '0' will be used instead
    tmp = XMLTools.getAttrValue( element, "width" );
    int w = 0;
    try
    {
      w = Integer.parseInt( tmp );
    }
    catch( Exception e )
    {}
    tmp = XMLTools.getAttrValue( element, "height" );
    int h = 0;
    try
    {
      h = Integer.parseInt( tmp );
    }
    catch( Exception e )
    {}

    String scrollable = XMLTools.getAttrValue( element, "scrolling" );

    String[] moduleJS = createModuleJSList( element );
    Module module = new Module_Impl( name, content, hidden, type, w, h, scrollable, moduleJS, mc,
        paramList );

    Debug.debugMethodEnd();
    return module;
  }

  /**
   * creates an instance of a class encapsulating the access the configuration
   * of Module
   * 
   * @param element
   *          <ModuleConfiguration>
   * 
   * @return instance of <tt>ModuleConfiguration</tt>
   * 
   * @throws XMLParsingException
   */
  private static ModuleConfiguration createModuleConfiguration( Element element )
      throws XMLParsingException
  {
    Debug.debugMethodBegin();

    ModuleConfiguration mc = null;
    if( element != null )
    {
      Element elem = XMLTools.getRequiredChildByName( "OnlineResource", CommonNamespaces.DGCNTXTNS,
          element );
      URL onlineResource = createOnlineResource( elem );
      mc = new ModuleConfiguration_Impl( onlineResource );
    }

    Debug.debugMethodEnd();
    return mc;
  }

  /**
   * creates an instance of a class encapsulating the layer list informations
   * 
   * @param element
   *          <LayerList>
   * 
   * @return instance of <tt>LayerList</tt>
   * 
   * @throws XMLParsingException
   */
  private static ParameterList createParameterList( Element element ) throws XMLParsingException
  {
    Debug.debugMethodBegin();

    ParameterList parameterList = new ParameterList_Impl();
    if( element != null )
    {
      ElementList el = XMLTools.getChildElementsByName( "Parameter", CommonNamespaces.DGCNTXTNS,
          element );
      for( int i = 0; i < el.getLength(); i++ )
      {
        Parameter parameter = createParameter( el.item( i ) );
        parameterList.addParameter( parameter );
      }
    }
    Debug.debugMethodEnd();
    return parameterList;
  }

  /**
   * creates an instance of a class encapsulating a parameter that shall be
   * passed to a module
   * 
   * @param element
   *          <Parameter>
   * 
   * @return instance of <tt>Parameter</tt>
   * 
   * @throws XMLParsingException
   */
  private static Parameter createParameter( Element element ) throws XMLParsingException
  {
    Debug.debugMethodBegin();

    String name = XMLTools.getRequiredStringValue( "Name", CommonNamespaces.DGCNTXTNS, element );
    String value = XMLTools.getRequiredStringValue( "Value", CommonNamespaces.DGCNTXTNS, element );
    //Parameter param = new Parameter_Impl( name+":"+value, value );
    Parameter param = new Parameter_Impl( name, value );

    Debug.debugMethodEnd();
    return param;
  }

  /**
   * creates an instance of a class encapsulating informations about controlling
   * options for a map presented to the user
   * 
   * @param element
   *          <MapParameter>
   * 
   * @return instance of <tt>MapParameter</tt>
   * 
   * @throws XMLParsingException
   */
  private static MapParameter createMapParameter( Element element ) throws XMLParsingException
  {
    Debug.debugMethodBegin();

    //<OfferedInfoFormats>
    Element elem = XMLTools.getRequiredChildByName( "OfferedInfoFormats",
        CommonNamespaces.DGCNTXTNS, element );
    Format[] infoFormats = createOfferedInfoFormats( elem );
    //<OfferedZoomFactor>
    elem = XMLTools.getRequiredChildByName( "OfferedZoomFactor", CommonNamespaces.DGCNTXTNS,
        element );
    MapOperationFactor[] zoomFactors = createOfferedMapOperationFactors( elem );
    //<OfferedPanFactor>
    elem = XMLTools
        .getRequiredChildByName( "OfferedPanFactor", CommonNamespaces.DGCNTXTNS, element );
    MapOperationFactor[] panFactors = createOfferedMapOperationFactors( elem );
    // <MinScale>
    String tmp = XMLTools.getRequiredStringValue( "MinScale", CommonNamespaces.DGCNTXTNS, element );
    double minScale = Double.parseDouble( tmp );
    // <MaxScale>
    tmp = XMLTools.getRequiredStringValue( "MaxScale", CommonNamespaces.DGCNTXTNS, element );
    double maxScale = Double.parseDouble( tmp );

    MapParameter mp = new MapParameter( infoFormats, panFactors, zoomFactors, minScale, maxScale );

    Debug.debugMethodEnd();
    return mp;
  }

  /**
   * Creates a list of the feature info formats offered by the client.
   * 
   * @param element
   *          <OfferedInfoFormats>element of the configuration
   * 
   * @return list of offered feature info formats
   * 
   * @throws XMLParsingException
   */
  private static Format[] createOfferedInfoFormats( Element element ) throws XMLParsingException
  {
    Debug.debugMethodBegin();

    Format[] format = null;

    // get list of offered feature info formats
    ElementList el = XMLTools
        .getChildElementsByName( "Format", CommonNamespaces.DGCNTXTNS, element );

    format = new Format[el.getLength()];

    for( int i = 0; i < el.getLength(); i++ )
    {
      String name = XMLTools.getStringValue( el.item( i ) );
      String sel = XMLTools.getAttrValue( "selected", el.item( i ) );

      boolean selected = "1".equals( sel ) || "true".equals( sel );
      try
      {
        format[i] = new Format( name, selected );
      }
      catch( ContextException e )
      {
        throw new XMLParsingException( "", e );
      }
    }

    Debug.debugMethodEnd();
    return format;
  }

  /**
   * returns a list of offered numerical map operation factors that can be used
   * to determine zoom or pan levels
   * 
   * @param element
   *          a <tt>Element</tt> that contains <Factor>elements as children
   * 
   * @return list of <tt>MapOperationFactor</tt> s
   * 
   * @throws XMLParsingException
   */
  private static MapOperationFactor[] createOfferedMapOperationFactors( Element element )
      throws XMLParsingException
  {
    Debug.debugMethodBegin();

    // get list of offered factors
    ElementList el = XMLTools
        .getChildElementsByName( "Factor", CommonNamespaces.DGCNTXTNS, element );

    MapOperationFactor[] mof = new MapOperationFactor[el.getLength()];

    for( int i = 0; i < el.getLength(); i++ )
    {
      boolean free = true;
      String tmp = XMLTools.getStringValue( el.item( i ) );
      double fac = -99;

      if( !tmp.equals( "*" ) )
      {
        free = false;
        fac = Double.parseDouble( tmp );
      }

      String sel = XMLTools.getAttrValue( "selected", el.item( i ) );
      boolean selected = "1".equals( sel ) || "true".equals( sel );
      mof[i] = new MapOperationFactor( fac, selected, free );
    }

    Debug.debugMethodEnd();
    return mof;
  }

  /**
   * creates an instance of a class encapsulating the IO setting informations
   * 
   * @param element
   * @return @throws
   *         XMLParsingException
   */
  private static IOSettings createIOSettings( Element element ) throws XMLParsingException
  {
    Debug.debugMethodBegin();

    String root = XMLTools.getRequiredStringValue( "RootDirectory", CommonNamespaces.DGCNTXTNS,
        element );
    // temp directory
    Element elem = XMLTools.getChildByName( "TempDirectory", CommonNamespaces.DGCNTXTNS, element );
    DirectoryAccess temp = null;
    if( elem != null )
    {
      temp = createDirectoryAccess( elem, null );
    }
    // download directory
    elem = XMLTools.getChildByName( "DownloadDirectory", CommonNamespaces.DGCNTXTNS, element );
    DirectoryAccess download = null;
    if( elem != null )
    {
      download = createDirectoryAccess( elem, temp );
    }
    if( temp == null && elem == null )
    {
      throw new XMLParsingException( "If <TempDirectory> isn't set, "
          + "downloaddirectory must be set!" );
    }
    // SLD directory
    elem = XMLTools.getChildByName( "SLDDirectory", CommonNamespaces.DGCNTXTNS, element );
    DirectoryAccess sld = null;
    if( elem != null )
    {
      sld = createDirectoryAccess( elem, temp );
    }
    if( temp == null && elem == null )
    {
      throw new XMLParsingException( "If <TempDirectory> isn't set, " + "slddirectory must be set!" );
    }
    // Print directory
    elem = XMLTools.getChildByName( "PrintDirectory", CommonNamespaces.DGCNTXTNS, element );
    DirectoryAccess print = null;
    if( elem != null )
    {
      print = createDirectoryAccess( elem, temp );
    }
    if( temp == null && elem == null )
    {
      throw new XMLParsingException( "If <TempDirectory> isn't set, "
          + "printdirectory must be set!" );
    }

    IOSettings ioSettings = new IOSettings( root, download, sld, print );

    Debug.debugMethodEnd();
    return ioSettings;
  }

  /**
   * @param element
   * @param tempDir
   * @return @throws
   *         XMLParsingException
   */
  private static DirectoryAccess createDirectoryAccess( Element element, DirectoryAccess tempDir )
      throws XMLParsingException
  {
    Debug.debugMethodBegin();

    // 	directory name
    String name = XMLTools.getStringValue( "Name", CommonNamespaces.DGCNTXTNS, element, null );

    URL url = null;
    Element elem = XMLTools.getChildByName( "Access", CommonNamespaces.DGCNTXTNS, element );
    if( elem != null )
    {
      elem = XMLTools.getRequiredChildByName( "OnlineResource", CommonNamespaces.CNTXTNS, elem );
      url = createOnlineResource( elem );
    }

    DirectoryAccess da = null;
    if( name == null || url == null )
    {
      da = tempDir;
    }
    da = new DirectoryAccess( name, url );

    Debug.debugMethodEnd();
    return da;

  }

  /**
   * creates an instance of a class encapsulating the layer list informations
   * 
   * @param element
   *          <LayerList>
   * 
   * @return instance of <tt>LayerList</tt>
   * 
   * @throws XMLParsingException
   */
  private static LayerList createLayerList( Element element ) throws XMLParsingException
  {
    Debug.debugMethodBegin();

    ElementList el = XMLTools.getChildElementsByName( "Layer", CommonNamespaces.CNTXTNS, element );
    Layer[] layers = new Layer[el.getLength()];
    for( int i = 0; i < layers.length; i++ )
    {
      layers[i] = createLayer( el.item( i ) );
    }
    LayerList list = new LayerList( layers );

    Debug.debugMethodEnd();
    return list;
  }

  /**
   * creates an instance of a class encapsulating a web map context layer's
   * attributes
   * 
   * @param element
   *          <Layer>
   * 
   * @return instance of <tt>Layer</tt>
   * 
   * @throws XMLParsingException
   */
  private static Layer createLayer( Element element ) throws XMLParsingException
  {
    Debug.debugMethodBegin();

    String tmp = XMLTools.getRequiredAttrValue( "queryable", element );
    boolean queryable = "1".equals( tmp ) || "true".equals( tmp );
    tmp = XMLTools.getRequiredAttrValue( "hidden", element );
    boolean hidden = "1".equals( tmp ) || "true".equals( tmp );
    // <Server>
    Element elem = XMLTools.getRequiredChildByName( "Server", CommonNamespaces.CNTXTNS, element );
    Server server = createServer( elem );
    // <Name>
    String name = XMLTools.getRequiredStringValue( "Name", CommonNamespaces.CNTXTNS, element );
    // <Title>
    String title = XMLTools.getRequiredStringValue( "Title", CommonNamespaces.CNTXTNS, element );
    // <Abstract>
    String abstract_ = XMLTools
        .getStringValue( "Abstract", CommonNamespaces.CNTXTNS, element, null );
    // <DataURL>
    elem = XMLTools.getChildByName( "DataURL", CommonNamespaces.CNTXTNS, element );
    BaseURL dataURL = createBaseURL( elem );
    // <MetaDataURL>
    elem = XMLTools.getChildByName( "MetaDataURL", CommonNamespaces.CNTXTNS, element );
    BaseURL metadataURL = createBaseURL( elem );
    // <SRS>
    tmp = XMLTools.getStringValue( "SRS", CommonNamespaces.CNTXTNS, element, null );
    String[] srs = StringExtend.toArray( tmp, ",; ", true );
    // <FormatList>
    elem = XMLTools.getChildByName( "FormatList", CommonNamespaces.CNTXTNS, element );
    FormatList formatList = createFormatList( elem );
    // <StyleList>
    elem = XMLTools.getChildByName( "StyleList", CommonNamespaces.CNTXTNS, element );
    StyleList styleList = createStyleList( elem );
    // <Extension>
    elem = XMLTools.getChildByName( "Extension", CommonNamespaces.CNTXTNS, element );
    LayerExtension extension = createLayerExtension( elem );

    Layer layer = null;
    try
    {
      layer = new Layer( server, name, title, abstract_, srs, dataURL, metadataURL, formatList,
          styleList, queryable, hidden, extension );
    }
    catch( Exception e )
    {
      throw new XMLParsingException( "couldn't create map context layer", e );
    }

    Debug.debugMethodEnd();
    return layer;
  }

  /**
   * creates an instance of a class encapsulating informations about the server
   * (service) a layer based on
   * 
   * @param element
   *          <Server>
   * 
   * @return instance of <tt>Server</tt>
   * 
   * @throws XMLParsingException
   */
  private static Server createServer( Element element ) throws XMLParsingException
  {
    Debug.debugMethodBegin();

    String service = XMLTools.getRequiredAttrValue( "service", element );
    String version = XMLTools.getRequiredAttrValue( "version", element );
    String title = XMLTools.getRequiredAttrValue( "title", element );
    // <OnlineResource>
    Element elem = XMLTools.getRequiredChildByName( "OnlineResource", CommonNamespaces.CNTXTNS,
        element );
    URL onlineResource = createOnlineResource( elem );

    Server server = null;
    try
    {
      server = new Server( title, version, service, onlineResource );
    }
    catch( Exception e )
    {
      throw new XMLParsingException( "could not create context server", e );
    }

    Debug.debugMethodEnd();
    return server;
  }

  /**
   * creates an instance of a class encapsulating a list of image formats a
   * layer offers
   * 
   * @param element
   *          <FormatList>
   * 
   * @return instance of <tt>FormatList</tt>
   * 
   * @throws XMLParsingException
   */
  private static FormatList createFormatList( Element element ) throws XMLParsingException
  {
    Debug.debugMethodBegin();

    ElementList el = XMLTools.getChildElementsByName( "Format", CommonNamespaces.CNTXTNS, element );
    Format[] formats = new Format[el.getLength()];
    for( int i = 0; i < formats.length; i++ )
    {
      String name = XMLTools.getStringValue( el.item( i ) );
      String tmp = XMLTools.getAttrValue( element, "current" );
      boolean current = "1".equals( tmp ) || "true".equals( tmp ) || formats.length == 1;
      try
      {
        formats[i] = new Format( name, current );
      }
      catch( Exception e )
      {
        throw new XMLParsingException( "could not create context format", e );
      }
    }

    FormatList formatList = null;
    try
    {
      formatList = new FormatList( formats );
    }
    catch( Exception e )
    {
      throw new XMLParsingException( "could not create context formatList", e );
    }

    Debug.debugMethodEnd();
    return formatList;
  }

  /**
   * creates an instance of a class encapsulating a list of styles available for
   * a layer
   * 
   * @param element
   *          <StyleList>
   * 
   * @return instance of <tt>StyleList</tt>
   * 
   * @throws XMLParsingException
   */
  private static StyleList createStyleList( Element element ) throws XMLParsingException
  {
    Debug.debugMethodBegin();

    ElementList el = XMLTools.getChildElementsByName( "Style", CommonNamespaces.CNTXTNS, element );
    Style[] styles = new Style[el.getLength()];
    for( int i = 0; i < styles.length; i++ )
    {
      styles[i] = createStyle( el.item( i ) );
    }
    StyleList styleList = null;
    try
    {
      styleList = new StyleList( styles );
    }
    catch( Exception e )
    {
      throw new XMLParsingException( "could not create context stylelist", e );
    }

    Debug.debugMethodEnd();
    return styleList;
  }

  /**
   * creates an instance of a class encapsulating a description of a Style
   * 
   * @param element
   *          <Style>
   * 
   * @return instance of <tt>Style</tt>
   * 
   * @throws XMLParsingException
   */
  private static Style createStyle( Element element ) throws XMLParsingException
  {
    Debug.debugMethodBegin();

    Style style = null;

    String tmp = XMLTools.getAttrValue( element, "current" );
    boolean current = "1".equals( tmp ) || "true".equals( tmp );

    Element elem = XMLTools.getChildByName( "SLD", CommonNamespaces.CNTXTNS, element );
    if( elem != null )
    {
      SLD sld = createSLD( elem );
      try
      {
        style = new Style( sld, current );
      }
      catch( Exception e )
      {
        throw new XMLParsingException( "could not create context style", e );
      }
    }
    else
    {
      String name = XMLTools.getRequiredStringValue( "Name", CommonNamespaces.CNTXTNS, element );
      String title = XMLTools.getRequiredStringValue( "Title", CommonNamespaces.CNTXTNS, element );
      String abstract_ = XMLTools.getStringValue( "Abstract", CommonNamespaces.CNTXTNS, element,
          null );
      // <LegendURL>
      elem = XMLTools.getChildByName( "LegendURL", CommonNamespaces.CNTXTNS, element );
      ImageURL legendURL = createImageURL( elem );
      try
      {
        style = new Style( name, title, abstract_, legendURL, current );
      }
      catch( Exception e )
      {
        throw new XMLParsingException( "could not create context style", e );
      }
    }

    Debug.debugMethodEnd();
    return style;
  }

  /**
   * creates an instance of a class encapsulating a description of a Style based
   * on a SLD
   * 
   * @param element
   *          <SLD>
   * 
   * @return instance of <tt>SLD</tt>
   * 
   * @throws XMLParsingException
   */
  private static SLD createSLD( Element element ) throws XMLParsingException
  {
    Debug.debugMethodBegin();

    SLD sld = null;

    String name = XMLTools.getRequiredStringValue( "Name", CommonNamespaces.CNTXTNS, element );
    String title = XMLTools.getStringValue( "Title", CommonNamespaces.CNTXTNS, element, null );

    Element elem = XMLTools.getChildByName( "OnlineResource", CommonNamespaces.CNTXTNS, element );
    try
    {
      if( elem != null )
      {
        URL onlineResource = createOnlineResource( elem );
        sld = new SLD( name, title, onlineResource );
      }
      else
      {
        elem = XMLTools.getChildByName( "StyledLayerDescriptor", CommonNamespaces.SLDNS, element );
        if( elem != null )
        {
          StyledLayerDescriptor styledLayerDescriptor = SLDFactory
              .createStyledLayerDescriptor( elem );
          sld = new SLD( name, title, styledLayerDescriptor );
        }
        else
        {
          FeatureTypeStyle fts = SLDFactory.createFeatureTypeStyle( elem );
          sld = new SLD( name, title, fts );
        }
      }
    }
    catch( Exception e )
    {
      throw new XMLParsingException( "couldn't create map context SLD", e );
    }

    Debug.debugMethodEnd();
    return sld;
  }

  /**
   * creates an instance of a class encapsulating the deegree specific
   * extensions of a Layer
   * 
   * @param element
   *          <Extension>
   * 
   * @return instance of <tt>LayerExtension</tt>
   * 
   * @throws XMLParsingException
   */
  private static LayerExtension createLayerExtension( Element element ) throws XMLParsingException
  {
    Debug.debugMethodBegin();

    LayerExtension le = null;
    if( element != null )
    {

      DataService dataService = null;
      Element elem = XMLTools.getChildByName( "DataService", CommonNamespaces.DGCNTXTNS, element );
      if( elem != null )
      {
        Element el = XMLTools.getRequiredChildByName( "Server", CommonNamespaces.CNTXTNS, elem );
        Server server = createServer( el );
        String geoType = XMLTools.getStringValue( "GeometryType", CommonNamespaces.DGCNTXTNS, elem,
            null );
        String featureType = XMLTools.getStringValue( "FeatureType", CommonNamespaces.DGCNTXTNS,
            elem, null );
        dataService = new DataService( server, featureType, geoType );
      }
      boolean masterLayer = false;
      elem = XMLTools.getChildByName( "MasterLayer", CommonNamespaces.DGCNTXTNS, element );
      if( elem != null )
      {
        String s = XMLTools.getStringValue( elem );
        masterLayer = "true".equals( s ) || "1".equals( s );
      }
      le = new LayerExtension( dataService, masterLayer );
    }

    Debug.debugMethodEnd();
    return le;
  }

  /**
   * creates a list (String[]) containing the name of the JavaScript files used
   * by the moudle
   * 
   * @param element
   *          <Module>
   * 
   * @return instance of <tt>String[]</tt>
   * 
   * @throws XMLParsingException
   */
  private static String[] createModuleJSList( Element element ) throws XMLParsingException
  {
    Debug.debugMethodBegin();

    String[] moduleJS = null;
    if( element != null )
    {
      ElementList el = XMLTools.getChildElementsByName( "ModuleJS", CommonNamespaces.DGCNTXTNS,
          element );
      moduleJS = new String[el.getLength()];
      for( int i = 0; i < el.getLength(); i++ )
      {
        moduleJS[i] = ( (Text)el.item( i ).getFirstChild() ).getData();
      }
    }

    Debug.debugMethodEnd();
    return moduleJS;
  }

}