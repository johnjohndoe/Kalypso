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
package org.deegree_impl.services.gazetteer.capabilities;

import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Reader;
import java.net.URL;

import org.deegree.gml.GMLException;
import org.deegree.gml.GMLGeometry;
import org.deegree.model.geometry.GM_Exception;
import org.deegree.model.geometry.GM_Object;
import org.deegree.services.capabilities.Service;
import org.deegree.services.gazetteer.GazetteerException;
import org.deegree.services.gazetteer.SI_LocationType;
import org.deegree.services.gazetteer.capabilities.SI_Gazetteer;
import org.deegree.services.gazetteer.capabilities.WFSGCapabilities;
import org.deegree.services.wcas.metadatadesc.CitedResponsibleParty;
import org.deegree.services.wfs.capabilities.Capability;
import org.deegree.services.wfs.capabilities.FeatureTypeList;
import org.deegree.services.wfs.capabilities.WFSCapabilities;
import org.deegree.xml.ElementList;
import org.deegree.xml.XMLTools;
import org.deegree_impl.gml.GMLFactory;
import org.deegree_impl.model.geometry.GMLAdapter;
import org.deegree_impl.services.gazetteer.SI_LocationType_Impl;
import org.deegree_impl.services.wfs.capabilities.WFSCapabilitiesFactory;
import org.deegree_impl.tools.Debug;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.xml.sax.SAXException;

/**
 * 
 * @version $Revision$
 * @author AxxL
 *  
 */
public class WFSGCapabilitiesFactory
{

  //	TODO Exception Handling

  private static final String NAMESPACE_WFSG = "http://www.opengis.net/wfs-g";

  /**
   * 
   * @param url
   * @return
   */
  public static WFSGCapabilities createCapabilities( URL url ) throws GazetteerException
  {
    Debug.debugMethodBegin();
    WFSGCapabilities capabilities = null;

    try
    {
      InputStreamReader isr = new InputStreamReader( url.openStream() );
      capabilities = createCapabilities( isr );
    }
    catch( IOException e )
    {
      throw new GazetteerException( "IOException occured in creating the Capabilties:\n"
          + e.getMessage() );
    }
    catch( SAXException e )
    {
      throw new GazetteerException( "SAXException occured in creating the Capabilties:\n"
          + e.getMessage() );
    }

    Debug.debugMethodEnd();
    return capabilities;
  }

  /**
   * factory method for creating a <tt>WFSGCapabilities</tt> object from a
   * file that contains a OGC WFS 1.0 conform XML capabilities document
   */
  public static synchronized WFSGCapabilities createCapabilities( Reader reader )
      throws IOException, SAXException, GazetteerException
  {
    Debug.debugMethodBegin();

    Document doc = XMLTools.parse( reader );
    reader.close();
    WFSGCapabilities capabilities = createCapabilities( doc );

    Debug.debugMethodEnd();
    return capabilities;
  }

  /**
   * factory method for creating a <tt>WFSGCapabilities</tt> object from a OGC
   * WFS 1.0 conform XML capabilities document
   */
  public static synchronized WFSGCapabilities createCapabilities( Document doc )
      throws GazetteerException
  {
    Debug.debugMethodBegin();

    WFSCapabilities wfs = null;
    try
    {
      wfs = WFSCapabilitiesFactory.createCapabilities( doc );
    }
    catch( Exception e )
    {
      throw new GazetteerException(
          "Exception occured in creating WFSCapabilities out of Gazetteer:\n" + e.getMessage() );
    }

    Capability capability = wfs.getCapability();
    FeatureTypeList ftl = wfs.getFeatureTypeList();
    String version = wfs.getVersion();
    String updateSequence = wfs.getUpdateSequence();
    Service service = wfs.getService();

    // TODO Profile profile =

    // create capabilities object
    WFSGCapabilities_Impl capabilities = new WFSGCapabilities_Impl( capability, ftl, version,
        updateSequence, service );

    Debug.debugMethodEnd();

    return capabilities;
  }

  /**
   * 
   * @param element
   * @return
   */
  public static SI_Gazetteer getSI_Gazetteer( Element element ) throws GazetteerException
  {

    // identifier
    org.w3c.dom.Node node = XMLTools.getChildByName( "identifier", NAMESPACE_WFSG, element );
    String identifier = null;
    if( node != null )
    {
      identifier = XMLTools.getStringValue( node );
    }
    else
    {
      throw new GazetteerException(
          "mandatory element <Identifier> missing in Gazetteer Capabilities." );
    }

    // territoryOfUse
    node = XMLTools.getChildByName( "territoryOfUse", NAMESPACE_WFSG, element );
    GM_Object territoryOfUse = null;
    if( node != null )
    {
      territoryOfUse = getTerritoryOfUse( (Element)node );
    }
    else
    {
      throw new GazetteerException(
          "madatory element <territoryOfUse> missing in Gazetteer Capabilities." );
    }

    // custodian
    node = XMLTools.getChildByName( "custodian", NAMESPACE_WFSG, element );
    CitedResponsibleParty custodian = null;
    if( node != null )
    {
      custodian = getCustodian( (Element)node );
    }
    else
    {
      throw new GazetteerException(
          "madatory element <custodian> missing in Gazetteer Capabilities." );
    }

    // locationTypes maxOccurs="unbounded"
    ElementList elemlist = XMLTools
        .getChildElementsByName( "locationType", NAMESPACE_WFSG, element );
    SI_LocationType[] locationTypes = null;

    if( elemlist != null && elemlist.getLength() > 0 )
    {
      locationTypes = new SI_LocationType[elemlist.getLength()];
      SI_LocationType lt = null;
      for( int i = 0; i < elemlist.getLength(); i++ )
      {
        lt = getSI_LocationType( elemlist.item( i ) );
        locationTypes[i] = lt;
      }
    }
    else
    {
      throw new GazetteerException(
          "madatory element <LocationType> missing in Gazetteer Capabilities." );
    }

    // scope minOccurs="0"
    node = XMLTools.getChildByName( "scope", NAMESPACE_WFSG, element );
    String scope = null;
    if( node != null )
    {
      scope = XMLTools.getStringValue( node );
    }

    // coordinateSystem minOccurs="0"
    node = XMLTools.getChildByName( "coordinateSystem", NAMESPACE_WFSG, element );
    String coordinateSystem = null;
    if( node != null )
    {
      coordinateSystem = XMLTools.getStringValue( node );
    }

    SI_Gazetteer si_gazetteer = new SI_Gazetteer_Impl( identifier, territoryOfUse, custodian,
        locationTypes, scope, coordinateSystem );

    return si_gazetteer;
  }

  /**
   * 
   * @param element
   * @return
   */
  private static GM_Object getTerritoryOfUse( Element element ) throws GazetteerException
  {
    Element gml = XMLTools.getFirstElement( element );
    GMLGeometry gmlgeom = null;
    GM_Object gmobject = null;
    try
    {
      gmlgeom = GMLFactory.createGMLGeometry( gml );
      gmobject = GMLAdapter.wrap( gmlgeom );
    }
    catch( GMLException e )
    {
      throw new GazetteerException( e.getMessage() );
    }
    catch( GM_Exception e )
    {
      throw new GazetteerException( e.getMessage() );
    }
    return gmobject;
  }

  /**
   * calls getCitedResponsibleParty(element)
   * 
   * @param element
   * @return
   */
  private static CitedResponsibleParty getCustodian( Element element )
  {
    return getCitedResponsibleParty( element );
  }

  /**
   * 
   * @param element
   * @return
   */
  protected static CitedResponsibleParty getCitedResponsibleParty( Element element )
  {
    // TODO getCitedResponsibleParty

    // import org.deegree_impl.services.wcas.metadatadesc.WCAS_ISO19119Factory;

    // ContactInfo[] contactinfo;
    // String[] individualname;
    // String[] organisationname;
    // String[] positionname;
    // RoleCode[] rolecode;

    return null;
  }

  /**
   * 
   * @param element
   * @return
   */
  public static SI_LocationType getSI_LocationType( Element element ) throws GazetteerException
  {

    // Name
    org.w3c.dom.Node node = XMLTools.getChildByName( "name", NAMESPACE_WFSG, element );

    String name = null;
    if( node != null )
    {
      name = XMLTools.getStringValue( node );
    }
    else
    {
      throw new GazetteerException( "mandatory element <name> missing in SI_LocationType." );
    }

    // Theme
    node = XMLTools.getChildByName( "theme", NAMESPACE_WFSG, element );
    String theme = null;
    if( node != null )
    {
      XMLTools.getStringValue( node );
    }
    else
    {
      throw new GazetteerException( "mandatory element <theme> missing in SI_LocationType." );
    }

    // Identifier
    node = XMLTools.getChildByName( "identifier", NAMESPACE_WFSG, element );
    String identifier = null;
    if( node != null )
    {
      identifier = XMLTools.getStringValue( node );
    }
    else
    {
      throw new GazetteerException( "mandatory element <Identifier> missing in SI_LocationType." );
    }

    // Definition
    node = XMLTools.getChildByName( "definition", NAMESPACE_WFSG, element );
    String definition = null;
    if( node != null )
    {
      definition = XMLTools.getStringValue( node );
    }
    else
    {
      throw new GazetteerException( "mandatory element <Definition> missing in SI_LocationType." );
    }

    // Owner
    Element elem = XMLTools.getChildByName( "owner", NAMESPACE_WFSG, element );
    CitedResponsibleParty owner = null;
    if( elem != null )
    {
      owner = getCitedResponsibleParty( elem );
    }
    else
    {
      throw new GazetteerException( "mandatory element <owner> missing in SI_LocationType." );
    }

    // Parent
    ElementList parentlist = XMLTools.getChildElementsByName( "parent", NAMESPACE_WFSG, element );
    SI_LocationType[] parents = new SI_LocationType[parentlist.getLength()];

    if( parentlist != null && parentlist.getLength() > 0 )
    {
      SI_LocationType oneparent = null;
      for( int i = 0; i < parentlist.getLength(); i++ )
      {
        oneparent = getParent( parentlist.item( i ) );
        parents[i] = oneparent;
      }
    }

    // Child
    ElementList childlist = XMLTools.getChildElementsByName( "child", NAMESPACE_WFSG, element );
    SI_LocationType[] children = new SI_LocationType[childlist.getLength()];

    if( childlist != null && childlist.getLength() > 0 )
    {
      SI_LocationType onechild = null;
      for( int i = 0; i < childlist.getLength(); i++ )
      {
        onechild = getChild( childlist.item( i ) );
        children[i] = onechild;
      }
    }

    // TerritoryOfUse
    Element territoryofuseelement = XMLTools.getChildByName( "territoryOfUse", NAMESPACE_WFSG,
        element );
    GM_Object territoryOfUse = null;
    if( territoryofuseelement != null )
    {
      territoryOfUse = getTerritoryOfUse( territoryofuseelement );
    }
    else
    {
      throw new GazetteerException(
          "madatory element <TerritoryOfUse> missing in Gazetteer Capabilities." );
    }

    return new SI_LocationType_Impl( name, theme, identifier, definition, owner, parents, children,
        territoryOfUse );
  }

  /**
   * @param element
   * @return
   */
  private static SI_LocationType getParent( Element element ) throws GazetteerException
  {
    Element lt_elem = XMLTools.getChildByName( "SI_LocationType", NAMESPACE_WFSG, element );
    SI_LocationType parent = getSI_LocationType( lt_elem );

    if( parent != null )
    {
      return parent;
    }
    else
    {
      throw new GazetteerException( "Parent does not include a LocationType" );
    }

  }

  /**
   * @param element
   * @return
   */
  private static SI_LocationType getChild( Element element ) throws GazetteerException
  {
    Element lt_elem = XMLTools.getChildByName( "SI_LocationType", NAMESPACE_WFSG, element );
    SI_LocationType child = getSI_LocationType( lt_elem );
    return child;
  }

  /**
   * for testing purposes only
   * 
   * @param args
   */
  public static void main( String[] args )
  {
    try
    {
      Document doc = XMLTools.parse( "D:/gazetteer/xml/SI_Gazetteer.xml" );

      Element element = doc.getDocumentElement();

      getSI_Gazetteer( element );

    }
    catch( IOException e )
    {
      e.printStackTrace();
    }
    catch( SAXException e )
    {
      e.printStackTrace();
    }
    catch( GazetteerException e )
    {
      e.printStackTrace();
    }
  }
}