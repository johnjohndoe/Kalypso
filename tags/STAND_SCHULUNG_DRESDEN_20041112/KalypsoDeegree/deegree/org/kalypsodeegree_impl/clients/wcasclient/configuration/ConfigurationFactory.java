// $Header:
// /cvsroot/deegree/deegree/org/deegree_impl/clients/wcasclient/configuration/ConfigurationFactory.java,v
// 1.7 2004/03/15 07:34:37 poth Exp $
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
package org.deegree_impl.clients.wcasclient.configuration;

import java.io.FileReader;
import java.io.IOException;
import java.io.Reader;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.HashMap;

import org.deegree.xml.XMLTools;
import org.deegree_impl.clients.wmsclient.configuration.WMSClientConfiguration;
import org.deegree_impl.clients.wmsclient.configuration.WMSClientConfigurationFactory;
import org.deegree_impl.tools.Debug;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.SAXException;

/**
 * Factory class for creating a basic catalog client configuration
 * 
 * <p>
 * ----------------------------------------------------------------------
 * </p>
 * 
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 * @version 6.11.2003
 */
public class ConfigurationFactory
{
  private static final String CCNS = "http://www.deegree.org/catalogclient";

  /**
   * 
   * 
   * @param confFile
   * 
   * @return @throws
   *         SAXException
   * @throws IOException
   * @throws Exception
   */
  public static CSWClientConfiguration createConfiguration( String confFile ) throws SAXException,
      IOException, Exception
  {
    Debug.debugMethodBegin( "ConfigurationFactory", "createConfiguration(String)" );

    Reader reader = new FileReader( confFile );
    CSWClientConfiguration conf = createConfiguration( reader );
    reader.close();
    Debug.debugMethodEnd();
    return conf;
  }

  /**
   * 
   * 
   * @param reader
   * 
   * @return @throws
   *         SAXException
   * @throws IOException
   * @throws Exception
   */
  public static CSWClientConfiguration createConfiguration( Reader reader ) throws SAXException,
      IOException, Exception
  {
    Debug.debugMethodBegin( "ConfigurationFactory", "createConfiguration(String)" );

    Document doc = XMLTools.parse( reader );

    // catalog descriptions
    Element element = doc.getDocumentElement();
    NodeList nl = element.getElementsByTagNameNS( CCNS, "catalog" );
    CCatalog[] ccatalog = createCatalogs( nl );

    // mappings
    Element elem = (Element)element.getElementsByTagNameNS( CCNS, "mappings" ).item( 0 );
    nl = elem.getElementsByTagNameNS( CCNS, "field" );

    CMapping mapping = createMapping( nl );

    // maxInactiveInterval
    Node node = element.getElementsByTagNameNS( CCNS, "maxInactiveInterval" ).item( 0 );
    String tmp = node.getFirstChild().getNodeValue();
    int maxInactiveInterval = Integer.parseInt( tmp );

    // gets the maps width and height
    elem = (Element)element.getElementsByTagNameNS( CCNS, "mapsettings" ).item( 0 );
    WMSClientConfiguration wmsCConfig = createWMSClientConfiguration( elem );

    // thesaurus descriptions
    element = doc.getDocumentElement();
    nl = element.getElementsByTagNameNS( CCNS, "thesaurus" );
    HashMap thes = createThesauriDesc( nl );

    // get number of maximum visible records
    elem = XMLTools.getChildByName( "maxRecords", CCNS, element );
    int maxRecords = 20;

    if( elem != null )
    {
      String mr = XMLTools.getStringValue( elem );
      maxRecords = Integer.parseInt( mr );
    }

    // get download configuration
    elem = XMLTools.getRequiredChildByName( "download", CCNS, element );
    Download download = createDownload( elem );

    // create textcomponent
    elem = XMLTools.getRequiredChildByName( "textComponent", CCNS, element );
    TextComponent textComponent = createTextComponent( elem );

    // get filter -> IDs of valid UDK objects
    nl = element.getElementsByTagNameNS( CCNS, "datasetID" );
    String[] filterIDs = new String[nl.getLength()];

    for( int i = 0; i < nl.getLength(); i++ )
    {
      filterIDs[i] = XMLTools.getStringValue( nl.item( i ) );
    }

    // create configuration
    CSWClientConfiguration conf = new CSWClientConfiguration( ccatalog, mapping,
        maxInactiveInterval, wmsCConfig, thes, download, textComponent, maxRecords, filterIDs );

    Debug.debugMethodEnd();
    return conf;
  }

  /**
   * creates the access description for all catalogs served by the client
   * 
   * @param nl
   * 
   * @return @throws
   *         MalformedURLException
   * @throws Exception
   */
  private static CCatalog[] createCatalogs( NodeList nl ) throws MalformedURLException, Exception
  {
    Debug.debugMethodBegin( "ConfigurationFactory", "createCatalogs" );

    CCatalog[] catalogs = new CCatalog[nl.getLength()];

    for( int i = 0; i < nl.getLength(); i++ )
    {
      Element element = (Element)nl.item( i );
      Node node = element.getElementsByTagNameNS( CCNS, "name" ).item( 0 );
      String name = node.getFirstChild().getNodeValue();
      NodeList nll = element.getElementsByTagNameNS( CCNS, "type" );
      String[] types = new String[nll.getLength()];

      for( int k = 0; k < nll.getLength(); k++ )
      {
        types[k] = XMLTools.getStringValue( nll.item( k ) );
      }

      node = element.getElementsByTagNameNS( CCNS, "onlineResource" ).item( 0 );

      String tmp = XMLTools.getStringValue( node );
      catalogs[i] = new CCatalog( name, types, new URL( tmp ) );
    }

    Debug.debugMethodEnd();
    return catalogs;
  }

  /**
   * creates the mappings between (HTML)form fields and catalog fields
   * 
   * @param nl
   * 
   * @return
   */
  private static CMapping createMapping( NodeList nl )
  {
    Debug.debugMethodBegin( "ConfigurationFactory", "createMapping" );

    HashMap map = new HashMap( 200 );

    for( int i = 0; i < nl.getLength(); i++ )
    {
      Element element = (Element)nl.item( i );
      String name = XMLTools.getAttrValue( element, "name" );
      NodeList nll = element.getElementsByTagNameNS( CCNS, "cField" );
      String[] cFields = new String[nll.getLength()];

      for( int k = 0; k < nll.getLength(); k++ )
      {
        cFields[k] = XMLTools.getStringValue( nll.item( k ) );
      }

      map.put( name.toUpperCase(), cFields );
    }

    CMapping mapping = new CMapping( map );

    Debug.debugMethodEnd();
    return mapping;
  }

  /**
   * creates a map of thesauri names and associated addresses
   */
  private static HashMap createThesauriDesc( NodeList nl ) throws MalformedURLException
  {
    Debug.debugMethodBegin( "ConfigurationFactory", "createThesauriDesc" );

    HashMap thes = new HashMap();

    for( int i = 0; i < nl.getLength(); i++ )
    {
      Element element = (Element)nl.item( i );
      Node node = element.getElementsByTagNameNS( CCNS, "name" ).item( 0 );
      String name = XMLTools.getStringValue( node );
      node = element.getElementsByTagNameNS( CCNS, "onlineResource" ).item( 0 );
      String tmp = XMLTools.getStringValue( node );
      thes.put( name, new URL( tmp ) );
    }

    Debug.debugMethodEnd();
    return thes;
  }

  /**
   *  
   */
  private static TextComponent createTextComponent( Element elem ) throws Exception
  {
    Debug.debugMethodBegin();

    String s = XMLTools.getStringValue( elem );
    TextComponent textComponent = new TextComponent( s );

    Debug.debugMethodEnd();
    return textComponent;
  }

  /**
   * creates the configuration for the map client(s) embedded in the catalog
   * client
   */
  private static WMSClientConfiguration createWMSClientConfiguration( Element elem )
      throws Exception
  {
    Debug.debugMethodBegin();

    elem = (Element)elem.getElementsByTagNameNS( CCNS, "WMSClientConfiguration" ).item( 0 );
    WMSClientConfigurationFactory fac = new WMSClientConfigurationFactory( CCNS );
    WMSClientConfiguration conf = fac.createWMSClientConfiguration( elem, null );
    org.deegree_impl.clients.wmsclient.control.MapApplicationHandler
        .setDefaultClientConfiguration( conf );

    Debug.debugMethodEnd();
    return conf;
  }

  /**
   * creates an object that encapsulates the required informations for
   * downloading data through the catalog
   */
  private static Download createDownload( Element elem ) throws Exception
  {
    Debug.debugMethodBegin();

    String tmp = XMLTools.getRequiredStringValue( "cleanerInterval", CCNS, elem );
    int cleanerInterval = Integer.parseInt( tmp );
    tmp = XMLTools.getRequiredStringValue( "lifeTime", CCNS, elem );
    int lifeTime = Integer.parseInt( tmp );
    String storageDirectory = XMLTools.getRequiredStringValue( "storageDirectory", CCNS, elem );
    tmp = XMLTools.getRequiredStringValue( "onlineResource", CCNS, elem );
    URL onlineResource = new URL( tmp );

    elem = XMLTools.getRequiredChildByName( "mail", CCNS, elem );
    String mailFrom = XMLTools.getRequiredStringValue( "from", CCNS, elem );
    String mailSubject = XMLTools.getRequiredStringValue( "subject", CCNS, elem );
    String mailHost = XMLTools.getRequiredStringValue( "mailHost", CCNS, elem );

    Download download = new Download( cleanerInterval, lifeTime, storageDirectory, onlineResource,
        mailFrom, mailSubject, mailHost );

    Debug.debugMethodEnd();
    return download;
  }
}
/*******************************************************************************
 * Changes to this class. What the people have been up to: $Log:
 * ConfigurationFactory.java,v $ Revision 1.7 2004/03/15 07:34:37 poth no
 * message
 * 
 * 
 *  
 ******************************************************************************/