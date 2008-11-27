/*--------------- Kalypso-Header ------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and

 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

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

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de

 --------------------------------------------------------------------------*/

package org.kalypso.robotronadapter;

import java.io.File;
import java.net.MalformedURLException;
import java.rmi.RemoteException;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Map;
import java.util.Properties;
import java.util.logging.Logger;

import javax.activation.DataHandler;
import javax.activation.FileDataSource;
import javax.xml.rpc.ParameterMode;
import javax.xml.rpc.ServiceException;

import org.apache.axis.Constants;
import org.apache.axis.client.Call;
import org.apache.axis.client.Service;
import org.apache.commons.configuration.Configuration;
import org.apache.commons.io.FileUtils;
import org.kalypso.metadoc.IMetaDocCommiter;
import org.kalypso.metadoc.impl.MetaDocException;

/**
 * Commits the metadoc whithin the robotron/IMS framework. <br>
 * Following is an example of a XML-metadata string devlivered along the document-file.
 * 
 * <pre>
 &lt;meta&gt;
 &lt;autor&gt;Strumpf&lt;/autor&gt;
 &lt;doktyp&gt;Prognose&lt;/doktyp&gt;
 &lt;dokument&gt;Vorhersage-20.05.2005&lt;/dokument&gt;
 &lt;description&gt;blablabla Beschreibung&lt;/description&gt;
 &lt;schluessel&gt;eindeutiger Schlüssel-foobarscript&lt;/schluessel&gt;
 &lt;klasse&gt;Scheitelwerttabelle&lt;/klasse&gt;
 &lt;ersteller&gt;HVZ&lt;/ersteller&gt;
 &lt;erstelldat&gt;2005-08-26&lt;/erstelldat&gt;
 &lt;gueltigdat&gt;2005-08-26&lt;/gueltigdat&gt;
 &lt;station_id&gt;570123&lt;/station_id&gt;
 &lt;simulation&gt;0&lt;/simulation&gt;
 &lt;files&gt;
 &lt;file&gt;datei1.png&lt;/file&gt;
 &lt;file&gt;datei2.csv&lt;/file&gt;
 &lt;/files&gt;
 &lt;/meta&gt;
 * </pre>
 * 
 * <p>
 * You can configure defaults values (keys begin with robotron.preset.) or specific values (keys begin with
 * robotron.md.) in the properties of this commiter. The properties are usually defined in the associated Metadoc
 * Service (its configuration file).
 * 
 * @author schlienger
 */
public class RobotronMetaDocCommiter implements IMetaDocCommiter
{
  /*
   * If this system property is set to an absolute file path; every committed document (and xml metadata) are
   * additionally copied into this directory.
   */
  private static final String SYSPROP_DEBUG_DIR = "org.kalypso.robotronadapter.debugdir";

  private final static String XML_HEADER = "<?xml version=\"1.0\" encoding=\"utf-8\" ?>\n";
  private final static String LT = "<";
  private final static String CLT = "</";
  private final static String GT = ">";
  //  private final static String CGT = "/>";

  private static final String TAG_META = "meta";

  private final static String TAG_AUTOR = "autor";
  private final static String TAG_DESCRIPTION = "description";
  private final static String TAG_ERSTELLER = "ersteller";
  private final static String TAG_ERSTELLUNGSDATUM = "erstelldat";
  private final static String TAG_GUELTIGKEITSDATUM = "gueltigdat";

  private final static String TAG_DOKUMENT = "dokument";
  private final static String TAG_DOKUMENTTYP = "doktyp";
  private final static String TAG_GEBIET = "gebiet";
  private static final String TAG_STATION = "station_id";
  private static final String TAG_SIMULATION = "simulation";
  private static final String TAG_SCHLUESSEL = "schluessel";
  private static final String TAG_TYP = "typ";
  private static final String TAG_KLASSE = "klasse";

  private static final String TAG_FILES = "files";
  private static final String TAG_FILE = "file";

  /** date format for the date elements of the xml file */
  private final static DateFormat DFDATE = new SimpleDateFormat( "yyyy-MM-dd'T'HH:mm:ss" );

  private final static Logger LOG = Logger.getLogger( RobotronMetaDocCommiter.class.getName() );

  /** @see #SYSPROP_DEBUG_DIR */
  private final File m_debugDir;

  public RobotronMetaDocCommiter()
  {
    String debugDirProp = System.getProperty( SYSPROP_DEBUG_DIR, null );
    if( debugDirProp != null )
    {
      m_debugDir = new File( debugDirProp );
      m_debugDir.mkdirs();
    }
    else
      m_debugDir = null;
  }

  /**
   * @see org.kalypso.metadoc.IMetaDocCommiter#prepareMetainf(java.util.Properties, java.util.Map)
   */
  public void prepareMetainf( final Properties serviceProps, final Map metadata ) throws MetaDocException
  {
    metadata.put( TAG_AUTOR, "string;" + serviceProps.getProperty( "robotron.preset." + TAG_AUTOR, "Autor" ) );
    metadata.put( TAG_DESCRIPTION, "string;" + serviceProps.getProperty( "robotron.preset." + TAG_DESCRIPTION, "" ) );
    metadata.put( TAG_ERSTELLER, "string;" + serviceProps.getProperty( "robotron.preset." + TAG_ERSTELLER, "" ) );

    final String strd = DFDATE.format( new Date() );
    metadata.put( TAG_ERSTELLUNGSDATUM, "date;" + strd );

    // Put empty gültigkeit: docs are endlessly valid
    metadata.put( TAG_GUELTIGKEITSDATUM, "date;" + "" );
  }

  /**
   * @see org.kalypso.metadoc.IMetaDocCommiter#commitDocument(java.util.Properties, java.util.Map, java.io.File,
   *      java.lang.String, java.lang.String, org.apache.commons.configuration.Configuration)
   */
  public void commitDocument( final Properties serviceProps, final Map metadata, final File doc,
      final String identifier, final String category, final Configuration metadataExtensions ) throws MetaDocException
  {
    final String endpoint = serviceProps.getProperty( "robotron.ws.endpoint" );

    if( endpoint == null )
      throw new MetaDocException( "Service-Endpoint 'robotron.ws.endpoint' muss in die Properties-Datei definiert sein" );

    try
    {
      /* Create metadata XML */
      final Properties mdProps = new Properties();
      mdProps.putAll( metadata );
      final String metadataXml = buildXML( serviceProps, mdProps, doc.getName(), identifier, category,
          metadataExtensions );

      /* If configured; write doc+xml into separate directory */
      if( m_debugDir != null )
        writeDebug( doc, metadataXml );

      /* Try to invoce Robotron service */
      final Service service = new Service();
      final Call call = (Call)service.createCall();
      call.setTargetEndpointAddress( new java.net.URL( endpoint ) );
      call.setOperationName( "commitDocument" );
      call.addParameter( "docnames", Constants.SOAP_ARRAY, ParameterMode.IN );
      call.addParameter( "metadoc", Constants.XSD_STRING, ParameterMode.IN );
      call.setReturnType( Constants.XSD_STRING );

      final DataHandler[] docs = new DataHandler[]
      { new DataHandler( new FileDataSource( doc ) ) };

      LOG.info( "Metadata:\n" + metadataXml );

      LOG.info( "Calling IMS.commitDocument()" );
      final long dBegin = System.currentTimeMillis();
      final String ret = (String)call.invoke( new Object[]
      {
          docs,
          metadataXml } );

      final String msg = ret.length() > 0 ? " \"" + ret + "\"" : "";
      LOG.info( "IMS.commitDocument returned" + msg + ". Duration (ms)= " + ( System.currentTimeMillis() - dBegin ) );

      if( ret.length() > 0 )
        throw new MetaDocException( "Die IMS-Dokumentenablage hat einen Fehler verursacht: " + ret );
    }
    catch( final MetaDocException e )
    {
      throw e;
    }
    catch( RemoteException re )
    {
      throw new MetaDocException( "Fehler beim Zugriff auf Robotron-IMS (" + endpoint + ")", re );
    }
    catch( MalformedURLException e )
    {
      throw new MetaDocException( "Adresse des Robotron Endpoint ungültig: " + endpoint, e );
    }
    catch( ServiceException e )
    {
      throw new MetaDocException( "Apache Axis Service konnte nicht instantiiert werden", e );
    }
  }

  /**
   * Copies the commit data into a separate directory
   * 
   * @see #SYSPROP_DEBUG_DIR
   */
  private void writeDebug( File doc, String metadataXml )
  {
    try
    {
      FileUtils.copyFileToDirectory( doc, m_debugDir );
      final File xmlFile = new File( doc.getParentFile(), doc.getName() + ".xml" );
      FileUtils.writeStringToFile( xmlFile, metadataXml, "UTF-8" );
    }
    catch( Exception e )
    {
      e.printStackTrace();
    }
  }

  /**
   * Builds the metadata-XML
   * 
   * @param serviceProps
   *          properties of the service
   * @param mdProps
   *          properties of the metadata
   * @param identifier
   *          identifier of the document. Documents with same identifiers within the same forecast
   *          (&lt;documen&gt;-Element) are overwriten in the IMS/Robotron.
   * @param category
   * @param metadataExtensions
   *          might contain some addition information that can be used to fill the metadata for the document (for
   *          instance: Pegelkennziffer or station_id in the sense of Robotron IMS)
   */
  public static String buildXML( final Properties serviceProps, final Properties mdProps, final String fileName,
      final String identifier, final String category, final Configuration metadataExtensions )
  {
    final StringBuffer bf = new StringBuffer();

    bf.append( XML_HEADER );

    bf.append( LT + TAG_META + GT );

    bf.append( LT + TAG_AUTOR + GT + valueOfProperty( mdProps.getProperty( TAG_AUTOR ) ) + CLT + TAG_AUTOR + GT );
    bf.append( LT + TAG_ERSTELLER + GT + valueOfProperty( mdProps.getProperty( TAG_ERSTELLER ) ) + CLT + TAG_ERSTELLER
        + GT );
    bf.append( LT + TAG_ERSTELLUNGSDATUM + GT + valueOfProperty( mdProps.getProperty( TAG_ERSTELLUNGSDATUM ) ) + CLT
        + TAG_ERSTELLUNGSDATUM + GT );
    final String valueGueltdat = valueOfProperty( mdProps.getProperty( TAG_GUELTIGKEITSDATUM ) );
    bf.append( LT + TAG_GUELTIGKEITSDATUM + GT + valueGueltdat + CLT + TAG_GUELTIGKEITSDATUM + GT );

    final String dokumentTyp = serviceProps.getProperty( "robotron.preset." + TAG_DOKUMENTTYP );
    bf.append( LT + TAG_DOKUMENTTYP + GT + dokumentTyp + CLT + TAG_DOKUMENTTYP + GT );

    final String keyDokument = serviceProps.getProperty( "robotron.md." + TAG_DOKUMENT );
    String dokument = "";
    if( metadataExtensions != null && keyDokument != null )
      dokument = metadataExtensions.getString( keyDokument, "" );
    bf.append( LT + TAG_DOKUMENT + GT + dokument + CLT + TAG_DOKUMENT + GT );

    bf.append( LT + TAG_SCHLUESSEL + GT + identifier + CLT + TAG_SCHLUESSEL + GT );

    String klasse = "";
    String typ = "";
    if( category != null )
    {
      final String[] splits = category.split( ";", 2 );
      if( splits.length == 2 )
      {
        typ = splits[0];
        klasse = splits[1];
      }
      else if( splits.length == 1 )
        klasse = splits[0];
    }

    bf.append( LT + TAG_TYP + GT + typ + CLT + TAG_TYP + GT );
    bf.append( LT + TAG_KLASSE + GT + klasse + CLT + TAG_KLASSE + GT );

    bf.append( LT + TAG_DESCRIPTION + GT + valueOfProperty( mdProps.getProperty( TAG_DESCRIPTION ) ) + CLT
        + TAG_DESCRIPTION + GT );

    final String keyScenario = serviceProps.getProperty( "robotron.md.scenario" );
    String scenarioId = "";
    if( metadataExtensions != null && keyScenario != null )
      scenarioId = metadataExtensions.getString( keyScenario, "" );

    final String simulation = serviceProps.getProperty( "robotron.md." + TAG_SIMULATION + "_" + scenarioId );
    bf.append( LT + TAG_SIMULATION + GT + simulation + CLT + TAG_SIMULATION + GT );

    final String keyGebiet = serviceProps.getProperty( "robotron.md." + TAG_GEBIET );
    String gebiet = "";
    if( metadataExtensions != null && keyGebiet != null )
      gebiet = metadataExtensions.getString( keyGebiet, "" );
    bf.append( LT + TAG_GEBIET + GT + gebiet + CLT + TAG_GEBIET + GT );

    final String keyStationId = serviceProps.getProperty( "robotron.md." + TAG_STATION );
    String[] stationId = {};
    if( metadataExtensions != null && keyStationId != null )
      stationId = metadataExtensions.getStringArray( keyStationId );

    // robotron will nur ein station_id, sonst macht es keinen Sinn
    if( stationId != null && stationId.length == 1 )
      bf.append( LT + TAG_STATION + GT + stationId[0] + CLT + TAG_STATION + GT );
    else
      bf.append( LT + TAG_STATION + GT + "" + CLT + TAG_STATION + GT ); // sonst leeres id hier

    bf.append( LT + TAG_FILES + GT );
    bf.append( LT + TAG_FILE + GT + fileName + CLT + TAG_FILE + GT );
    bf.append( CLT + TAG_FILES + GT );

    bf.append( CLT + TAG_META + GT );

    return bf.toString();
  }

  /**
   * Properties can contain the value type and the value. In that case, just return the value.
   * 
   * @return value part of prop
   */
  private static String valueOfProperty( final String prop )
  {
    if( prop == null )
      return "";

    final String[] splits = prop.split( ";", 2 );

    if( splits.length > 1 )
      return splits[1];

    return prop;
  }
}
