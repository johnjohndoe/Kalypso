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
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Map;
import java.util.Properties;

import javax.xml.rpc.ParameterMode;

import org.apache.axis.Constants;
import org.apache.axis.client.Call;
import org.apache.axis.client.Service;
import org.kalypso.metadoc.IMetaDocCommiter;
import org.kalypso.metadoc.impl.MetaDocException;

/**
 * Commits the metadoc whithin the robotron framework
 * 
 * <pre>
 &lt;meta&gt;
 &lt;autor&gt;Strumpf&lt;/autor&gt;
 &lt;doktyp&gt;Prognose&lt;/doktyp&gt;
 &lt;ersteller&gt;HVZ&lt;/ersteller&gt;
 &lt;erstelldat&gt;2005-08-26&lt;/erstelldat&gt;
 &lt;gueltigdat&gt;2005-08-26&lt;/gueltigdat&gt;
 &lt;station_id&gt;570123&lt;/station_id&gt;
 &lt;files&gt;
 &lt;file&gt;datei1.png&lt;/file&gt;
 &lt;file&gt;datei2.csv&lt;/file&gt;
 &lt;/files&gt;
 &lt;/meta&gt;
 * </pre>
 * 
 * @author schlienger
 */
public class RobotronMetaDocCommiter implements IMetaDocCommiter
{
  private final static String XML_HEADER = "<?xml version=\"1.0\" encoding=\"utf-8\" ?>\n";
  private final static String LT = "<";
  private final static String CLT = "</";
  private final static String GT = ">";

  private static final String TAG_META = "meta";
  private final static String TAG_AUTOR = "autor";
  private final static String TAG_DOKUMENTTYP = "doktyp";
  private final static String TAG_ERSTELLER = "ersteller";
  private final static String TAG_ERSTELLUNGSDATUM = "erstelldat";
  private final static String TAG_GUELTIGKEITSDATUM = "gueltigdat";
  private static final String TAG_STATION = "station_id";
  private static final String TAG_FILES = "files";
  private static final String TAG_FILE = "file";

  /** date format for the date elements of the xml file */
  private final static DateFormat DFDATE = new SimpleDateFormat( "yyyy-MM-dd" );
  //private final static DateFormat DFDATETIME = new SimpleDateFormat( "yyyy-MM-dd'T'HH:mm:ss" );

  /**
   * @see org.kalypso.metadoc.IMetaDocCommiter#prepareMetainf(java.util.Properties, java.util.Map)
   */
  public void prepareMetainf( final Properties serviceProps, final Map metadata ) throws MetaDocException
  {
    metadata.put( TAG_AUTOR, "string;" + serviceProps.getProperty( "robotron.preset." + IMetaDocCommiter.KEY_AUTOR, "Autor" ) );
    metadata.put( TAG_DOKUMENTTYP, "string;" + serviceProps.getProperty( "robotron.preset." + TAG_DOKUMENTTYP, "Dokumenttyp" ) );
    metadata.put( TAG_ERSTELLER, "string;" + serviceProps.getProperty( "robotron.preset." + TAG_ERSTELLER, "Ersteller" ) );
    metadata.put( TAG_ERSTELLUNGSDATUM, "date;" + DFDATE.format( new Date() ) );
    metadata.put( TAG_GUELTIGKEITSDATUM, "date;" + DFDATE.format( new Date() ) );
    metadata.put( TAG_STATION, "string;" + serviceProps.getProperty( "robotron.preset." + TAG_STATION, "123456" ) );
  }

  /**
   * @see org.kalypso.metadoc.IMetaDocCommiter#commitDocument(java.util.Properties, java.util.Map, java.io.File)
   */
  public void commitDocument( final Properties serviceProps, final Map metadata, final File doc )
      throws MetaDocException
  {
    //String endpoint = "http://localhost:8080/eXForms/KalypsoConnectorWS.jws";
    final String endpoint = serviceProps.getProperty( "robotron.ws.endpoint" );

    if( endpoint == null )
      throw new MetaDocException( "Service-Endpoint 'robotron.ws.endpoint' muss in die Properties-Datei definiert sein" );

    try
    {
      final Service service = new Service();
      final Call call = (Call)service.createCall();
      call.setTargetEndpointAddress( new java.net.URL( endpoint ) );
      call.setOperationName( "commitDocument" );
      call.addParameter( "docnames", Constants.SOAP_ARRAY, ParameterMode.IN );
      call.addParameter( "metadoc", Constants.XSD_STRING, ParameterMode.IN );
      call.setReturnType( Constants.XSD_STRING );

      final Properties mdProps = new Properties();
      mdProps.putAll( metadata );

      final String[] docs = new String[] { doc.getAbsolutePath() };
      final String metadataXml = buildXML( serviceProps, mdProps, doc.getAbsolutePath() );

      final String ret = (String)call.invoke( new Object[]
      { docs, metadataXml } );

      System.out.println( "Got result : " + ret );
    }
    catch( final Exception e )
    {
      e.printStackTrace();

      throw new MetaDocException( e );
    }
    finally
    {
      doc.delete();
    }
  }

  /**
   * Builds the metadata-XML
   * 
   * @param serviceProps
   *          properties of the service
   * @param mdProps
   *          properties of the metadata
   */
  public static String buildXML( final Properties serviceProps, final Properties mdProps, final String fileName )
  {
    final StringBuffer bf = new StringBuffer();

    bf.append( XML_HEADER );

    bf.append( LT + TAG_META + GT );

    bf.append( LT + TAG_AUTOR + GT + valueOfProperty( mdProps.getProperty( TAG_AUTOR ) ) + CLT + TAG_AUTOR + GT );
    bf.append( LT + TAG_DOKUMENTTYP + GT + valueOfProperty( mdProps.getProperty( TAG_DOKUMENTTYP ) ) + CLT
        + TAG_DOKUMENTTYP + GT );
    bf.append( LT + TAG_ERSTELLER + GT + valueOfProperty( mdProps.getProperty( TAG_ERSTELLER ) ) + CLT + TAG_ERSTELLER
        + GT );
    bf.append( LT + TAG_ERSTELLUNGSDATUM + GT + valueOfProperty( mdProps.getProperty( TAG_ERSTELLUNGSDATUM ) ) + CLT
        + TAG_ERSTELLUNGSDATUM + GT );
    bf.append( LT + TAG_GUELTIGKEITSDATUM + GT + valueOfProperty( mdProps.getProperty( TAG_GUELTIGKEITSDATUM ) ) + CLT
        + TAG_GUELTIGKEITSDATUM + GT );

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

    final String[] splits = prop.split( ";" );

    if( splits.length > 1 )
      return splits[1];

    return prop;
  }
}
