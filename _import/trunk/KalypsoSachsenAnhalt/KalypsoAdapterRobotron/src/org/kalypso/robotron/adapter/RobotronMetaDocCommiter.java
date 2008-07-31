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

package org.kalypso.robotron.adapter;

import java.io.File;
import java.net.URL;

import javax.activation.DataHandler;
import javax.activation.URLDataSource;
import javax.xml.namespace.QName;
import javax.xml.rpc.ParameterMode;

import org.apache.axis.Constants;
import org.apache.axis.client.Call;
import org.apache.axis.client.Service;
import org.kalypso.commons.java.io.FileUtilities;
import org.kalypso.hwv.services.metadoc.DocumentServiceSimulation;
import org.kalypso.hwv.services.metadoc.IDocumentServiceConstants;
import org.kalypso.simulation.core.ISimulationMonitor;
import org.kalypsodeegree.model.feature.Feature;

/**
 * Commits the metadoc whithin the robotron/IMS framework. <br>
 * Following is an example of a XML-metadata string devlivered along the document-file.
 * 
 * <pre>
 * &lt;meta&gt;
 *  &lt;autor&gt;Strumpf&lt;/autor&gt;
 *  &lt;doktyp&gt;Prognose&lt;/doktyp&gt;
 *  &lt;dokument&gt;Vorhersage-20.05.2005&lt;/dokument&gt;
 *  &lt;description&gt;blablabla Beschreibung&lt;/description&gt;
 *  &lt;schluessel&gt;eindeutiger Schlüssel-foobarscript&lt;/schluessel&gt;
 *  &lt;klasse&gt;Scheitelwerttabelle&lt;/klasse&gt;
 *  &lt;ersteller&gt;HVZ&lt;/ersteller&gt;
 *  &lt;erstelldat&gt;2005-08-26&lt;/erstelldat&gt;
 *  &lt;gueltigdat&gt;2005-08-26&lt;/gueltigdat&gt;
 *  &lt;station_id&gt;570123&lt;/station_id&gt;
 *  &lt;simulation&gt;0&lt;/simulation&gt;
 *  &lt;files&gt;
 *  &lt;file&gt;datei1.png&lt;/file&gt;
 *  &lt;file&gt;datei2.csv&lt;/file&gt;
 *  &lt;/files&gt;
 *  &lt;/meta&gt;
 * </pre>
 * <p>
 * 
 * @author Marc Schlienger
 * @author Holger Albert
 */
public class RobotronMetaDocCommiter extends DocumentServiceSimulation
{
  public static String SYSPROP_ROBOTRON_ENDPOINT = IDocumentServiceConstants.SYSPROP_BASE + ".robotron.ws.endpoint";

  /**
   * @see org.kalypso.hwv.services.metadoc.DocumentServiceSimulation#commitDocument(java.io.File,
   *      org.kalypsodeegree.model.feature.Feature, java.net.URL, org.kalypso.simulation.core.ISimulationMonitor)
   */
  @Override
  protected void commitDocument( File tmpdir, Feature metadataFeature, URL documentURL, ISimulationMonitor monitor ) throws Exception
  {
    /* The connector address. */
    String endpoint = System.getProperty( SYSPROP_ROBOTRON_ENDPOINT );
    if( endpoint == null )
      throw new Exception( "Service-Endpoint '" + SYSPROP_ROBOTRON_ENDPOINT + "' muss in der config.ini definiert sein ..." );

    String preferredFilename = (String) metadataFeature.getProperty( IDocumentServiceConstants.QNAME_META_PREFERRED_FILENAME );
    String preferredValidFilename = FileUtilities.validateName( preferredFilename, "_" );

    /* Create the docs. */
    DataHandler[] docs = new DataHandler[] { new DataHandler( new URLDataSource( documentURL ) ) };

    /* Create the metadata XML string. */
    String metadataXml = MetaDocSerializer.buildXML( preferredValidFilename, metadataFeature );

    /* Distribute the document and its metadata. */
    commitDocument( endpoint, docs, metadataXml );
  }

  /**
   * This function commits the document.
   * 
   * @param endpoint
   *          The connector address.
   * @param docs
   *          The docs, which should be committed.
   * @param metadataXml
   *          The metadata XML string.
   */
  private void commitDocument( String endpoint, DataHandler[] docs, String metadataXml ) throws Exception
  {
    /* Create the service. */
    Service service = new Service();

    /* Create the call. */
    Call call = (Call) service.createCall();

    /* Not sure, if it really works. I wasn't able to test it. But the error in the WPS goes away. */
    /* Now there is an mail error. What with the mail-api from java? */
    QName qn = new QName( "urn:BeanService", "DataHandler" );
    call.registerTypeMapping( DataHandler.class, qn, new org.apache.axis.encoding.ser.JAFDataHandlerSerializerFactory( DataHandler.class, qn ), new org.apache.axis.encoding.ser.JAFDataHandlerDeserializerFactory( DataHandler.class, qn ) );

    call.setTargetEndpointAddress( new URL( endpoint ) );
    call.setOperationName( "commitDocument" );
    call.addParameter( "docnames", Constants.SOAP_ARRAY, ParameterMode.IN );
    call.addParameter( "metadoc", Constants.XSD_STRING, ParameterMode.IN );
    call.setReturnType( Constants.XSD_STRING );

    /* Calling IMS.commitDocument(). */
    String ret = (String) call.invoke( new Object[] { docs, metadataXml } );
    String msg = ret.length() > 0 ? " \"" + ret + "\"" : "";
    if( msg.length() > 0 )
      throw new Exception( "Die IMS-Dokumentenablage hat einen Fehler verursacht: " + msg );
  }
}