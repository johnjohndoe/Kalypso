/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraﬂe 22
 *  21073 Hamburg, Germany
 *  http://www.tuhh.de/wb
 * 
 *  and
 *  
 *  Bjoernsen Consulting Engineers (BCE)
 *  Maria Trost 3
 *  56070 Koblenz, Germany
 *  http://www.bjoernsen.de
 * 
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 * 
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 * 
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 * 
 *  Contact:
 * 
 *  E-Mail:
 *  belger@bjoernsen.de
 *  schlienger@bjoernsen.de
 *  v.doemming@tuhh.de
 *   
 *  ---------------------------------------------------------------------------*/
package org.kalypso.core.jaxb;

import static javax.xml.XMLConstants.W3C_XML_SCHEMA_NS_URI;

import java.net.URL;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;
import javax.xml.bind.PropertyException;
import javax.xml.validation.Schema;
import javax.xml.validation.SchemaFactory;

import org.kalypso.contribs.eclipse.core.runtime.PluginUtilities;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.core.KalypsoCorePlugin;
import org.kalypso.jwsdp.JaxbUtilities;
import org.kalypso.template.gismapview.ObjectFactory;
import org.xml.sax.SAXException;

/**
 * Utility class for handling with the 'template' binding schemata.
 * 
 * @author Gernot Belger
 */
public class TemplateUtilitites
{
  private static final String SCHEMA_PATH = "etc/schemas/template/";

  private static final SchemaFactory SCHEMA_FACTORY = SchemaFactory.newInstance( W3C_XML_SCHEMA_NS_URI );

  /* GisMapView */
  
  public static final JAXBContext JC_GISMAPVIEW = JaxbUtilities.createQuiet( ObjectFactory.class );

  public static final ObjectFactory OF_GISMAPVIEW = new ObjectFactory();

  private static Schema SH_GISMAPVIEW = null;

  /* GisTableView */
  
  public static final JAXBContext JC_GISTABLEVIEW = JaxbUtilities.createQuiet( org.kalypso.template.gistableview.ObjectFactory.class );

  public static final org.kalypso.template.types.ObjectFactory OF_TYPES = new org.kalypso.template.types.ObjectFactory();


  private TemplateUtilitites( )
  {
    // do not instantiat, eversthing is static
  }

  public static synchronized Schema getGismapviewSchema( )
  {
    if( SH_GISMAPVIEW == null )
    {
      final URL schemaUrl = PluginUtilities.findResource( KalypsoCorePlugin.getID(), SCHEMA_PATH + "gismapview.xsd" );

      try
      {
        SH_GISMAPVIEW = SCHEMA_FACTORY.newSchema( schemaUrl );
      }
      catch( final SAXException e )
      {
        KalypsoCorePlugin.getDefault().getLog().log( StatusUtilities.statusFromThrowable( e ) );
      }
    }

    return SH_GISMAPVIEW;
  }

  public static Marshaller createGismapviewMarshaller( final String encoding ) throws JAXBException, PropertyException
  {
    final Marshaller marshaller = JaxbUtilities.createMarshaller( TemplateUtilitites.JC_GISMAPVIEW );
    marshaller.setProperty( Marshaller.JAXB_FORMATTED_OUTPUT, Boolean.TRUE );
    marshaller.setProperty( Marshaller.JAXB_ENCODING, encoding );
    marshaller.setSchema( TemplateUtilitites.getGismapviewSchema() );
    return marshaller;
  }
  
}
