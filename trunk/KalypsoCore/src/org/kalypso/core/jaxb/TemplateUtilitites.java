/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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
import java.util.HashMap;
import java.util.Map;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;
import javax.xml.bind.PropertyException;
import javax.xml.bind.Unmarshaller;
import javax.xml.validation.Schema;
import javax.xml.validation.SchemaFactory;

import org.eclipse.core.runtime.Platform;
import org.kalypso.commons.bind.JaxbUtilities;
import org.kalypso.contribs.eclipse.core.runtime.PluginUtilities;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.core.KalypsoCoreDebug;
import org.kalypso.core.KalypsoCorePlugin;
import org.kalypso.template.gismapview.ObjectFactory;
import org.xml.sax.SAXException;

/**
 * Utility class for handling with the 'template' binding schemata.
 *
 * @author Gernot Belger
 */
public class TemplateUtilitites
{
  private static final String SCHEMA_PATH = "etc/schemas/template/"; //$NON-NLS-1$

  private static final SchemaFactory SCHEMA_FACTORY = SchemaFactory.newInstance( W3C_XML_SCHEMA_NS_URI );

  private final static Map<String, Schema> SCHEMA_MAP = new HashMap<String, Schema>();

  /* GisMapView */
  public static final JAXBContext JC_GISMAPVIEW = JaxbUtilities.createQuiet( ObjectFactory.class );

  public static final ObjectFactory OF_GISMAPVIEW = new ObjectFactory();

  /* GisTableView */
  public static final JAXBContext JC_GISTABLEVIEW = JaxbUtilities.createQuiet( org.kalypso.template.gistableview.ObjectFactory.class );

  /* GisTreeView */
  public static final JAXBContext JC_GISTREEVIEW = JaxbUtilities.createQuiet( org.kalypso.template.gistreeview.ObjectFactory.class );

  /* Featureview */
  public static final org.kalypso.template.featureview.ObjectFactory OF_FEATUREVIEW = new org.kalypso.template.featureview.ObjectFactory();

  public static final JAXBContext JC_FEATUREVIEW = JaxbUtilities.createQuiet( org.kalypso.template.featureview.ObjectFactory.class );

  private TemplateUtilitites( )
  {
    // do not instantiat, everything is static
  }

  public static Schema getFeatureviewSchema( )
  {
    return getTemplateSchema( "featureview.xsd" ); //$NON-NLS-1$
  }

  public static synchronized Schema getGismapviewSchema( )
  {
    return getTemplateSchema( "gismapview.xsd" ); //$NON-NLS-1$
  }

  private static synchronized Schema getTemplateSchema( final String schemaFilename )
  {
    if( !SCHEMA_MAP.containsKey( schemaFilename ) )
    {
      final URL schemaUrl = PluginUtilities.findResource( KalypsoCorePlugin.getID(), SCHEMA_PATH + schemaFilename );

      try
      {
        if( schemaUrl != null )
        {
          final Schema schema = SCHEMA_FACTORY.newSchema( schemaUrl );
          SCHEMA_MAP.put( schemaFilename, schema );
        }
      }
      catch( final SAXException e )
      {
        KalypsoCorePlugin.getDefault().getLog().log( StatusUtilities.statusFromThrowable( e ) );
      }
    }

    return SCHEMA_MAP.get( schemaFilename );
  }

  public static Marshaller createGismapviewMarshaller( final String encoding ) throws JAXBException, PropertyException
  {
    final Marshaller marshaller = JaxbUtilities.createMarshaller( TemplateUtilitites.JC_GISMAPVIEW );
    marshaller.setProperty( Marshaller.JAXB_FORMATTED_OUTPUT, Boolean.TRUE );
    marshaller.setProperty( Marshaller.JAXB_ENCODING, encoding );

    // REMARK: only validate in trace mode, because this lead often to errors
    // because the 'href' attribute of the styledLayers are anyURIs, but its values are often not.
    if( KalypsoCoreDebug.GISMAPVIEW_VALIDATE.isEnabled() )
      marshaller.setSchema( TemplateUtilitites.getGismapviewSchema() );

    return marshaller;
  }

  public static Unmarshaller createGismapviewUnmarshaller( ) throws JAXBException
  {
    final Unmarshaller unmarshaller = TemplateUtilitites.JC_GISMAPVIEW.createUnmarshaller();

    // REMARK: only validate in trace mode, because this lead often to errors
    // because the 'href' attribute of the styledLayers are anyURIs, but its values are often not.
    if( "true".equals( Platform.getDebugOption( KalypsoCorePlugin.getID() + "/debug/validatebinding/gismapview" ) ) ) //$NON-NLS-1$ //$NON-NLS-2$
      unmarshaller.setSchema( getGismapviewSchema() );

    return unmarshaller;
  }

  public static Unmarshaller createFeatureviewUnmarshaller( ) throws JAXBException
  {
    final Unmarshaller unmarshaller = JC_FEATUREVIEW.createUnmarshaller();

    if( "true".equals( Platform.getDebugOption( KalypsoCorePlugin.getID() + "/debug/validatebinding/featureview" ) ) ) //$NON-NLS-1$ //$NON-NLS-2$
      unmarshaller.setSchema( TemplateUtilitites.getFeatureviewSchema() );

    return unmarshaller;
  }
}
