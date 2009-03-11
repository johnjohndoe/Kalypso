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
package org.kalypso.core.catalog;

import java.io.File;
import java.io.InputStream;
import java.io.OutputStream;

import javax.xml.transform.OutputKeys;
import javax.xml.transform.Source;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import org.apache.commons.io.IOUtils;
import org.apache.tools.ant.filters.StringInputStream;
import org.kalypso.contribs.java.net.IUrlResolver2;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypsodeegree.graphics.sld.FeatureTypeStyle;
import org.kalypsodeegree.xml.XMLTools;
import org.kalypsodeegree_impl.graphics.sld.SLDFactory;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

/**
 * @author doemming
 */
public class CatalogSLD extends ObjectCatalog<FeatureTypeStyle>
{
  /** Do not call this yourself but rather get the catalog via {@link KalypsoCorePlugin#}*/
  public CatalogSLD( final CatalogManager cManager, final File repositoryBase )
  {
    super( repositoryBase, cManager, FeatureTypeStyle.class );
  }

  /**
   * @see org.kalypso.commons.serializer.ISerializer#read(java.io.InputStream)
   */
  @Override
  protected FeatureTypeStyle read( final IUrlResolver2 urlResolver, final InputStream ins )
  {
    try
    {
      final Document doc = XMLTools.parse( ins );
      final Element documentElement = doc.getDocumentElement();
      return SLDFactory.createFeatureTypeStyle( urlResolver, documentElement );
    }
    catch( final Exception e )
    {
      // TODO generate new exception type
      throw new UnsupportedOperationException( e );
    }
  }

  @Override
  protected void write( final FeatureTypeStyle ftStyle, final OutputStream os )
  {
    final String ftStyleAsString = ftStyle.exportAsXML();
    final Document doc;
    try
    {
      doc = XMLTools.parse( new StringInputStream( ftStyleAsString ) );
      final Source source = new DOMSource( doc );

      final Transformer t = TransformerFactory.newInstance().newTransformer();
      t.setOutputProperty( "{http://xml.apache.org/xslt}indent-amount", "2" ); //$NON-NLS-1$ //$NON-NLS-2$
      t.setOutputProperty( OutputKeys.INDENT, "yes" ); //$NON-NLS-1$
      t.transform( source, new StreamResult( os ) );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
    }
    finally
    {
      IOUtils.closeQuietly( os );
    }
  }

  public FeatureTypeStyle getSelected( final IUrlResolver2 resolver, final IFeatureType featureType )
  {
    return null;

    // in order to implement this, we must generate the appropriate url
    // but this is not yet possible with this generator stuff
    // we should get rid of this somehow

// try
// {
// final IURNGenerator generator = m_manager.getURNGeneratorFor( m_supportingClass );
// if( generator == null )
// return null;
// final String defaultURN = generator.generateDefaultURNForRelated( parent );
// return getValue( resolver, defaultURN, defaultURN );
// }
// catch( final Exception e )
// {
// final IStatus status = StatusUtilities.statusFromThrowable( e );
// KalypsoCorePlugin.getDefault().getLog().log( status );
// return null;
// }
  }

}
