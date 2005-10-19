/*
 * ---------------- FILE HEADER KALYPSO ------------------------------------------
 * 
 * This file is part of kalypso. Copyright (C) 2004 by:
 * 
 * Technical University Hamburg-Harburg (TUHH) Institute of River and coastal engineering Denickestraße 22 21073
 * Hamburg, Germany http://www.tuhh.de/wb
 * 
 * and
 * 
 * Bjoernsen Consulting Engineers (BCE) Maria Trost 3 56070 Koblenz, Germany http://www.bjoernsen.de
 * 
 * This library is free software; you can redistribute it and/or modify it under the terms of the GNU Lesser General
 * Public License as published by the Free Software Foundation; either version 2.1 of the License, or (at your option)
 * any later version.
 * 
 * This library is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
 * details.
 * 
 * You should have received a copy of the GNU Lesser General Public License along with this library; if not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 * 
 * Contact:
 * 
 * E-Mail: g.belger@bjoernsen.de m.schlienger@bjoernsen.de v.doemming@tuhh.de
 * 
 * ---------------------------------------------------------------------------
 */
package org.kalypso.services.metadoc.impl;

import java.io.File;
import java.io.IOException;
import java.util.Map;
import java.util.Properties;

import org.apache.commons.configuration.Configuration;
import org.apache.commons.configuration.ConfigurationUtils;
import org.apache.commons.io.FileUtils;
import org.kalypso.metadoc.IMetaDocCommiter;
import org.kalypso.metadoc.impl.MetaDocException;

/**
 * Default commits to a file
 * 
 * @author thuel2
 */
public class DefaultMetaDocCommitter implements IMetaDocCommiter
{
  /**
   * @see org.kalypso.metadoc.IMetaDocCommiter#prepareMetainf(java.util.Properties, java.util.Map)
   */
  public void prepareMetainf( Properties serviceProps, Map metadata )
  {
  // nothing
  }

  /**
   * @see org.kalypso.metadoc.IMetaDocCommiter#commitDocument(java.util.Properties, java.util.Map, java.io.File,
   *      org.apache.commons.configuration.Configuration)
   */
  public void commitDocument( final Properties serviceProps, final Map metadata, final File doc,
      final Configuration metadataExtensions ) throws MetaDocException
  {
    final String strdir = serviceProps.getProperty( "defaultCommitter.dir" );
    final File file = new File( strdir, doc.getName() );

    // since there are currently no metadata with this committer, we
    // do not create the metadata file.
    //    final File metaFile = new File( strdir, doc.getName() + ".txt" );
    //    final Properties metaProps = new Properties();
    //    metaProps.putAll( metadata );
    //    
    //    OutputStream out = null;

    // output the metadataExtensions for debugging purposes
    System.out.println( "DefaultCommiter, MetadataExtensions: " + ConfigurationUtils.toString( metadataExtensions ) );

    try
    {
      System.out.println( "DefaultMetaDocCommitter: copying file " + doc + " to " + file );
      FileUtils.copyFile( doc, file );

      //      out = new FileOutputStream( metaFile );
      //      metaProps.store( out, "Metadata für " + file );
      //      out.close();
    }
    catch( final IOException e )
    {
      e.printStackTrace();

      throw new MetaDocException( e );
    }
    finally
    {
      doc.delete();

      //      IOUtils.closeQuietly( out );
    }
  }
}
