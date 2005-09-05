/*--------------- Kalypso-Header --------------------------------------------------------------------

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
 
 ---------------------------------------------------------------------------------------------------*/
package org.kalypso.util.transformation;

import java.io.BufferedWriter;
import java.io.ByteArrayInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PipedInputStream;
import java.io.PipedOutputStream;
import java.io.UnsupportedEncodingException;
import java.util.Iterator;
import java.util.Map;
import java.util.Properties;
import java.util.Map.Entry;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.kalypso.commons.java.io.ReaderUtilities;
import org.kalypso.commons.java.util.StringUtilities;
import org.kalypso.contribs.java.io.StreamUtilities;

/**
 * @deprecated use ant task instead in your model-configuration
 * 
 * @author belger
 */
public class ReplaceTransformation extends AbstractTransformation
{
  /**
   * @see org.kalypso.util.transformation.AbstractTransformation#transformIntern(java.util.Properties, java.io.BufferedWriter, java.io.BufferedWriter, org.eclipse.core.runtime.IProgressMonitor)
   */
  public void transformIntern( final Properties properties, final BufferedWriter msgWriter,
      final BufferedWriter logWriter, final IProgressMonitor monitor ) throws TransformationException
  {
    monitor.beginTask( "Transform", 2000 );

    final Properties replaceProps = new Properties();

    for( Iterator iter = properties.entrySet().iterator(); iter.hasNext(); )
    {
      final Map.Entry entry = (Entry)iter.next();

      final String key = (String)entry.getKey();
      final String value = (String)entry.getValue();

      if( key.startsWith( "protocol" ) )
      {
        final String[] strings = value.split( "=" );
        replaceProps.put( strings[0] + ":", strings[1] );
      }
    }

    final IWorkspaceRoot root = ResourcesPlugin.getWorkspace().getRoot();

    for( Iterator iter = properties.entrySet().iterator(); iter.hasNext(); )
    {
      final Map.Entry entry = (Entry)iter.next();

      final String key = (String)entry.getKey();
      final String value = (String)entry.getValue();

      if( key.startsWith( "file" ) )
      {
        try
        {
          final IFile file = root.getFile( new Path( value ) );
          replaceFilecontent( file, replaceProps );
        }
        catch( final Exception e )
        {
          e.printStackTrace();

          throw new TransformationException( e );
        }
      }
    }
  }

  private void replaceFilecontent( final IFile file, final Properties replaceProperties ) throws FileNotFoundException,
      IOException, UnsupportedEncodingException, CoreException
  {
    final InputStreamReader r = new InputStreamReader( file.getContents(), file.getCharset() );
    final String string = ReaderUtilities.readStringFromReader( r );
    final String newContent = StringUtilities.replaceAll( string, replaceProperties );

    final PipedOutputStream pos = new PipedOutputStream();
    final PipedInputStream pis = new PipedInputStream( pos );

    final Thread writeThread = new Thread()
    {
      /**
       * @see java.lang.Thread#run()
       */
      public void run()
      {
        try
        {
          final ByteArrayInputStream bis = new ByteArrayInputStream( newContent.getBytes( file.getCharset() ) );
          StreamUtilities.streamCopy( bis, pos );
        }
        catch( final Exception e )
        {
          try
          {
            pos.close();
          }
          catch( IOException e1 )
          {
            e1.printStackTrace();
          }
        }

      }
    };
    writeThread.start();

    file.setContents( pis, false, true, new NullProgressMonitor() );
  }

}