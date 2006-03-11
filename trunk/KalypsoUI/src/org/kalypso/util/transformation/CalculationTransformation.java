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
import java.io.IOException;
import java.io.PipedInputStream;
import java.io.PipedOutputStream;
import java.util.Iterator;
import java.util.Map;
import java.util.Properties;
import java.util.Map.Entry;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.kalypso.commons.java.util.PropertiesHelper;
import org.kalypso.contribs.java.lang.CatchThread;

/**
 * @deprecated use ant task instead in your model-configuration
 * @author belger
 */
@Deprecated
public class CalculationTransformation extends AbstractTransformation
{
  /** Dateiname für das Ergebnis */
  public static final String PROP_OUTPUT = "output";

  /** Key/Value Paare */
  public static final String PROP_ENTRY = "entry";

  /**
   * @see org.kalypso.util.transformation.AbstractTransformation#transformIntern(java.util.Properties, java.io.BufferedWriter, java.io.BufferedWriter, org.eclipse.core.runtime.IProgressMonitor)
   */
  public void transformIntern( final Properties properties, final BufferedWriter msgWriter,
      final BufferedWriter logWriter, final IProgressMonitor monitor ) throws TransformationException
  {
    try
    {
      final Properties targetProperties = parseProperties( properties );
      final PipedOutputStream pos = new PipedOutputStream();
      final PipedInputStream pis = new PipedInputStream( pos );

      final CatchThread ct = new CatchThread()
      {
        protected void runIntern() throws Throwable
        {
          targetProperties.store( pos, "Steuerparameter der Berechnung" );
          pos.close();
        }
      };
      ct.start();

      final IFile outputFile = ResourcesPlugin.getWorkspace().getRoot().getFile(
          new Path( properties.getProperty( PROP_OUTPUT ) ) );
      outputFile.create( pis, false, monitor );
    }
    catch( final CoreException e )
    {
      throw new TransformationException( e );
    }
    catch( IOException e )
    {
      e.printStackTrace();
    }
  }

  /**
   * Aus den Entries wieder einzelne Properties machen
   * 
   * @param properties
   * @return properties
   */
  private Properties parseProperties( final Properties properties )
  {
    final Properties newProps = new Properties();

    for( final Iterator pIt = properties.entrySet().iterator(); pIt.hasNext(); )
    {
      final Map.Entry entry = (Entry)pIt.next();

      final String key = entry.getKey().toString();

      if( key.startsWith( PROP_ENTRY ) )
      {
        final String value = entry.getValue().toString();
        final Properties entryProp = PropertiesHelper.parseFromString( value, '#' );

        for( final Iterator eIt = entryProp.entrySet().iterator(); eIt.hasNext(); )
        {
          final Map.Entry entryEntry = (Entry)eIt.next();

          final String entryKey = entryEntry.getKey().toString();
          final String entryValue = entryEntry.getValue().toString();

          newProps.setProperty( entryKey, entryValue );
        }
      }
    }

    return newProps;
  }
}