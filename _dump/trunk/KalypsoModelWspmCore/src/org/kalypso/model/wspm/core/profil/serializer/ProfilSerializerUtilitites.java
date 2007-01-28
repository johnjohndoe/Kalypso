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
package org.kalypso.model.wspm.core.profil.serializer;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.Reader;
import java.io.Writer;

import org.apache.commons.io.IOUtils;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.ProfilFactory;

/**
 * Helper class with utility methods to handle {@link IProfilSource}s.
 * 
 * @author Gernot Belger
 */
public class ProfilSerializerUtilitites
{
  private ProfilSerializerUtilitites( )
  {
    // Helper class, do not instantiate
  }

  /** Read a file via the given profile source and creates a profile from it. */
  public static IProfil readProfile( final IProfilSource source, final File prfFile, final String profilType ) throws IOException
  {
    final IProfil profile = ProfilFactory.createProfil( profilType );

    Reader fileReader = null;
    try
    {
      fileReader = new BufferedReader( new InputStreamReader( new FileInputStream( prfFile ) ) );
      source.read( profile, fileReader );
      fileReader.close();

      return profile;
    }
    finally
    {
      IOUtils.closeQuietly( fileReader );
    }
  }

  /**
   * Writes a single profile into a file.
   * 
   * @param file
   *          The file to write into
   * @param profile
   *          This profiles gets written
   */
  public static void writeProfile( final IProfilSink sink, final IProfil profile, final File file ) throws IOException
  {
    Writer writer = null;
    try
    {
      writer = new BufferedWriter( new OutputStreamWriter( new FileOutputStream( file ) ) );
      sink.write( profile, writer );
      writer.close();
    }
    finally
    {
      IOUtils.closeQuietly( writer );
    }
  }

}
