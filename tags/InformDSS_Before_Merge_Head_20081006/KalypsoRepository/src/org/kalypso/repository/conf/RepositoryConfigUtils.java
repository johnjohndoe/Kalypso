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
package org.kalypso.repository.conf;

import java.io.BufferedInputStream;
import java.io.InputStream;
import java.net.URL;
import java.util.List;
import java.util.Vector;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.Unmarshaller;

import org.apache.commons.io.IOUtils;
import org.kalypso.commons.bind.JaxbUtilities;
import org.kalypso.repository.RepositoryException;
import org.kalypso.repository.conf.Repconf.Repository;

/**
 * Utility class for the repository config package.
 * 
 * @author schlienger
 */
public class RepositoryConfigUtils
{
  private static final JAXBContext JC = JaxbUtilities.createQuiet( ObjectFactory.class );

  private RepositoryConfigUtils( )
  {
    // not to be instanciated
  }

  /**
   * Loads the config from an <code>InputStream</code> and closes the stream once finished.
   * 
   * @param ins
   * @throws RepositoryException
   */
  public static List<RepositoryFactoryConfig> loadConfig( final URL location ) throws RepositoryException
  {
    InputStream ins = null;
    try
    {
      ins = new BufferedInputStream( location.openStream() );
      final Unmarshaller unmarshaller = JC.createUnmarshaller();

      final Repconf repconf = (Repconf) unmarshaller.unmarshal( ins );
      ins.close();

      final List<Repconf.Repository> list = repconf.getRepository();

      final List<RepositoryFactoryConfig> fConfs = new Vector<RepositoryFactoryConfig>( list.size() );

      for( final Repository elt : list )
      {
        final RepositoryFactoryConfig item = new RepositoryFactoryConfig( elt.getName(), elt.getFactory(), elt.getConf(), elt.isReadOnly(), null );
        fConfs.add( item );
      }

      return fConfs;
    }
    catch( final Exception e )
    {
      throw new RepositoryException( "Unable to load repository config from location: " + location, e );
    }
    finally
    {
      IOUtils.closeQuietly( ins );
    }
  }
}