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

import java.io.IOException;
import java.io.InputStream;
import java.util.Iterator;
import java.util.List;
import java.util.Vector;

import javax.xml.bind.JAXBException;
import javax.xml.bind.Unmarshaller;

import org.kalypso.repository.RepositoryException;

/**
 * Utility class for the repository config package.
 * 
 * @author schlienger
 */
public class RepositoryConfigUtils
{
  private RepositoryConfigUtils( )
  {
    // not to be instanciated
  }

  /**
   * Loads the config from an <code>InputStream</code> and closes the stream
   * once finished.
   * 
   * @param ins
   * @return
   * @throws RepositoryException
   */
  public static List loadConfig( final InputStream ins )
      throws RepositoryException
  {
    try
    {
      final ObjectFactory factory = new ObjectFactory();
      final Unmarshaller unmarshaller = factory.createUnmarshaller();

      final RepconfType repconf = (RepconfType) unmarshaller.unmarshal( ins );

      final List list = repconf.getRepository();

      final List fConfs = new Vector( list.size() );

      for( final Iterator it = list.iterator(); it.hasNext(); )
      {
        final RepconfType.RepositoryType elt = (RepconfType.RepositoryType) it
            .next();

        final RepositoryFactoryConfig item = new RepositoryFactoryConfig( elt
            .getName(), elt.getFactory(), elt.getConf(), elt.isReadOnly() );
        fConfs.add( item );
      }

      return fConfs;
    }
    catch( JAXBException e )
    {
      throw new RepositoryException( e );
    }
    finally
    {
      try
      {
        ins.close();
      }
      catch( IOException e )
      {
        throw new RepositoryException( e );
      }
    }
  }
}