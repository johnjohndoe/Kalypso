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
  private RepositoryConfigUtils()
  {
  // not to be instanciated
  }

  /**
   * Loads the config from an <code>InputStream</code> and closes the stream once finished.
   *  
   */
  public static RepositoryConfig loadConfig( final InputStream ins ) throws RepositoryException
  {
    try
    {
      final ObjectFactory factory = new ObjectFactory();
      final Unmarshaller unmarshaller = factory.createUnmarshaller();

      final RepconfType repconf = (RepconfType)unmarshaller.unmarshal( ins );

      final List list = repconf.getRep();

      final List items = new Vector( list.size() );

      for( final Iterator it = list.iterator(); it.hasNext(); )
      {
        final RepconfType.RepType elt = (RepconfType.RepType)it.next();

        final RepositoryConfigItem item = new RepositoryConfigItem( elt.getClassName(), elt
            .getConfString(), elt.isReadOnly() );
        items.add( item );
      }

      return new RepositoryConfig( items );
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