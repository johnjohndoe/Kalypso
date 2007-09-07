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
 *  g.belger@bjoernsen.de
 *  m.schlienger@bjoernsen.de
 *  v.doemming@tuhh.de
 *   
 *  ---------------------------------------------------------------------------*/
package org.kalypso.ui.repository;

import java.io.File;
import java.io.FileOutputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.io.StringWriter;

import javax.xml.bind.JAXBException;

import org.apache.commons.io.IOUtils;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.kalypso.commons.factory.FactoryException;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.view.ObservationCache;
import org.kalypso.ogc.sensor.zml.ZmlFactory;
import org.kalypso.repository.IRepository;
import org.kalypso.repository.IRepositoryItem;
import org.kalypso.repository.RepositoryException;
import org.kalypso.zml.ObservationType;

/**
 * This class dumps a repository kompletely into the filesystem.
 * 
 * @author Holger Albert
 */
public class RepositoryDumper
{
  /**
   * The constructor.
   */
  private RepositoryDumper()
  {
  /* Nothing to do. */
  }

  /**
   * This function dumps the structure into a file in the given directory. Furthermore it create folders and places the
   * .zmls in it.
   * 
   * @param directory
   *          The choosen directory.
   * @param monitor
   *          A progress monitor.
   * @throws RepositoryException
   * @throws InterruptedException
   */
  public static void dumpExtended( File directory, IRepository root, IProgressMonitor monitor )
      throws InterruptedException, RepositoryException
  {
    StringWriter writer = null;
    FileWriter structureWriter = null;

    try
    {
      /* The content will be written to this writer. */
      writer = new StringWriter();

      /* Do the structure dump first. */
      root.dumpStructure( writer, new NullProgressMonitor() );

      /* Update monitor. */
      monitor.worked( 100 );

      /* Get the structure as string. */
      String structure = writer.toString();

      /* Close the writer. */
      writer.close();

      /* Create the structure file. */
      File structureFile = new File( directory, "structure.txt" );

      /* The writer to save the file. */
      structureWriter = new FileWriter( structureFile );
      structureWriter.write( structure );
      structureWriter.close();

      /* Update monitor. */
      monitor.worked( 100 );

      /* Do the dump into the filesystem. */
      dumpExtendedRecursive( directory, root, monitor );

      /* Update monitor. */
      monitor.worked( 800 );
    }
    catch( InterruptedException e )
    {
      throw e;
    }
    catch( Exception e )
    {
      throw new RepositoryException( e );
    }
    finally
    {
      IOUtils.closeQuietly( writer );
      IOUtils.closeQuietly( structureWriter );
    }
  }

  /**
   * Creates the dump structure in the file-system.
   * 
   * @param directory
   *          The choosen directory.
   * @param monitor
   *          A progress monitor.
   * @throws InterruptedException
   * @throws RepositoryException
   */
  private static void dumpExtendedRecursive( File directory, IRepositoryItem item, IProgressMonitor monitor )
      throws InterruptedException, RepositoryException
  {
    /* If the user cancled the operation, abort. */
    if( monitor.isCanceled() )
      throw new InterruptedException();

    FileOutputStream writer = null;

    try
    {
      /* The name will be used as filename. */
      String name = item.getName();

      /* This is the directory, where the .zml is placed. */
      File newDirectory = new File( directory, name );

      /* Create it. */
      if( !newDirectory.mkdir() )
        throw new RepositoryException( "Could not create the directory '" + newDirectory.getAbsolutePath() + "' ..." );

      IObservation observation = ObservationCache.getInstance().getObservationFor( item );
      if( observation != null )
      {
        /* Dump if neccessary. */
        // DateRange dra = ObservationViewHelper.makeDateRange( item );
        // PlainObsProvider provider = new PlainObsProvider( observation, new ObservationRequest( dra ) );
        // IObservation scaledObservation = provider.getObservation();
        writer = new FileOutputStream( new File( newDirectory, newDirectory.getName() ) );
        ObservationType observationType = ZmlFactory.createXML( observation, null );
        ZmlFactory.getMarshaller().marshal( observationType, writer );
        writer.close();
      }

      IRepositoryItem[] items = item.getChildren();
      if( items == null )
        return;

      for( int i = 0; i < items.length; i++ )
        dumpExtendedRecursive( newDirectory, items[i], monitor );
    }
    catch( IOException e )
    {
      throw new RepositoryException( e );
    }
    catch( FactoryException e )
    {
      throw new RepositoryException( e );
    }
    catch( JAXBException e )
    {
      throw new RepositoryException( e );
    }
    finally
    {
      IOUtils.closeQuietly( writer );
    }
  }
}