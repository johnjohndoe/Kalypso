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
import java.io.File;
import java.io.FileOutputStream;
import java.io.OutputStreamWriter;
import java.io.StringWriter;
import java.util.Iterator;
import java.util.List;
import java.util.Properties;

import org.apache.commons.io.IOUtils;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.kalypso.contribs.java.lang.reflect.ClassUtilities;
import org.kalypso.contribs.java.lang.reflect.ClassUtilityException;
import org.kalypso.eclipse.core.resources.FileUtilities;
import org.kalypso.eclipse.core.runtime.LogStatusWrapper;
import org.kalypso.model.xml.TransformationList;
import org.kalypso.model.xml.TransformationType;

/**
 * Static Helper class for transformations
 * 
 * @author belger
 */
public class TransformationHelper
{
  /**
   * Creates a 'real' {@link ITransformation}from a {@link TransformationType XML-TransformationType}.
   * 
   * @param trans
   * @return transformation
   * @throws TransformationException
   */
  public static ITransformation createTransformation( final TransformationType trans ) throws TransformationException
  {
    try
    {
      final ITransformation transformation = (ITransformation)ClassUtilities.newInstance( trans.getClassName(),
          ITransformation.class, TransformationHelper.class.getClassLoader() );

      final Properties props = new Properties();
      final List arguments = trans.getArgument();
      for( Iterator argIter = arguments.iterator(); argIter.hasNext(); )
      {
        final TransformationType.ArgumentType argument = (TransformationType.ArgumentType)argIter.next();
        props.setProperty( argument.getName(), argument.getValue() );
      }

      transformation.setProperties( props );

      return transformation;
    }
    catch( final ClassUtilityException e )
    {
      throw new TransformationException( "Could not create Transformation: " + trans.getClassName(), e );
    }
  }

  /**
   * Führt alle Transformationen einer Transformationen Liste aus
   * 
   * @param folder
   * @param trans
   * @param monitor
   * @return result of transformation
   * @throws TransformationException
   */
  public static LogStatusWrapper doTranformations( final IFolder folder, final TransformationList trans,
      final IProgressMonitor monitor ) throws TransformationException
  {
    final List transList = trans.getTransformation();

    final String logFileName = trans.getLogFile();

    final IFile logFile = folder.getFile( logFileName );

    String charset = null;
    try
    {
      charset = logFile.getCharset( true );
    }
    catch( CoreException e )
    {
      // ignored
    }

    if( charset == null )
      charset = "utf-8";

    final StringWriter msgWriter = new StringWriter();
    final BufferedWriter bufWriter = new BufferedWriter( msgWriter );
    BufferedWriter logWriter = null;
    File tmpFile = null;

    try
    {
      // create a temp file that will contain the logs (this file will be copied
      // to the real log file)
      tmpFile = File.createTempFile( logFileName, ".log" );

      logWriter = new BufferedWriter( new OutputStreamWriter( new FileOutputStream( tmpFile ), charset ) );

      monitor.beginTask( "Transformationen durchführen", transList.size() + 1 );

      for( final Iterator iter = transList.iterator(); iter.hasNext(); )
      {
        final TransformationType element = (TransformationType)iter.next();
        final ITransformation ccTrans = TransformationHelper.createTransformation( element );

        // perform transformation using:
        // - msgWriter: a short list of messages that sum up the problems
        // - logWriter: a complete description of the problem
        ccTrans.transform( bufWriter, logWriter, new SubProgressMonitor( monitor, 1 ) );

        monitor.worked( 1 );
      }
    }
    catch( Exception e )
    {
      e.printStackTrace();

      throw new TransformationException( e );
    }
    finally
    {
      IOUtils.closeQuietly( bufWriter );
      IOUtils.closeQuietly( logWriter );

      monitor.done();
    }

    // copy temp file into real log file
    try
    {
      FileUtilities.copyFile( charset, tmpFile, logFile, new SubProgressMonitor( monitor, 1 ) );
    }
    catch( CoreException e )
    {
      e.printStackTrace();
    }

    // now delete temp file
    tmpFile.delete();

    // now handle the case where operation was successfull, but some messages
    // were thrown for user.
    final String msg = msgWriter.toString();
    if( msg.length() > 0 )
      return new LogStatusWrapper( msg, logFile );

    return LogStatusWrapper.OK_RESULT;
  }
}