package org.kalypso.util.transformation;

import java.io.File;
import java.io.FileOutputStream;
import java.io.OutputStreamWriter;
import java.io.BufferedWriter;
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
import org.kalypso.eclipse.core.resources.FileUtilities;
import org.kalypso.java.lang.reflect.ClassUtilities;
import org.kalypso.java.lang.reflect.ClassUtilities.ClassUtilityException;
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
   * Creates a 'real' {@link ITransformation}from a
   * {@link TransformationType XML-TransformationType}.
   * 
   * @param trans
   * @return transformation
   * @throws TransformationException
   */
  public static ITransformation createTransformation(
      final TransformationType trans ) throws TransformationException
  {
    try
    {
      final ITransformation transformation = (ITransformation) ClassUtilities
          .newInstance( trans.getClassName(), ITransformation.class,
              TransformationHelper.class.getClassLoader() );

      final Properties props = new Properties();
      final List arguments = trans.getArgument();
      for( Iterator argIter = arguments.iterator(); argIter.hasNext(); )
      {
        final TransformationType.ArgumentType argument = (TransformationType.ArgumentType) argIter
            .next();
        props.setProperty( argument.getName(), argument.getValue() );
      }

      transformation.setProperties( props );

      return transformation;
    }
    catch( final ClassUtilityException e )
    {
      throw new TransformationException( "Could not create Transformation: "
          + trans.getClassName(), e );
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
  public static TransformationResult doTranformations( final IFolder folder,
      final TransformationList trans, final IProgressMonitor monitor )
      throws TransformationException
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

      logWriter = new BufferedWriter( new OutputStreamWriter(
          new FileOutputStream( tmpFile ), charset ) );

      monitor.beginTask( "Transformationen durchführen", transList.size() + 1 );

      for( final Iterator iter = transList.iterator(); iter.hasNext(); )
      {
        final TransformationType element = (TransformationType) iter.next();
        final ITransformation ccTrans = TransformationHelper
            .createTransformation( element );

        // perform transformation using:
        // - msgWriter: a short list of messages that sum up the problems
        // - logWriter: a complete description of the problem
        ccTrans.transform( bufWriter, logWriter, new SubProgressMonitor(
            monitor, 1 ) );

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
      FileUtilities.copyFile( tmpFile, logFile, new SubProgressMonitor(
          monitor, 1 ) );
    }
    catch( CoreException e )
    {
      e.printStackTrace();
    }

    // now delete temp file
    //tmpFile.delete();

    // now handle the case where operation was successfull, but some messages
    // were
    // thrown for user.
    final String msg = msgWriter.toString();
    if( msg.length() > 0 )
      return new TransformationResult( msg, logFile );
    else
      return TransformationResult.OK_RESULT;
  }
}