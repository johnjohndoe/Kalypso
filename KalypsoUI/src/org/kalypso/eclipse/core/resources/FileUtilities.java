package org.kalypso.eclipse.core.resources;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.io.Writer;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.kalypso.eclipse.util.SetContentHelper;

/**
 * FileUtilities
 * 
 * @author schlienger
 */
public class FileUtilities
{
  private FileUtilities( )
  {
    // not intended to be instanciated
  }

  /**
   * Sets the contents of the dest file using the source file.
   * 
   * @param sourceCharset
   * @param source
   * @param dest
   * @param monitor
   * @throws CoreException
   */
  public static void copyFile( final String sourceCharset, final File source,
      final IFile dest, final IProgressMonitor monitor ) throws CoreException
  {
    final SetContentHelper helper = new SetContentHelper()
    {
      protected void write( final Writer writer ) throws Throwable
      {
        final PrintWriter pwr = new PrintWriter( writer );
        final BufferedReader reader = new BufferedReader(
            new InputStreamReader( new FileInputStream( source ), sourceCharset ) );

        try
        {
          String strLine = reader.readLine();
          while( strLine != null )
          {
            pwr.println( strLine );

            strLine = reader.readLine();
          }
        }
        finally
        {
          reader.close();
          pwr.close();
        }
      }
    };

    helper.setFileContents( dest, false, false, monitor );
  }
}