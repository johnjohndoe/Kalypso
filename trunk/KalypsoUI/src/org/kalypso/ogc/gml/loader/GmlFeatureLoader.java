package org.kalypso.ogc.gml.loader;

import java.io.IOException;
import java.io.OutputStreamWriter;
import java.io.PipedInputStream;
import java.io.PipedOutputStream;
import java.io.UnsupportedEncodingException;
import java.net.URL;
import java.util.Properties;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.kalypso.loader.AbstractLoader;
import org.kalypso.loader.LoaderException;
import org.kalypso.ogc.gml.KalypsoFeatureLayer;
import org.kalypso.ogc.gml.serialize.GmlSerializeException;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.ui.KalypsoGisPlugin;

/**
 * @author schlienger
 *  
 */
public final class GmlFeatureLoader extends AbstractLoader
{
  /**
   * @see org.kalypso.loader.ILoader#getDescription()
   */
  public String getDescription()
  {
    return "ESRI Shape";
  }

  /**
   * @see org.kalypso.loader.AbstractLoader#loadIntern(java.util.Properties, java.net.URL, org.eclipse.core.runtime.IProgressMonitor)
   */
  protected Object loadIntern( Properties source, URL context, IProgressMonitor monitor ) throws LoaderException
  {
    // TODO: currently unsupported, remove deprecated one and implement this one
    throw new UnsupportedOperationException();
  }


  /**
   * @see org.kalypso.loader.AbstractLoader#loadIntern(java.util.Properties,
   *      org.eclipse.core.resources.IProject,
   *      org.eclipse.core.runtime.IProgressMonitor)
   */
  protected final Object loadIntern( final Properties source, final IProject project,
      final IProgressMonitor monitor ) throws LoaderException
  {
    try
    {
      final String schemaPath = source.getProperty( "XSD", "" );
      final IFile schemaFile = project.getFile( schemaPath );
  
      // use toString because URL need / instead of \ 
      final URL schemaURL = new URL( "platform:/resource/" + schemaFile.getFullPath().toString() );
      
      final String gmlPath = source.getProperty( "PATH", "" );
      final IFile gmlFile = project.getFile( gmlPath );
      final URL gmlURL = new URL( "platform:/resource/" + gmlFile.getFullPath().toString() );
  
      final KalypsoFeatureLayer[] layers = GmlSerializer.deserialize( schemaURL, gmlURL, KalypsoGisPlugin.getDefault().getCoordinatesSystem(), monitor );
      
      addResource( gmlFile, layers );
      addResource( schemaFile, layers );
      
      return layers;
    }
    catch( final Exception e )
    {
      throw new LoaderException( "Konnte GML nicht laden", e );
    }
  }

  /**
   * @see org.kalypso.loader.AbstractLoader#save(java.util.Properties,
   *      org.eclipse.core.resources.IProject,
   *      org.eclipse.core.runtime.IProgressMonitor, java.lang.Object)
   * 
   * TODO: besseres error-handling: am besten IStatus zur?ckgeben lassen
   */
  public void save( final Properties source, final IProject project,
      final IProgressMonitor monitor, final Object data ) throws LoaderException
  {
    final KalypsoFeatureLayer[] layers = (KalypsoFeatureLayer[])data;

    try
    {
      final String sourcePath = source.getProperty( "PATH", "" );
      final IFile file = project.getFile( sourcePath );

      final PipedOutputStream pos = new PipedOutputStream();
      final PipedInputStream pis = new PipedInputStream( pos );
      
      final Runnable runnable = new Runnable()
      {
        public void run()
        {
          try
          {
            final OutputStreamWriter osw = new OutputStreamWriter( pos, file.getCharset() );

             GmlSerializer.serialize( osw, layers, monitor );
          }
          catch( final GmlSerializeException e )
          {
            e.printStackTrace();
          }
          catch( final UnsupportedEncodingException e )
          {
            e.printStackTrace();
          }
          catch( CoreException e )
          {
            e.printStackTrace();
          }
          finally
          {
            try
            {
              pos.close();
            }
            catch( IOException e1 )
            {
              System.out.println("error in resource"+file.getLocation().toString());
              e1.printStackTrace();
            }
          }
        }
      };
      
      final Thread thread = new Thread( runnable, "GML Save Thread" );
      thread.start();

      file.setContents( pis, false, true, monitor );
      pis.close();      

    }
    catch( final Exception e )
    {
      throw new LoaderException( "Fehler beim Schriben der GML-Datei", e );
    }
  }
}