package org.kalypso.loader.impl;

import java.io.IOException;
import java.io.OutputStreamWriter;
import java.io.PipedInputStream;
import java.io.PipedOutputStream;
import java.util.Properties;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.IProgressMonitor;
import org.kalypso.eclipse.core.runtime.UtilProgressMonitor;
import org.kalypso.loader.AbstractLoader;
import org.kalypso.loader.LoaderException;
import org.kalypso.ogc.gml.KalypsoFeatureLayer;
import org.kalypso.ogc.gml.serialize.GmlSerializeException;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.plugin.KalypsoGisPlugin;
import org.xml.sax.InputSource;

/**
 * @author schlienger
 *  
 */
public final class GMLArrayLoader extends AbstractLoader
{
  /**
   * @see org.kalypso.loader.ILoader#getDescription()
   */
  public String getDescription()
  {
    return "ESRI Shape";
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
  
      final InputSource schemaSource = new InputSource( schemaFile.getContents() );
      schemaSource.setEncoding( schemaFile.getCharset() );
      
  
      final String gmlPath = source.getProperty( "PATH", "" );
      final IFile gmlFile = project.getFile( gmlPath );
      final InputSource gmlSource = new InputSource( gmlFile.getContents() );
      gmlSource.setEncoding( gmlFile.getCharset() );
  
      return GmlSerializer.deserialize(schemaSource, gmlSource, KalypsoGisPlugin.getDefault().getCoordinatesSystem(), new UtilProgressMonitor( monitor )  );
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
            final OutputStreamWriter osw = new OutputStreamWriter( pos );
             GmlSerializer.serialize( osw, layers, new UtilProgressMonitor( monitor ) );
          }
          catch( GmlSerializeException e )
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