package org.kalypso.ogc.gml.loader;

import java.io.File;
import java.io.FileWriter;
import java.io.Writer;
import java.net.MalformedURLException;
import java.net.URL;

import org.deegree.model.feature.FeatureVisitor;
import org.deegree.model.feature.GMLWorkspace;
import org.deegree_impl.model.feature.visitors.ResortVisitor;
import org.deegree_impl.model.feature.visitors.TransformVisitor;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.kalypso.eclipse.core.resources.ResourceUtilities;
import org.kalypso.eclipse.util.SetContentThread;
import org.kalypso.java.net.IUrlResolver;
import org.kalypso.loader.AbstractLoader;
import org.kalypso.loader.LoaderException;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypso.util.url.UrlResolver;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * Lädt einen GMLWorkspace aus einem GML
 * 
 * @author Belger
 */
public class GmlLoader extends AbstractLoader
{
  private final IUrlResolver m_urlResolver = new UrlResolver();
  
  /**
   * @see org.kalypso.loader.AbstractLoader#loadIntern(java.lang.String, java.net.URL, org.eclipse.core.runtime.IProgressMonitor)
   */
  protected Object loadIntern( final String source, final URL context,
      final IProgressMonitor monitor ) throws LoaderException
  {
    try
    {
      monitor.beginTask( "GML laden", 1000 );

      final URL gmlURL = m_urlResolver.resolveURL( context, source );

      final IResource gmlFile = ResourceUtilities.findFileFromURL( gmlURL );
      
      final CommandableWorkspace workspace = new CommandableWorkspace( GmlSerializer.createGMLWorkspace( gmlURL, m_urlResolver ) );

      try
      {
        final CS_CoordinateSystem targetCRS = KalypsoGisPlugin.getDefault().getCoordinatesSystem();
        workspace.accept( new TransformVisitor( targetCRS ), workspace.getRootFeature(), FeatureVisitor.DEPTH_INFINITE );
        workspace.accept( new ResortVisitor(), workspace.getRootFeature(), FeatureVisitor.DEPTH_INFINITE );
      }
      catch( final Throwable e1 )
      {
        e1.printStackTrace();
      }
      
      if( gmlFile != null )
        addResource( gmlFile, workspace );

      return workspace;
    }
    catch( final LoaderException le )
    {
      le.printStackTrace();

      throw le;
    }
    catch( final Exception e )
    {
      e.printStackTrace();

      throw new LoaderException( "Konnte GML nicht laden", e );
    }
    finally
    {
      monitor.done();
    }
  }

  /**
   * @see org.kalypso.loader.ILoader#getDescription()
   */
  public String getDescription()
  {
    return "GML Layer";
  }

  /**
   * @see org.kalypso.loader.ILoader#save(java.lang.String, java.net.URL, org.eclipse.core.runtime.IProgressMonitor, java.lang.Object)
   */
  public void save( final String source, final URL context,
      final IProgressMonitor monitor, final Object data ) throws LoaderException
  {
    try
    {
      final GMLWorkspace workspace = (GMLWorkspace)data;

      final URL gmlURL = m_urlResolver.resolveURL( context, source );
  
      // ists im Workspace?
      final IFile file = ResourceUtilities.findFileFromURL( gmlURL );
      if( file != null )
      {
        final SetContentThread thread = new SetContentThread( file, !file.exists(), false, true, new NullProgressMonitor() ) 
        {
          protected void write( final Writer writer ) throws Throwable
          {
            GmlSerializer.serializeWorkspace( writer, workspace);            
          }
        };
        thread.start();
        thread.join();
        
        final CoreException fileException = thread.getFileException();
        if( fileException != null )
          throw fileException;
        
        final Throwable thrown = thread.getThrown();
        if( thrown != null )
          throw thrown;
      }
      else if( file == null && gmlURL.getProtocol().equals( "file" ) )
      {
        final Writer w = new FileWriter( new File( gmlURL.getFile() ) );
        GmlSerializer.serializeWorkspace( w, workspace);
      }
      else
        throw new LoaderException( "Die URL kann nicht beschrieben werden: " + gmlURL );
    }
    catch( final MalformedURLException e )
    {
      e.printStackTrace();
      
      throw new LoaderException( "Der angegebene Pfad ist ungültig: " + source + "\n" + e.getLocalizedMessage(), e );
    }
    catch( final Throwable e )
    {
      e.printStackTrace();
      throw new LoaderException( "Fehler beim Speichern der URL\n" + e.getLocalizedMessage(), e );
    }
  }
}