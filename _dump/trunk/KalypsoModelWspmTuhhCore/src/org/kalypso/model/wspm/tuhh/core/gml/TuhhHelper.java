package org.kalypso.model.wspm.tuhh.core.gml;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.lang.reflect.InvocationTargetException;

import javax.xml.namespace.QName;

import org.apache.commons.io.IOUtils;
import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.kalypso.commons.java.util.zip.ZipUtilities;
import org.kalypso.gmlschema.GMLSchemaException;
import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypso.model.wspm.core.gml.WspmWaterBody;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;

public class TuhhHelper implements IWspmConstants, IWspmTuhhConstants
{
  private TuhhHelper( )
  {
    // never instantiate
  }

  /**
   * TODO: just name it createTuhh-Project
   * <p>
   * Ensures that the given container holds a valid wspm-tuhh modell.
   * </p>
   * <p>
   * If it is not the case, it creates the structure
   * </p>
   * 
   * @return the freshly created modell file
   */
  public static IFile ensureValidWspmTuhhStructure( final IContainer wspmContainer, final IProgressMonitor monitor ) throws CoreException, InvocationTargetException
  {
    monitor.beginTask( "Validierung der Modellstruktur", 2000 );

    InputStream zipInputStream = null;
    try
    {
      if( !wspmContainer.exists() )
      {
        if( wspmContainer instanceof IFolder )
        {
          monitor.subTask( " - erzeuge Verzeichnis " + wspmContainer.getName() );
          ((IFolder) wspmContainer).create( false, true, new SubProgressMonitor( monitor, 100 ) );
        }
        else
          monitor.worked( 100 );
        // else if( wspmContainer instanceof IProject )
        // {
        // monitor.subTask( "Projekt wird angelegt" );
        // final IProject project = (IProject) wspmContainer;
        // (project).create( new SubProgressMonitor( monitor, 50 ) );
        // monitor.subTask( "Projekt wird geöffnet" );
        // project.open( new SubProgressMonitor( monitor, 50 ) );
        // }
      }

      monitor.subTask( " - erzeuge Datenmodell" );

      final IFile targetFile = wspmContainer.getFile( new Path( "modell.gml" ) );
      // Add the tuhh namespace to the known namespaces, so it is later known for adding new feature
      final String[] additionalNamespaces = new String[] { IWspmTuhhConstants.NS_WSPM_TUHH };
      GmlSerializer.createGmlFile( new QName( IWspmConstants.NS_WSPMPROJ, "WspmProject" ), additionalNamespaces, targetFile, new SubProgressMonitor( monitor, 900 ) );

      monitor.subTask( " - kopiere Modellstruktur" );
      final File containerDir = wspmContainer.getLocation().toFile();

      zipInputStream = TuhhHelper.class.getResourceAsStream( "resources/wspmTuhh_template.zip" );
      ZipUtilities.unzip( zipInputStream, containerDir, false );
      monitor.worked( 500 );
      wspmContainer.refreshLocal( IResource.DEPTH_INFINITE, new SubProgressMonitor( monitor, 500 ) );

      return targetFile;
    }
    catch( final IOException e )
    {
      throw new InvocationTargetException( e );
    }
    finally
    {
      IOUtils.closeQuietly( zipInputStream );
      monitor.done();
    }

  }

  /** TODO: shouldn't this belong to one of the feature wrapper classes? e.g. WaterBody? */
  public static TuhhReach createNewReachForWaterBody( final WspmWaterBody waterBody ) throws GMLSchemaException
  {
    final Feature newTuhhReach = FeatureHelper.addFeature( waterBody.getFeature(), new QName( NS_WSPM, "reachMember" ), new QName( NS_WSPM_TUHH, "ReachWspmTuhhSteadyState" ) );

    final TuhhReach tuhhReach = new TuhhReach( newTuhhReach );

    tuhhReach.setWaterBody( waterBody );

    return tuhhReach;
  }

}
