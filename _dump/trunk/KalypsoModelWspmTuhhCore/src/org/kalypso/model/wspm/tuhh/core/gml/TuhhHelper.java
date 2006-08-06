package org.kalypso.model.wspm.tuhh.core.gml;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;

import javax.xml.namespace.QName;

import org.apache.commons.io.IOUtils;
import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.kalypso.commons.java.util.zip.ZipUtilities;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.gmlschema.GMLSchemaException;
import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypso.model.wspm.core.gml.WspmWaterBody;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;

public class TuhhHelper implements IWspmConstants, IWspmTuhhConstants
{
  private TuhhHelper( )
  {
    // never instantiate
  }

  /**
   * Ensures that the given container holds a valid wspm-tuhh modell.
   * <p>
   * If it is not the case, it creates the structure
   * </p>
   */
  public static void ensureValidWspmTuhhStructure( final IContainer wspmContainer, final IProgressMonitor monitor ) throws CoreException
  {
    monitor.beginTask( "Validiere Modellstruktur", 1000 );

    InputStream zipInputStream = null;
    try
    {
      if( !wspmContainer.exists() )
      {
        if( wspmContainer instanceof IFolder )
        {
          monitor.subTask( "Verzeichnis wird erzeugt" );
          ((IFolder) wspmContainer).create( false, true, new SubProgressMonitor( monitor, 100 ) );
        }
        else if( wspmContainer instanceof IProject )
        {
          monitor.subTask( "Projekt wird angelegt" );
          final IProject project = (IProject) wspmContainer;
          (project).create( new SubProgressMonitor( monitor, 50 ) );
          monitor.subTask( "Projekt wird geöffnet" );
          project.open( new SubProgressMonitor( monitor, 50 ) );
        }
      }

      monitor.subTask( "Validiere Modellstruktur" );
      final File containerDir = wspmContainer.getLocation().toFile();
      zipInputStream = TuhhHelper.class.getResourceAsStream( "resources/wspmTuhh_template.zip" );
      ZipUtilities.unzip( zipInputStream, containerDir, false );
      monitor.worked( 650 );
      wspmContainer.refreshLocal( IResource.DEPTH_INFINITE, new SubProgressMonitor( monitor, 250 ) );

      // TODO: check if model.gml is valid xml with right namespace
    }
    catch( final IOException e )
    {
      throw new CoreException( StatusUtilities.statusFromThrowable( e ) );
    }
    finally
    {
      IOUtils.closeQuietly( zipInputStream );
      monitor.done();
    }

  }

  public static TuhhReach createNewReachForWaterBody( final WspmWaterBody waterBody ) throws GMLSchemaException
  {
    final Feature newTuhhReach = FeatureHelper.addFeature( waterBody.getFeature(), new QName( NS_WSPM, "reachMember" ), new QName( NS_WSPM_TUHH, "ReachWspmTuhhSteadyState" ) );

    final TuhhReach tuhhReach = new TuhhReach( newTuhhReach );

    tuhhReach.setWaterBody( waterBody );

    return tuhhReach;
  }

}
