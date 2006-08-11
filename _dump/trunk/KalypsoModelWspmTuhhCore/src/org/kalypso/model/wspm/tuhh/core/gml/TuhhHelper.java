package org.kalypso.model.wspm.tuhh.core.gml;

import java.lang.reflect.InvocationTargetException;

import javax.xml.namespace.QName;

import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.SubProgressMonitor;
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
   * TODO: just name it createTuhh-Project Ensures that the given container holds a valid wspm-tuhh modell.
   * <p>
   * If it is not the case, it creates the structure
   * </p>
   * 
   * @return the freshly created modell file
   */
  public static IFile ensureValidWspmTuhhStructure( final IContainer wspmContainer, final IProgressMonitor monitor ) throws CoreException, InvocationTargetException
  {
    monitor.beginTask( "Validierung der Modellstruktur", 1000 );

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
          monitor.worked(100);
        // else if( wspmContainer instanceof IProject )
        // {
        // monitor.subTask( "Projekt wird angelegt" );
        // final IProject project = (IProject) wspmContainer;
        // (project).create( new SubProgressMonitor( monitor, 50 ) );
        // monitor.subTask( "Projekt wird geöffnet" );
        // project.open( new SubProgressMonitor( monitor, 50 ) );
        // }
      }

      monitor.subTask( " - erzeuge Modellstruktur" );

      final IFile targetFile = wspmContainer.getFile( new Path( "modell.gml" ) );
      // Add the tuhh namespace to the known namespaces, so it is later known for adding new feature
      final String[] additionalNamespaces = new String[] { IWspmTuhhConstants.NS_WSPM_TUHH };
      GmlSerializer.createGmlFile( new QName( IWspmConstants.NS_WSPMPROJ, "WspmProject" ), additionalNamespaces, targetFile, new SubProgressMonitor( monitor, 900 ) );

      return targetFile;
    }
    finally
    {
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
